## 02. Build the universe of possessions
##
## From the raw dumps (scripts/01_download_data.R) to one row per possession that starts
## after an opponent's made field goal, with:
##   delta_s          seconds of game clock to the first action (inbound included)
##   length_s         possession length (robustness)
##   dead_ball_first  dead ball before the first action
##   net_points       points minus the value of the opponent's next possession,
##                    with values estimated within the same season
##   excluded         last 2:00 of Q4/OT, last 54 s of Q1-Q3, garbage time
##
## Output: data/processed/universe.rds, data/processed/next_possession_values.rds
##
## Open the RProject (transition-after-makes.Rproj) before running this script.

# Packages ----------------------------------------------------------------

library(here)
library(tidyverse)
source(here("R", "possessions.R"))


# 1. Data -----------------------------------------------------------------

seasons <- c(2023, 2024)

read_raw <- function(dataset, season) {
  read_csv(here("data", "raw", paste0(dataset, "_", season, ".csv")),
           col_types = cols(.default = "c"))
}

poss <- map(seasons, function(s) prepare_possessions(read_raw("pbpstats", s), season = s))
events <- map(seasons, function(s) read_raw("nbastats", s))


# 2. First action of each possession ---------------------------------------

first <- map2(poss, events, first_actions)


# 3. Universe -------------------------------------------------------------

universe_list <- map2(poss, first, build_universe)

# How many possessions after a made field goal were dropped, per season
tibble(
  season            = map_chr(poss, ~ unique(.x$season)),
  no_first_action   = map_int(universe_list, ~ attr(.x, "n_no_first_action")),
  negative_delta    = map_int(universe_list, ~ attr(.x, "n_negative_delta")),
  kept              = map_int(universe_list, nrow)
)


# 4. Net points -------------------------------------------------------------
# Value of each start type, from all possessions of the same season

values <- map_dfr(poss, next_possession_value)

values %>%
  filter(start_type %in% c(made_fg_start_types, "Off Steal", "Off Dead Ball", "End of period")) %>%
  pivot_wider(names_from = season, values_from = value)

universe <- universe_list %>%
  bind_rows() %>%
  add_net_points(values) %>%
  flag_exclusions()


# 5. Exclusions ---------------------------------------------------------------

universe %>%
  group_by(season) %>%
  summarise(
    possessions   = n(),
    last_2_min    = sum(excl_last2min),
    end_quarter   = sum(excl_end_quarter),
    garbage_time  = sum(excl_garbage),
    n_excluded    = sum(excluded),
    analysed      = sum(!excluded),
    no_net_points = sum(is.na(net_points) & !excluded)
  )


# 6. Time to first action after exclusions ----------------------------------

universe %>%
  filter(!excluded) %>%
  ggplot(aes(delta_s)) +
  geom_histogram(binwidth = 1, boundary = -0.5, fill = "grey60", colour = "white") +
  facet_wrap(~season) +
  labs(x = "Seconds to first action after the opponent's made field goal",
       y = "Possessions") +
  theme_classic(base_size = 13)


# 7. Save -------------------------------------------------------------------

dir.create(here("data", "processed"), showWarnings = FALSE)
write_rds(universe, here("data", "processed", "universe.rds"))
write_rds(values, here("data", "processed", "next_possession_values.rds"))
