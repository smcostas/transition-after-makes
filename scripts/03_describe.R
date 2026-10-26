## 03. Team-game summaries and descriptive plots
##
## Input:  data/processed/universe.rds (scripts/02_build_universe.R), data/raw/nbastats_*.csv
## Output: data/processed/team_games*.rds (all, close, q1), team_season_results.rds
##
## Nothing here is a model. These are raw descriptive views of the data the model will use,
## to check that they make basketball sense before fitting anything.
##
## Open the RProject (transition-after-makes.Rproj) before running this script.

# Packages ----------------------------------------------------------------

library(here)
library(tidyverse)
library(ggrepel)
source(here("R", "possessions.R"))
source(here("R", "aggregate.R"))


# 1. Data -----------------------------------------------------------------

universe <- read_rds(here("data", "processed", "universe.rds"))

events <- map(c(2023, 2024), function(s) {
  read_csv(here("data", "raw", paste0("nbastats_", s, ".csv")), col_types = cols(.default = "c"))
})

results <- map2_dfr(events, c(2023, 2024), team_season_results)
homes <- map_dfr(events, home_teams)

# Team-game summaries for the three game-state analyses, with home court
add_home <- function(tg) {
  tg %>%
    left_join(homes, by = "game_id") %>%
    mutate(home = as.integer(team == home)) %>%
    select(-away)
}
team_games <- summarise_team_games(universe) %>% add_home()
team_games_close <- summarise_team_games(universe, subset = "close") %>% add_home()
team_games_q1 <- summarise_team_games(universe, subset = "q1") %>% add_home()

# Every team-game should have a home flag
sum(is.na(team_games$home))

# Sanity check: games per team-season (82 expected, a few games are missing in the dump)
summary(results$games)

team_seasons <- team_games %>%
  group_by(season, team, team_season) %>%
  summarise(
    games      = n(),
    poss_pg    = mean(n_poss),
    mean_delta = weighted.mean(mean_delta, n_poss),
    net_per100 = weighted.mean(net_per100, n_poss),
    .groups = "drop"
  ) %>%
  left_join(results %>% select(season, team, win_pct, point_diff_pg), by = c("season", "team"))

team_seasons %>% arrange(season, mean_delta)


# 2. Pace after makes by team-season -----------------------------------------
# Lower = faster to the first action after the opponent's made field goal

team_seasons %>%
  mutate(team = reorder(team, -mean_delta)) %>%
  ggplot(aes(mean_delta, team)) +
  geom_point(size = 2.5) +
  facet_wrap(~season, scales = "free_y") +
  labs(x = "Mean seconds to first action after an opponent's made field goal",
       y = NULL,
       title = "Pace after makes, by team-season") +
  theme_classic(base_size = 12)


# 3. Pace after makes vs net points, raw -------------------------------------
# Team-season averages. Raw association, no adjustment: not an estimate of anything.

team_seasons %>%
  ggplot(aes(mean_delta, net_per100)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_point(size = 2.5, colour = "grey30") +
  geom_text_repel(aes(label = team), size = 3, colour = "grey30") +
  facet_wrap(~season) +
  scale_x_reverse() +
  labs(x = "Mean seconds to first action (faster to the right)",
       y = "Net points per 100 possessions after makes",
       title = "Raw team-season averages") +
  theme_classic(base_size = 12)


# 4. Net value of a possession by seconds to first action ---------------------
# Descriptive only. Short possessions are selected: a team shoots early when an open shot
# appears, so this curve overstates the value of playing fast. It shows where value changes
# with time, not what pushing is worth.

universe %>%
  filter(!excluded, !is.na(net_points), delta_s <= 24) %>%
  group_by(season, delta_s) %>%
  summarise(
    n   = n(),
    net = mean(net_points),
    se  = sd(net_points) / sqrt(n()),
    .groups = "drop"
  ) %>%
  ggplot(aes(delta_s, 100 * net)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_pointrange(aes(ymin = 100 * (net - 2 * se), ymax = 100 * (net + 2 * se)),
                  size = 0.3) +
  facet_wrap(~season) +
  labs(x = "Seconds to first action after an opponent's made field goal",
       y = "Net points per 100 possessions (±2 SE)",
       title = "Value by time to first action (descriptive, self-selected)") +
  theme_classic(base_size = 12)


# 5. League check -------------------------------------------------------------
# Raw points per possession after an opponent's made field goal, all possessions analysed.
# For reference, Inpredictable reports about 1.10 for 2025-26 (another season: not comparable).

universe %>%
  filter(!excluded, !is.na(points)) %>%
  group_by(season) %>%
  summarise(possessions = n(), points_per_poss = mean(points), net_per100 = 100 * mean(net_points,
                                                                                       na.rm = TRUE))


# 6. Save -------------------------------------------------------------------

write_rds(team_games, here("data", "processed", "team_games.rds"))
write_rds(team_games_close, here("data", "processed", "team_games_close.rds"))
write_rds(team_games_q1, here("data", "processed", "team_games_q1.rds"))
write_rds(results, here("data", "processed", "team_season_results.rds"))

# Overall pace (possessions per game), descriptive colour for the report
pace <- map_dfr(c(2023, 2024), function(s) {
  read_csv(here("data", "raw", paste0("pbpstats_", s, ".csv")), col_types = cols(.default = "c")) %>%
    team_season_pace(season = s)
})
write_rds(pace, here("data", "processed", "team_season_pace.rds"))
