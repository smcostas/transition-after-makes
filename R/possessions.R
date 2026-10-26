# Possession-level data: from the raw pbpstats and nbastats dumps to the universe of
# possessions after an opponent's made field goal, with time to first action, exclusions and
# net points. See report/formulation.qmd for the definitions.

library(dplyr)

# Start types (pbpstats) of possessions that begin after the opponent's made field goal.
made_fg_start_types <- c(
  "Off At Rim Make", "Off Short Mid-Range Make", "Off Long Mid-Range Make",
  "Off Arc 3 Make", "Off Corner 3 Make"
)

# Seconds left in the period from "MM:SS" (pbpstats) or "M:SS" (nbastats).
clock_to_seconds <- function(x) {
  parts <- strsplit(x, ":", fixed = TRUE)
  vapply(parts, function(p) as.numeric(p[1]) * 60 + as.numeric(p[2]), numeric(1))
}

season_label <- function(season) {
  paste0(season, "-", substr(season + 1, 3, 4))
}

# One row per possession. The raw dump has one row per event and no offense column: the
# offense is the other team of the game. Points come from the change in score differential
# (offense's point of view) between a possession and the next one.
prepare_possessions <- function(raw, season) {
  poss <- raw %>%
    distinct(GAMEID, PERIOD, STARTTIME, ENDTIME, OPPONENT, STARTTYPE, STARTSCOREDIFFERENTIAL) %>%
    transmute(
      season       = season_label(season),
      game_id      = as.numeric(GAMEID),
      period       = as.integer(PERIOD),
      opponent     = OPPONENT,
      start_type   = STARTTYPE,
      start_s      = clock_to_seconds(STARTTIME),
      end_s        = clock_to_seconds(ENDTIME),
      start_margin = as.numeric(STARTSCOREDIFFERENTIAL)
    )

  teams <- poss %>%
    distinct(game_id, opponent) %>%
    group_by(game_id) %>%
    mutate(team = if (n() == 2) rev(opponent) else NA_character_) %>%
    ungroup()

  poss %>%
    left_join(teams, by = c("game_id", "opponent")) %>%
    arrange(game_id, period, desc(start_s), desc(end_s)) %>%
    group_by(game_id) %>%
    mutate(
      next_team   = lead(team),
      next_period = lead(period),
      next_margin = lead(start_margin),
      next_type   = lead(start_type)
    ) %>%
    ungroup() %>%
    mutate(
      points = case_when(
        is.na(next_team) ~ NA_real_,
        next_team != team ~ -next_margin - start_margin,
        TRUE ~ next_margin - start_margin
      ),
      next_opp_start_type = case_when(
        is.na(next_team) ~ NA_character_,
        next_period != period ~ "End of period",
        next_team != team ~ next_type,
        TRUE ~ NA_character_
      ),
      poss_id = row_number()
    ) %>%
    select(poss_id, season, game_id, period, team, opponent, start_type, start_s, end_s,
           start_margin, points, next_opp_start_type)
}

# Time to the first possession-defining action: a field-goal attempt or turnover by the
# offense, or a shooting foul by the defense, inside the possession's clock window. A dead
# ball (non-shooting foul, violation, substitution, timeout, jump ball, replay) logged before
# that action is flagged. Returns only possessions where a first action was found.
first_actions <- function(poss, events) {
  ev <- events %>%
    transmute(
      game_id  = as.numeric(GAME_ID),
      period   = as.integer(PERIOD),
      eventnum = as.numeric(EVENTNUM),
      type     = as.numeric(EVENTMSGTYPE),
      action   = as.numeric(EVENTMSGACTIONTYPE),
      t_s      = clock_to_seconds(PCTIMESTRING),
      ev_team  = PLAYER1_TEAM_ABBREVIATION
    )
  shooting_foul <- c(2, 29)

  in_window <- poss %>%
    select(poss_id, game_id, period, team, opponent, start_s, end_s) %>%
    inner_join(ev, by = c("game_id", "period"), relationship = "many-to-many") %>%
    filter(t_s <= start_s, t_s >= end_s)

  first <- in_window %>%
    filter(
      (type %in% c(1, 2) & ev_team == team) |
        (type == 5 & (ev_team == team | is.na(ev_team))) |
        (type == 6 & action %in% shooting_foul & ev_team == opponent)
    ) %>%
    group_by(poss_id) %>%
    slice_min(eventnum, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(poss_id, first_eventnum = eventnum, delta_s = start_s - t_s)

  dead <- in_window %>%
    inner_join(first, by = "poss_id") %>%
    filter(
      eventnum < first_eventnum,
      type %in% c(6, 7, 8, 9, 10, 18),
      !(type == 6 & action %in% shooting_foul)
    ) %>%
    distinct(poss_id) %>%
    mutate(dead_ball_first = TRUE)

  first %>%
    left_join(dead, by = "poss_id") %>%
    transmute(poss_id, delta_s, dead_ball_first = coalesce(dead_ball_first, FALSE))
}

# Possessions after an opponent's made field goal with a valid first action. Possessions
# without a first action, or with a negative delta (logging errors), are dropped and counted
# in the attributes n_no_first_action and n_negative_delta.
build_universe <- function(poss, first) {
  u <- poss %>%
    filter(start_type %in% made_fg_start_types) %>%
    left_join(first, by = "poss_id") %>%
    mutate(length_s = start_s - end_s)
  n_no_first <- sum(is.na(u$delta_s))
  n_negative <- sum(u$delta_s < 0, na.rm = TRUE)
  u <- u %>% filter(!is.na(delta_s), delta_s >= 0)
  attr(u, "n_no_first_action") <- n_no_first
  attr(u, "n_negative_delta") <- n_negative
  u
}

# Exclusions: last 2:00 of the 4th quarter and of every overtime (clock stops after made
# baskets), the last 54 s of quarters 1-3 (2-for-1 and last-shot clock management drive the
# pace there), and garbage time (Cleaning the Glass margin rule, 4th quarter only, without the
# starters condition).
flag_exclusions <- function(df) {
  margin <- abs(df$start_margin)
  df %>%
    mutate(
      excl_last2min = period >= 4 & start_s <= 120,
      excl_end_quarter = period <= 3 & start_s <= 54,
      excl_garbage  = period == 4 & (
        (start_s >= 540 & margin >= 25) |
          (start_s >= 360 & start_s < 540 & margin >= 20) |
          (start_s < 360 & margin >= 10)
      ),
      excluded = excl_last2min | excl_end_quarter | excl_garbage
    )
}

# Fast break in the FIBA sense. Descriptive only: intention is measured with delta_s itself.
is_transition <- function(delta_s, dead_ball_first, cutoff = 8) {
  delta_s <= cutoff & !dead_ball_first
}

# League-average points per possession by start type, separately for each season, over the
# same possessions the analysis keeps (exclusions applied). A possession that ends a period
# hands the opponent nothing, so "End of period" is worth 0.
next_possession_value <- function(poss) {
  poss %>%
    flag_exclusions() %>%
    filter(!excluded) %>%
    group_by(season, start_type) %>%
    summarise(value = mean(points, na.rm = TRUE), .groups = "drop") %>%
    bind_rows(tibble(season = unique(poss$season), start_type = "End of period", value = 0))
}

add_net_points <- function(universe, values) {
  universe %>%
    left_join(values, by = c("season", "next_opp_start_type" = "start_type")) %>%
    mutate(net_points = points - value) %>%
    select(-value)
}
