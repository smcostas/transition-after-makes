# Team-game summaries of the universe and team-season results.

library(dplyr)

# One row per team-game: the observed pace after makes (mean_delta, with its spread for the
# measurement model) and net points per 100 possessions after an opponent's made field goal.
#   subset         "all", "close" (|margin at the start of the possession| <= close_margin,
#                  quarters 1-3 only: close 4th quarters are played slower by design)
#                  or "q1" (first quarter only)
#   drop_dead_ball drop possessions with a dead ball before the first action (robustness)
#   intent         "first_action" (delta_s, main) or "possession_length" (length_s, robustness)
summarise_team_games <- function(universe,
                                 subset = c("all", "close", "q1"),
                                 close_margin = 5,
                                 drop_dead_ball = FALSE,
                                 intent = c("first_action", "possession_length")) {
  subset <- match.arg(subset)
  intent <- match.arg(intent)

  u <- universe %>% filter(!excluded, !is.na(net_points))
  if (subset == "close") u <- u %>% filter(abs(start_margin) <= close_margin, period <= 3)
  if (subset == "q1") u <- u %>% filter(period == 1)
  if (drop_dead_ball) u <- u %>% filter(!dead_ball_first)
  u$pace <- if (intent == "first_action") u$delta_s else u$length_s

  u %>%
    group_by(season, game_id, team, opponent) %>%
    summarise(
      n_poss           = n(),
      mean_delta       = mean(pace),
      sd_delta         = sd(pace),
      median_delta     = median(pace),
      net_per100       = 100 * mean(net_points),
      share_fast_break = mean(is_transition(delta_s, dead_ball_first)),
      .groups = "drop"
    ) %>%
    mutate(
      team_season = paste(team, season),
      opp_season  = paste(opponent, season)
    )
}

# Home and away team of each game: the team most often credited in the home (visitor)
# descriptions of the nbastats events.
home_teams <- function(events) {
  ev <- events %>% mutate(game_id = as.numeric(GAME_ID))
  side_team <- function(description_col) {
    ev %>%
      filter(!is.na(.data[[description_col]]), !is.na(PLAYER1_TEAM_ABBREVIATION)) %>%
      count(game_id, team = PLAYER1_TEAM_ABBREVIATION) %>%
      group_by(game_id) %>%
      slice_max(n, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(game_id, team)
  }
  side_team("HOMEDESCRIPTION") %>%
    rename(home = team) %>%
    inner_join(side_team("VISITORDESCRIPTION") %>% rename(away = team), by = "game_id")
}

# Games, win share and average final margin per team-season, from the nbastats events of
# that season only. SCOREMARGIN is the home team's margin; the last non-missing value of a
# game is its final margin.
team_season_results <- function(events, season) {
  ev <- events %>%
    mutate(
      game_id  = as.numeric(GAME_ID),
      eventnum = as.numeric(EVENTNUM),
      period   = as.integer(PERIOD)
    )

  teams <- home_teams(events)

  final <- ev %>%
    filter(!is.na(SCOREMARGIN), SCOREMARGIN != "TIE") %>%
    group_by(game_id) %>%
    slice_max(tibble(period, eventnum), n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(game_id, home_margin = as.numeric(SCOREMARGIN)) %>%
    inner_join(teams, by = "game_id")

  bind_rows(
    final %>% transmute(game_id, team = home, margin = home_margin),
    final %>% transmute(game_id, team = away, margin = -home_margin)
  ) %>%
    group_by(team) %>%
    summarise(
      games         = n(),
      win_pct       = mean(margin > 0),
      point_diff_pg = mean(margin),
      .groups = "drop"
    ) %>%
    mutate(season = season_label(season), .before = 1)
}

# Overall pace: offensive possessions per game of each team in a season, all possession types
# (pbpstats dump; the offense is the other team of the game). Used as a descriptive colour in
# the report, not in the model.
team_season_pace <- function(raw, season) {
  poss <- raw %>%
    distinct(GAMEID, PERIOD, STARTTIME, ENDTIME, OPPONENT) %>%
    count(GAMEID, OPPONENT, name = "possessions")
  teams <- poss %>%
    distinct(GAMEID, OPPONENT) %>%
    group_by(GAMEID) %>%
    mutate(team = if (n() == 2) rev(OPPONENT) else NA_character_) %>%
    ungroup()
  poss %>%
    left_join(teams, by = c("GAMEID", "OPPONENT")) %>%
    filter(!is.na(team)) %>%
    group_by(team) %>%
    summarise(poss_per_game = mean(possessions), .groups = "drop") %>%
    mutate(season = season_label(season), .before = 1)
}
