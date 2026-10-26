toy_universe <- function() {
  tibble::tibble(
    season          = "2024-25",
    game_id         = 1,
    period          = c(1, 1, 2, 3),
    team            = "MIA",
    opponent        = "BOS",
    start_margin    = c(0, 11, -10, 2),
    delta_s         = c(3, 7, 10, 20),
    length_s        = c(5, 9, 14, 24),
    dead_ball_first = c(FALSE, FALSE, TRUE, FALSE),
    net_points      = c(1, 0, -1, 2),
    excluded        = FALSE
  )
}

test_that("summarise_team_games gives mean and median delta per team-game", {
  tg <- summarise_team_games(toy_universe())
  expect_equal(nrow(tg), 1)
  expect_equal(tg$mean_delta, 10)
  expect_equal(tg$median_delta, 8.5)
  expect_equal(tg$n_poss, 4)
  expect_equal(tg$net_per100, 50)
})

test_that("summarise_team_games uses possession length when intent = possession_length", {
  tg <- summarise_team_games(toy_universe(), intent = "possession_length")
  expect_equal(tg$mean_delta, 13)
})

test_that("drop_dead_ball removes possessions with a dead ball before the first action", {
  tg <- summarise_team_games(toy_universe(), drop_dead_ball = TRUE)
  expect_equal(tg$n_poss, 3)
})

test_that("close keeps possessions within the margin at their start, not by final result", {
  tg <- summarise_team_games(toy_universe(), subset = "close", close_margin = 10)
  expect_equal(tg$n_poss, 3) # the possession starting at +11 is dropped, -10 is kept
})

test_that("close uses a margin of 5 by default and leaves out the 4th quarter and overtime", {
  u <- toy_universe()
  u$period <- c(1, 4, 5, 2)
  u$start_margin <- c(5, 0, 0, -6)
  tg <- summarise_team_games(u, subset = "close")
  expect_equal(tg$n_poss, 1) # only the first-quarter possession at +5
})

test_that("q1 keeps only first-quarter possessions", {
  tg <- summarise_team_games(toy_universe(), subset = "q1")
  expect_equal(tg$n_poss, 2)
})

test_that("excluded possessions never enter the summaries", {
  u <- toy_universe()
  u$excluded[4] <- TRUE
  expect_equal(summarise_team_games(u)$n_poss, 3)
})

test_that("team_season distinguishes the same team in different seasons", {
  u <- dplyr::bind_rows(toy_universe(), dplyr::mutate(toy_universe(), season = "2023-24"))
  tg <- summarise_team_games(u)
  expect_setequal(tg$team_season, c("MIA 2024-25", "MIA 2023-24"))
  expect_setequal(tg$opp_season, c("BOS 2024-25", "BOS 2023-24"))
})

test_that("team_season_results gives win share and average margin from the events", {
  ev <- tibble::tribble(
    ~GAME_ID, ~EVENTNUM, ~PERIOD, ~HOMEDESCRIPTION, ~VISITORDESCRIPTION, ~PLAYER1_TEAM_ABBREVIATION, ~SCOREMARGIN,
    "1", "1", "1", "A shot", NA, "A", "2",
    "1", "2", "1", NA, "B shot", "B", NA,
    "1", "3", "4", "A shot", NA, "A", "10",
    "2", "1", "1", "A shot", NA, "A", "-1",
    "2", "2", "1", NA, "C shot", "C", NA,
    "2", "3", "4", NA, "C shot", "C", "-4"
  )
  res <- team_season_results(ev, season = 2024)
  a <- res[res$team == "A", ]
  expect_equal(a$games, 2)
  expect_equal(a$win_pct, 0.5)
  expect_equal(a$point_diff_pg, 3)
  expect_equal(res$point_diff_pg[res$team == "B"], -10)
  expect_equal(unique(res$season), "2024-25")
})

test_that("home_teams finds the home and away team of each game from the descriptions", {
  ev <- tibble::tribble(
    ~GAME_ID, ~HOMEDESCRIPTION, ~VISITORDESCRIPTION, ~PLAYER1_TEAM_ABBREVIATION,
    "1", "A shot", NA, "A",
    "1", NA, "B shot", "B",
    "1", "A rebound", NA, "A"
  )
  ht <- home_teams(ev)
  expect_equal(ht$game_id, 1)
  expect_equal(ht$home, "A")
  expect_equal(ht$away, "B")
})

test_that("team_season_pace counts offensive possessions per game for each team", {
  raw <- tibble::tribble(
    ~GAMEID, ~PERIOD, ~STARTTIME, ~ENDTIME, ~OPPONENT,
    "1", "1", "12:00", "11:40", "B",   # A on offense
    "1", "1", "12:00", "11:40", "B",   # same possession, another event row
    "1", "1", "11:40", "11:20", "A",   # B on offense
    "1", "1", "11:20", "11:00", "B",   # A on offense
    "2", "1", "12:00", "11:30", "C",   # A on offense
    "2", "1", "11:30", "11:10", "A"    # C on offense
  )
  pace <- team_season_pace(raw, season = 2024)
  expect_equal(pace$poss_per_game[pace$team == "A"], 1.5) # 2 in game 1, 1 in game 2
  expect_equal(unique(pace$season), "2024-25")
})
