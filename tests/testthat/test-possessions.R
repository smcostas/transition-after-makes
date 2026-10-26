# Toy data mimic the raw sources: pbpstats has one row per video event inside a possession,
# no offense column (only OPPONENT, the defense), and clocks as "MM:SS"; nbastats events use
# "M:SS". STARTSCOREDIFFERENTIAL is from the offense's point of view.

toy_raw <- function() {
  tibble::tribble(
    ~GAMEID, ~PERIOD, ~STARTTIME, ~ENDTIME, ~OPPONENT, ~STARTTYPE, ~STARTSCOREDIFFERENTIAL, ~DESCRIPTION,
    "1", "1", "12:00", "11:40", "B", "Off Dead Ball", "0", "A makes 2",
    "1", "1", "11:40", "11:20", "A", "Off At Rim Make", "-2", "B misses",
    "1", "1", "11:40", "11:20", "A", "Off At Rim Make", "-2", "B makes 3",
    "1", "1", "11:20", "11:00", "B", "Off Arc 3 Make", "-1", "A shooting foul drawn"
  )
}

toy_events <- function() {
  tibble::tribble(
    ~GAME_ID, ~EVENTNUM, ~EVENTMSGTYPE, ~EVENTMSGACTIONTYPE, ~PERIOD, ~PCTIMESTRING, ~PLAYER1_TEAM_ABBREVIATION,
    "1", "10", "1", "1", "1", "11:40", "A", # A's make that ends possession 1 (defense for poss 2)
    "1", "11", "8", "0", "1", "11:35", NA,  # substitution: dead ball
    "1", "12", "2", "1", "1", "11:30", "B", # B's missed shot: first action of poss 2
    "1", "13", "1", "1", "1", "11:20", "B", # B's make
    "1", "20", "6", "2", "1", "11:15", "B"  # shooting foul by B (defense): first action of poss 3
  )
}

test_that("clock_to_seconds reads both MM:SS and M:SS", {
  expect_equal(clock_to_seconds(c("11:43", "2:17", "00:04")), c(703, 137, 4))
})

test_that("prepare_possessions collapses event rows into one row per possession", {
  poss <- prepare_possessions(toy_raw(), season = 2023)
  expect_equal(nrow(poss), 3)
  expect_equal(unique(poss$season), "2023-24")
})

test_that("prepare_possessions recovers the offense as the other team of the game", {
  poss <- prepare_possessions(toy_raw(), season = 2023)
  expect_equal(poss$team, c("A", "B", "A"))
  expect_equal(poss$opponent, c("B", "A", "B"))
})

test_that("prepare_possessions derives points from the next possession's score differential", {
  poss <- prepare_possessions(toy_raw(), season = 2023)
  expect_equal(poss$points, c(2, 3, NA))
  expect_equal(poss$next_opp_start_type, c("Off At Rim Make", "Off Arc 3 Make", NA))
})

test_that("first_actions finds the first offensive action and a dead ball before it", {
  poss <- prepare_possessions(toy_raw(), season = 2023)
  fa <- first_actions(poss, toy_events())
  p2 <- fa[fa$poss_id == poss$poss_id[2], ]
  expect_equal(p2$delta_s, 10)
  expect_true(p2$dead_ball_first)
})

test_that("first_actions counts a shooting foul by the defense as the first action", {
  poss <- prepare_possessions(toy_raw(), season = 2023)
  fa <- first_actions(poss, toy_events())
  p3 <- fa[fa$poss_id == poss$poss_id[3], ]
  expect_equal(p3$delta_s, 5)
  expect_false(p3$dead_ball_first)
})

test_that("build_universe keeps only possessions after an opponent's made field goal", {
  poss <- prepare_possessions(toy_raw(), season = 2023)
  poss$start_type[3] <- "Off FT Make"
  fa <- first_actions(poss, toy_events())
  u <- build_universe(poss, fa)
  expect_equal(u$poss_id, poss$poss_id[2])
})

test_that("build_universe drops and counts negative deltas and missing first actions", {
  poss <- prepare_possessions(toy_raw(), season = 2023)
  fa <- tibble::tibble(poss_id = poss$poss_id[2], delta_s = -1, dead_ball_first = FALSE)
  u <- build_universe(poss, fa)
  expect_equal(nrow(u), 0)
  expect_equal(attr(u, "n_negative_delta"), 1)
  expect_equal(attr(u, "n_no_first_action"), 1) # possession 3 has no first action in fa
})

test_that("excl_last2min flags the last 2:00 of the 4th quarter and of every overtime", {
  df <- tibble::tibble(period = c(4, 5, 3), start_s = c(120, 120, 100), start_margin = 0)
  expect_equal(flag_exclusions(df)$excl_last2min, c(TRUE, TRUE, FALSE))
})

test_that("excl_end_quarter flags the last 54 s of quarters 1 to 3 (2-for-1, last shot)", {
  df <- tibble::tibble(period = c(1, 3, 3, 4), start_s = c(54, 10, 55, 50), start_margin = 0)
  expect_equal(flag_exclusions(df)$excl_end_quarter, c(TRUE, TRUE, FALSE, FALSE))
})

test_that("excl_garbage applies the Cleaning the Glass margin rule in the 4th quarter only", {
  df <- tibble::tibble(
    period       = c(4, 4, 4, 4, 3),
    start_s      = c(650, 650, 400, 200, 650),
    start_margin = c(25, -24, -20, 10, 40)
  )
  expect_equal(flag_exclusions(df)$excl_garbage, c(TRUE, FALSE, TRUE, TRUE, FALSE))
})

test_that("excluded is TRUE when any exclusion rule applies", {
  df <- tibble::tibble(period = c(4, 1), start_s = c(100, 300), start_margin = c(0, 0))
  expect_equal(flag_exclusions(df)$excluded, c(TRUE, FALSE))
})

test_that("is_transition uses the FIBA 8 s rule and requires no dead ball", {
  expect_true(is_transition(8, FALSE))
  expect_false(is_transition(8.5, FALSE))
  expect_false(is_transition(3, TRUE))
})

test_that("next_possession_value is computed separately for each season", {
  poss <- tibble::tibble(
    season = c("2023-24", "2023-24", "2024-25", "2024-25"),
    start_type = "Off Steal",
    points = c(1, 1, 2, 2),
    period = 1, start_s = 600, start_margin = 0
  )
  v <- next_possession_value(poss)
  expect_equal(v$value[v$season == "2023-24" & v$start_type == "Off Steal"], 1)
  expect_equal(v$value[v$season == "2024-25" & v$start_type == "Off Steal"], 2)
})

test_that("next_possession_value leaves out the possessions the analysis excludes", {
  poss <- tibble::tibble(
    season = "2024-25",
    start_type = "Off Steal",
    points = c(1, 1, 3, 0),
    period = c(1, 2, 4, 2),
    start_s = c(600, 600, 100, 30),   # last 2:00 of Q4 and last 54 s of Q2 are excluded
    start_margin = 0
  )
  v <- next_possession_value(poss)
  expect_equal(v$value[v$start_type == "Off Steal"], 1)
})

test_that("add_net_points subtracts the value of the same season only", {
  u <- tibble::tibble(
    season = c("2023-24", "2024-25"),
    points = c(2, 2),
    next_opp_start_type = "Off Steal"
  )
  v <- tibble::tibble(season = c("2023-24", "2024-25"), start_type = "Off Steal",
                      value = c(1, 1.5))
  expect_equal(add_net_points(u, v)$net_points, c(1, 0.5))
})
