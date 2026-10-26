set.seed(1)
n_draws <- 4000

# Four team-seasons of one season. Intent = posterior mean usual pace (lower = faster).
toy_draws <- function() {
  list(
    level = cbind(
      fast_good = rnorm(n_draws, 5, 1),     # fast and clearly above the league
      fast_bad  = rnorm(n_draws, -5, 1),    # fast and clearly below
      slow_good = rnorm(n_draws, 4, 1),     # slow and clearly above
      slow_unclear = rnorm(n_draws, 0.5, 2) # slow, interval crosses 0
    ),
    ret = cbind(
      fast_good = rnorm(n_draws, 3, 0.5),
      fast_bad  = rnorm(n_draws, -3, 0.5),
      slow_good = rnorm(n_draws, 0.2, 2),
      slow_unclear = rnorm(n_draws, 0, 2)
    ),
    intent = cbind(rnorm(n_draws, 14.5, 0.1), rnorm(n_draws, 15, 0.1),
                   rnorm(n_draws, 17.5, 0.1), rnorm(n_draws, 18, 0.1)),
    season = rep("2024-25", 4),
    team_season = c("fast_good", "fast_bad", "slow_good", "slow_unclear")
  )
}

test_that("quadrant_probs gives the probability of each quadrant per team-season", {
  x <- toy_draws()
  qp <- quadrant_probs(x$level, x$intent, x$season, x$team_season)
  fg <- qp[qp$team_season == "fast_good", ]
  expect_gt(fg$p_fast_good, 0.99)
  expect_equal(fg$p_slow_good + fg$p_slow_bad, 0)
  expect_equal(sum(qp[1, c("p_fast_good", "p_fast_bad", "p_slow_good", "p_slow_bad")]), 1)
})

test_that("name_extremes names a team per quadrant only when its interval excludes 0", {
  x <- toy_draws()
  qp <- quadrant_probs(x$level, x$intent, x$season, x$team_season)
  named <- name_extremes(qp, x$level, x$ret, prob = 0.95)
  expect_equal(named$team_season[named$role == "fast_good"], "fast_good")
  expect_equal(named$team_season[named$role == "fast_bad"], "fast_bad")
  expect_equal(named$team_season[named$role == "slow_good"], "slow_good")
  expect_false("slow_unclear" %in% named$team_season)
})

test_that("name_extremes does not name a team whose pace sits on the season median", {
  x <- toy_draws()
  # a fifth team-season, clearly good, whose usual pace is the season median
  level <- cbind(x$level, on_line = rnorm(n_draws, 8, 1))
  intent <- cbind(x$intent, on_line = rnorm(n_draws, 16.5, 0.6))
  qp <- quadrant_probs(level, intent, rep("2024-25", 5), c(x$team_season, "on_line"))
  named <- name_extremes(qp, level, cbind(x$ret, on_line = rnorm(n_draws, 0, 2)), prob = 0.95)
  expect_false("on_line" %in% named$team_season)
})

test_that("name_extremes names the largest and smallest return only if clear", {
  x <- toy_draws()
  qp <- quadrant_probs(x$level, x$intent, x$season, x$team_season)
  named <- name_extremes(qp, x$level, x$ret, prob = 0.95)
  expect_equal(named$team_season[named$role == "max_return"], "fast_good")
  expect_equal(named$team_season[named$role == "min_return"], "fast_bad")
})

test_that("wins_equivalent converts net points per 100 into wins over 82 games", {
  expect_equal(wins_equivalent(matrix(2), 45, 0.033), matrix(2 / 100 * 45 * 82 * 0.033))
})

test_that("wins_per_point fits win share on average margin separately for each season", {
  ts <- tibble::tibble(
    season = rep(c("2023-24", "2024-25"), each = 3),
    point_diff_pg = c(-5, 0, 5, -5, 0, 5),
    win_pct = c(0.35, 0.5, 0.65, 0.25, 0.5, 0.75)
  )
  w <- wins_per_point(ts)
  expect_equal(w$slope[w$season == "2023-24"], 0.03)
  expect_equal(w$slope[w$season == "2024-25"], 0.05)
})

test_that("center_by_season makes levels relative to their own season in every draw", {
  draws <- rbind(c(1, 3, 10, 20), c(2, 4, 0, 4))
  season <- c("2023-24", "2023-24", "2024-25", "2024-25")
  centred <- center_by_season(draws, season)
  expect_equal(unname(centred[1, ]), c(-1, 1, -5, 5))
  expect_equal(unname(centred[2, ]), c(-1, 1, -2, 2))
})
