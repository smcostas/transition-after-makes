toy_team_games <- function() {
  tibble::tibble(
    season      = c("2023-24", "2023-24", "2024-25", "2024-25"),
    game_id     = c(1, 1, 2, 2),
    team        = c("MIA", "BOS", "MIA", "BOS"),
    opponent    = c("BOS", "MIA", "BOS", "MIA"),
    team_season = paste(team, season),
    opp_season  = paste(opponent, season),
    n_poss      = c(30, 28, 31, 1),
    mean_delta  = c(15, 17, 16, 18),
    sd_delta    = c(5, 4, 6, NA),
    net_per100  = c(10, -5, 0, 3),
    home        = c(1, 0, 0, 1)
  )
}

test_that("make_stan_data maps each team-game to its own and its opponent's team-season", {
  sd <- make_stan_data(toy_team_games())
  ts <- sd$team_seasons
  expect_equal(ts$team_season[sd$data$t], toy_team_games()$team_season)
  expect_equal(ts$team_season[sd$data$o], toy_team_games()$opp_season)
})

test_that("make_stan_data keeps every team-season inside its own season", {
  sd <- make_stan_data(toy_team_games())
  expect_equal(sd$data$season_of[sd$data$t], sd$data$s)
  expect_equal(sd$data$season_of[sd$data$o], sd$data$s)
})

test_that("make_stan_data pools the within-game SD by season and ignores single possessions", {
  sd <- make_stan_data(toy_team_games())
  expect_equal(sd$data$s_within[1], sqrt((29 * 25 + 27 * 16) / (29 + 27)))
  expect_equal(sd$data$s_within[2], 6)
})

test_that("make_stan_data adds the priors from R/priors.R", {
  sd <- make_stan_data(toy_team_games())
  expect_equal(sd$data$pr_beta_bar_sd, 5)
  expect_equal(sd$data$pr_sigma_mean, 120)
})
