# Quantities derived from the posterior: quadrants, named extremes and wins equivalent.
# See report/formulation.qmd, section "Quantities to estimate".

library(dplyr)

# Levels relative to the league average of their own season, draw by draw. In m2 the season
# intercept and the mean level of a season are separated only by the prior; centring each draw
# gives the quantity the formulation defines (lambda_t, relative to the league).
center_by_season <- function(draws, season) {
  for (s in unique(season)) {
    cols <- which(season == s)
    draws[, cols] <- draws[, cols, drop = FALSE] - rowMeans(draws[, cols, drop = FALSE])
  }
  draws
}

# Probability that each team-season is in each quadrant. In every draw, a team-season is
# fast if its usual pace is below the median usual pace of its own season, and good if its
# level is above 0; probabilities are averages over draws.
#   level_draws   draws x team-seasons matrix of levels a_t
#   intent_draws  draws x team-seasons matrix of usual paces m_t (seconds; lower = faster)
quadrant_probs <- function(level_draws, intent_draws, season, team_season) {
  fast <- matrix(FALSE, nrow(intent_draws), ncol(intent_draws))
  for (s in unique(season)) {
    cols <- which(season == s)
    season_median <- apply(intent_draws[, cols, drop = FALSE], 1, median)
    fast[, cols] <- intent_draws[, cols, drop = FALSE] < season_median
  }
  good <- level_draws > 0
  tibble(
    team_season, season,
    intent      = unname(colMeans(intent_draws)),
    p_fast      = unname(colMeans(fast)),
    p_fast_good = unname(colMeans(fast & good)),
    p_fast_bad  = unname(colMeans(fast & !good)),
    p_slow_good = unname(colMeans(!fast & good)),
    p_slow_bad  = unname(colMeans(!fast & !good))
  )
}

# Team-seasons named in the report, within each season. A quadrant is named by the team-season
# with the highest probability of being in it, among those whose level interval (prob) lies
# entirely on that side of 0 and whose pace is on that side of the season median with
# probability >= intent_prob. The largest and smallest returns are named only if their
# interval excludes 0.
name_extremes <- function(qp, level_draws, return_draws, prob = 0.95, intent_prob = 0.9) {
  tails <- c((1 - prob) / 2, 1 - (1 - prob) / 2)
  interval <- function(draws) {
    q <- apply(draws, 2, quantile, probs = tails)
    tibble(lower = unname(q[1, ]), upper = unname(q[2, ]),
           median = unname(apply(draws, 2, median)))
  }
  lv <- bind_cols(qp, interval(level_draws))
  rt <- bind_cols(qp %>% select(team_season, season), interval(return_draws))

  quadrants <- tibble(
    role = c("fast_good", "fast_bad", "slow_good", "slow_bad"),
    col  = paste0("p_", role),
    good = c(TRUE, FALSE, TRUE, FALSE),
    fast = c(TRUE, TRUE, FALSE, FALSE)
  )
  named_quadrants <- purrr::pmap_dfr(quadrants, function(role, col, good, fast) {
    lv %>%
      filter(
        if (good) lower > 0 else upper < 0,
        if (fast) p_fast >= intent_prob else 1 - p_fast >= intent_prob
      ) %>%
      group_by(season) %>%
      slice_max(.data[[col]], n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(season, role = role, team_season, value = .data[[col]])
  })
  named_returns <- bind_rows(
    rt %>%
      filter(lower > 0) %>%
      group_by(season) %>%
      slice_max(median, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(season, role = "max_return", team_season, value = median),
    rt %>%
      filter(upper < 0) %>%
      group_by(season) %>%
      slice_min(median, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(season, role = "min_return", team_season, value = median)
  )
  bind_rows(named_quadrants, named_returns)
}

# Wins over an 82-game season from net points per 100 possessions after makes.
#   net_draws      draws x team-seasons matrix (or a single value)
#   poss_per_game  possessions after an opponent's made field goal per game
#   win_per_point  change in win share per point of average scoring margin, same season
wins_equivalent <- function(net_draws, poss_per_game, win_per_point) {
  net_draws <- as.matrix(net_draws)
  scale <- poss_per_game * win_per_point * 82 / 100
  sweep(net_draws, 2, scale, `*`)
}

# Win share on average scoring margin, fitted separately within each season.
wins_per_point <- function(team_seasons) {
  team_seasons %>%
    group_by(season) %>%
    summarise(slope = unname(coef(lm(win_pct ~ point_diff_pg))[2]), .groups = "drop")
}
