# Data list for the Stan models (stan/m2_partial_pooling.stan) from a team-games table
# (summarise_team_games() plus the home flag). Returns the Stan data and the table of
# team-seasons, so results can be mapped back to team names.

library(dplyr)

make_stan_data <- function(team_games) {
  # Within-game SD of seconds to first action, pooled by season. Team-games with a single
  # possession have no SD and are left out of the pooling (they still enter the model).
  s_within <- team_games %>%
    filter(n_poss > 1, !is.na(sd_delta)) %>%
    group_by(season) %>%
    summarise(s = sqrt(sum((n_poss - 1) * sd_delta^2) / sum(n_poss - 1)), .groups = "drop") %>%
    arrange(season)

  team_seasons <- team_games %>%
    distinct(season, team_season) %>%
    arrange(season, team_season) %>%
    mutate(t = row_number(), season_id = match(season, s_within$season))

  tg <- team_games %>%
    mutate(
      t = match(team_season, team_seasons$team_season),
      o = match(opp_season, team_seasons$team_season),
      s = match(season, s_within$season)
    )

  pr <- function(name, k) priors[[k]][priors$parameter == name]

  data <- list(
    K = nrow(tg), T = nrow(team_seasons), S = nrow(s_within),
    t = tg$t, o = tg$o, s = tg$s, season_of = team_seasons$season_id,
    n = tg$n_poss, dbar = tg$mean_delta, s_within = s_within$s,
    y = tg$net_per100, home = tg$home,
    pr_mbar_mean = pr("m_bar_s", "p1"), pr_mbar_sd = pr("m_bar_s", "p2"),
    pr_sigma_m_rate = pr("sigma_m", "p1"),
    pr_tau_rate = pr("tau", "p1"),
    pr_alpha_sd = pr("alpha_s", "p2"),
    pr_gamma_mean = pr("gamma", "p1"), pr_gamma_sd = pr("gamma", "p2"),
    pr_sigma_a_rate = pr("sigma_a", "p1"),
    pr_beta_bar_mean = pr("beta_bar", "p1"), pr_beta_bar_sd = pr("beta_bar", "p2"),
    pr_sigma_beta_rate = pr("sigma_beta", "p1"),
    pr_sigma_b_rate = pr("sigma_b", "p1"),
    pr_h_sd = pr("h", "p2"),
    pr_sigma_mean = pr("sigma", "p1"), pr_sigma_sd = pr("sigma", "p2")
  )

  list(data = data, team_seasons = team_seasons, team_games = tg)
}
