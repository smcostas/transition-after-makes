# Shared data and plot helpers for the report (report/index.qmd). Reads what the scripts in
# scripts/ produce; nothing here fits a model.

library(here)
library(tidyverse)
library(ggrepel)
library(knitr)

accent <- "#1f5f8b"
grey_line <- "grey65"

theme_report <- function(base_size = 12) {
  theme_classic(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(colour = "grey35", size = rel(0.85)),
      plot.caption = element_text(colour = "grey45", size = rel(0.75)),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

analysis_labels <- c(all = "All possessions", close = "Close games", q1 = "First quarters")
analysis_detail <- c(
  all = "every possession after an opponent's made field goal",
  close = "quarters 1-3, score within 5 points",
  q1 = "first quarter only"
)

# Name used in public for the team-season level a_t
level_name <- "Net offensive rating after opponent scores"

# Data -----------------------------------------------------------------------

universe <- read_rds(here("data", "processed", "universe.rds"))
analysed <- universe %>% filter(!excluded, !is.na(net_points))
team_games <- read_rds(here("data", "processed", "team_games.rds"))
pace <- read_rds(here("data", "processed", "team_season_pace.rds"))
results <- read_rds(here("data", "processed", "team_season_results.rds"))
fits <- map(c(all = "all", close = "close", q1 = "q1"),
            ~ read_rds(here("output", paste0("m2_", .x, ".rds"))))
m1 <- read_rds(here("output", "m1_draws.rds"))

team_of <- function(team_season) sub(" .*", "", team_season)

# Posterior probability that a population parameter is positive. Uses the saved draws when
# available; otherwise a normal approximation from the 90% interval (marked with "~").
prob_positive <- function(fit, variable) {
  if (!is.null(fit$hyper_draws)) {
    return(sprintf("%.2f", mean(fit$hyper_draws[[variable]] > 0)))
  }
  h <- fit$hyper[fit$hyper$variable == variable, ]
  sd_approx <- (h$q95 - h$q5) / (2 * 1.645)
  sprintf("~%.2f", pnorm(h$median / sd_approx))
}

hyper_table <- imap_dfr(fits, function(f, name) {
  f$hyper %>%
    filter(variable %in% c("beta_bar", "gamma", "sigma_beta", "tau", "sigma_m", "sigma_a", "h")) %>%
    transmute(analysis = name, variable, median, q5, q95)
})

hyper_value <- function(analysis, variable, what = "median") {
  hyper_table[[what]][hyper_table$analysis == analysis & hyper_table$variable == variable]
}

fmt <- function(x, digits = 1) formatC(x, format = "f", digits = digits)
fmt_interval <- function(analysis, variable, digits = 1) {
  paste0(fmt(hyper_value(analysis, variable), digits), " (",
         fmt(hyper_value(analysis, variable, "q5"), digits), " to ",
         fmt(hyper_value(analysis, variable, "q95"), digits), ")")
}

# Level (a_t), usual pace (m_t) and naming for one fit
level_table <- function(fit) {
  tibble(
    team_season = fit$team_seasons$team_season,
    season = fit$team_seasons$season,
    team = team_of(fit$team_seasons$team_season),
    intent = colMeans(fit$m),
    intent_lo = apply(fit$m, 2, quantile, 0.1),
    intent_hi = apply(fit$m, 2, quantile, 0.9),
    lo = apply(fit$a, 2, quantile, 0.1),
    med = apply(fit$a, 2, median),
    hi = apply(fit$a, 2, quantile, 0.9)
  ) %>%
    left_join(pace, by = c("season", "team")) %>%
    left_join(fit$named_95 %>% filter(!role %in% c("max_return", "min_return")) %>%
                select(team_season, role), by = "team_season") %>%
    mutate(named = !is.na(role))
}

return_table <- function(fit, name) {
  q <- apply(fit$beta, 2, quantile, probs = c(0.025, 0.1, 0.5, 0.9, 0.975))
  tibble(analysis = name, team_season = fit$team_seasons$team_season,
         season = fit$team_seasons$season, lo95 = q[1, ], lo = q[2, ], med = q[3, ],
         hi = q[4, ], hi95 = q[5, ])
}
