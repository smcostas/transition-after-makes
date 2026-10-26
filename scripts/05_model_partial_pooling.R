## 05. Model m2: partial pooling (main model)
##
##   dbar_k ~ Normal(mu_k, s_s / sqrt(n_k))       observed pace after makes
##   mu_k   ~ Normal(m_t, tau)                     true pace that night (latent)
##   m_t    ~ Normal(mbar_s, sigma_m)              usual pace of the team-season
##   y_k    ~ Normal(eta_k, sigma / sqrt(n_k))     net points per 100
##   eta_k  = alpha_s + a_t + beta_t * (m_t - mu_k) + b_o + h * home_k
##   a_t    ~ Normal(gamma * (mbar_s - m_t), sigma_a)   level (gamma: between teams)
##   beta_t ~ Normal(beta_bar, sigma_beta)              return (within team)
##   b_o    ~ Normal(0, sigma_b)                        opponent
##
## Bayesian workflow:
##   [1] Data
##   [2] Model specification (priors in R/priors.R + path to the .stan)
##   [3] Fit
##   [4] MCMC check
##   [5] Fit check (DHARMa + posterior predictive checks)
##   [6] Interpretation (technical): priors vs posteriors, returns, shrinkage against m1,
##       LOO, quadrants, named team-seasons, wins equivalent
##
## The same script fits the game-state analyses (06): change `analysis` below.
##
## Open the RProject (transition-after-makes.Rproj) before running this script.

# Packages ----------------------------------------------------------------

library(here)
library(tidyverse)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(DHARMa)
library(loo)
source(here("R", "priors.R"))
source(here("R", "model_data.R"))
source(here("R", "derived.R"))

# "all", "close" or "q1". 06_game_state.R sets it before sourcing this script.
if (!exists("analysis")) analysis <- "all"


# 1. Data -----------------------------------------------------------------

data_file <- c(all = "team_games.rds", close = "team_games_close.rds", q1 = "team_games_q1.rds")
team_games <- read_rds(here("data", "processed", data_file[[analysis]]))
results <- read_rds(here("data", "processed", "team_season_results.rds"))

prep <- make_stan_data(team_games)
d <- prep$team_games
team_seasons <- prep$team_seasons
datos_stan <- prep$data

c(team_games = datos_stan$K, team_seasons = datos_stan$T)
datos_stan$s_within


# 2. Model specification ----------------------------------------------------
# Priors come from R/priors.R through make_stan_data(); see the formulation for their
# densities and the prior predictive check.

priors

modelo <- cmdstan_model(here("stan", "m2_partial_pooling.stan"))


# 3. Fit ------------------------------------------------------------------

fit <- modelo$sample(
  data            = datos_stan,
  chains          = 4,
  parallel_chains = 4,
  iter_warmup     = 1000,
  iter_sampling   = 2000,   # sigma_beta needs more than 1000 for ESS > 400
  adapt_delta     = if (analysis == "all") 0.9 else 0.99,   # subsets: fewer possessions, funnels
  seed            = 42,
  show_messages   = FALSE
)


# 4. MCMC check -----------------------------------------------------------
# rhat <= 1.01, ess_bulk and ess_tail > 400, no divergences

fit$cmdstan_diagnose()

hiper <- c("mbar", "sigma_m", "tau", "alpha", "gamma", "sigma_a", "beta_bar", "sigma_beta",
           "sigma_b", "h", "sigma")
fit$summary(variables = hiper)

fit$summary(variables = c(hiper, "m", "a", "beta", "b", "mu")) %>%
  summarise(max_rhat = max(rhat), min_ess_bulk = min(ess_bulk), min_ess_tail = min(ess_tail))

muestras <- fit$draws(variables = hiper, format = "draws_df")
mcmc_trace(muestras, regex_pars = c("gamma", "beta_bar", "sigma_beta", "sigma_a", "tau", "^h$"))
mcmc_rank_overlay(muestras,
                  regex_pars = c("gamma", "beta_bar", "sigma_beta", "sigma_a", "tau", "^h$"))


# 5. Fit check ----------------------------------------------------------------

eta_mat   <- fit$draws(variables = "eta", format = "draws_matrix")
mu_mat    <- fit$draws(variables = "mu", format = "draws_matrix")
sigma_vec <- as.numeric(fit$draws(variables = "sigma", format = "draws_matrix"))

set.seed(7)
use <- sample(nrow(eta_mat), 500)
se_dbar <- datos_stan$s_within[datos_stan$s] / sqrt(datos_stan$n)
ysim <- matrix(NA, datos_stan$K, length(use))
dsim <- matrix(NA, datos_stan$K, length(use))
for (j in seq_along(use)) {
  ysim[, j] <- rnorm(datos_stan$K, eta_mat[use[j], ], sigma_vec[use[j]] / sqrt(datos_stan$n))
  dsim[, j] <- rnorm(datos_stan$K, mu_mat[use[j], ], se_dbar)
}

res <- createDHARMa(simulatedResponse = ysim, observedResponse = datos_stan$y,
                    integerResponse = FALSE)
plot(res)
plotResiduals(res, form = datos_stan$dbar)

ppc_y <- ppc_dens_overlay(y = datos_stan$y, yrep = t(ysim[, 1:50])) +
  labs(x = "Net points per 100 (team-game)")
ppc_dbar <- ppc_dens_overlay(y = datos_stan$dbar, yrep = t(dsim[, 1:50])) +
  labs(x = "Mean seconds to first action (team-game)")
ppc_y
ppc_dbar

# Save the fit checks for the report (report/index.qmd, Methods > Fit checks)
fig_dir <- here("output", "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
png(file.path(fig_dir, paste0(analysis, "_dharma.png")), width = 1100, height = 550, res = 110)
plot(res)
dev.off()
png(file.path(fig_dir, paste0(analysis, "_dharma_pace.png")), width = 900, height = 550, res = 110)
plotResiduals(res, form = datos_stan$dbar, xlab = "Observed pace (rank)")
dev.off()
ggsave(file.path(fig_dir, paste0(analysis, "_ppc_y.png")), ppc_y, width = 6, height = 3.6)
ggsave(file.path(fig_dir, paste0(analysis, "_ppc_dbar.png")), ppc_dbar, width = 6, height = 3.6)


# 6. Interpretation (technical) ---------------------------------------------

# -- Priors vs posteriors of the population-level parameters --
set.seed(11)
S <- nrow(muestras)
draw_prior <- function(name) {
  f <- priors$family[priors$parameter == name]
  a <- priors$p1[priors$parameter == name]
  b <- priors$p2[priors$parameter == name]
  if (f == "normal") rnorm(S, a, b) else if (f == "exponential") rexp(S, a) else abs(rnorm(S, a, b))
}
marg <- tribble(
  ~param,       ~prior_name,
  "beta_bar",   "beta_bar",
  "sigma_beta", "sigma_beta",
  "gamma",      "gamma",
  "sigma_a",    "sigma_a",
  "tau",        "tau",
  "h",          "h"
) %>%
  pmap_dfr(function(param, prior_name) {
    bind_rows(
      tibble(param = param, value = as.numeric(muestras[[param]]), type = "Posterior"),
      tibble(param = param, value = draw_prior(prior_name), type = "Prior")
    )
  }) %>%
  mutate(type = factor(type, levels = c("Prior", "Posterior")))

ggplot(marg, aes(value, fill = type, colour = type)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(values = c(Prior = "#3B0F70", Posterior = "#FE9F6D"), name = NULL) +
  scale_colour_manual(values = c(Prior = "#3B0F70", Posterior = "#FE9F6D"), name = NULL) +
  facet_wrap(~param, scales = "free") +
  labs(x = NULL, y = "Density", title = "Prior vs posterior") +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

# -- Correlation between population-level parameters --
mcmc_pairs(as_draws_matrix(muestras)[sample(nrow(muestras), 500), ],
           pars = c("gamma", "beta_bar", "sigma_beta", "sigma_a", "tau"),
           diag_fun = "dens", off_diag_fun = "scatter")

# -- Return by team-season --
beta_mat <- fit$draws(variables = "beta", format = "draws_matrix")
# Level relative to the league of its own season, draw by draw (lambda_t in the formulation)
a_mat    <- center_by_season(fit$draws(variables = "a", format = "draws_matrix"),
                             team_seasons$season)
m_mat    <- fit$draws(variables = "m", format = "draws_matrix")

quantiles <- function(mat) {
  q <- apply(mat, 2, quantile, probs = c(0.025, 0.1, 0.5, 0.9, 0.975))
  as_tibble(t(q)) %>% bind_cols(team_seasons)
}
ret <- quantiles(beta_mat)

ret %>%
  mutate(team_season = reorder(team_season, `50%`)) %>%
  ggplot(aes(`50%`, team_season)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_linerange(aes(xmin = `2.5%`, xmax = `97.5%`), colour = "grey60") +
  geom_linerange(aes(xmin = `10%`, xmax = `90%`), linewidth = 1) +
  geom_point() +
  facet_wrap(~season, scales = "free_y") +
  labs(x = "Return: net points per 100 per second faster (partial pooling)", y = NULL) +
  theme_classic(base_size = 10)

# -- Shrinkage: no pooling (m1) against partial pooling (m2) --
m1 <- read_rds(here("output", "m1_draws.rds"))
shrink <- tibble(
  team_season = m1$team_seasons$team_season,
  no_pooling = apply(m1$beta, 2, median)
) %>%
  inner_join(ret %>% select(team_season, season, partial_pooling = `50%`), by = "team_season")

shrink %>%
  mutate(team_season = reorder(team_season, no_pooling)) %>%
  ggplot(aes(y = team_season)) +
  geom_vline(xintercept = median(beta_mat), colour = "grey60", linetype = "dashed") +
  geom_segment(aes(x = no_pooling, xend = partial_pooling, yend = team_season),
               arrow = arrow(length = unit(0.12, "cm")), colour = "grey40") +
  geom_point(aes(x = no_pooling), colour = "grey70") +
  facet_wrap(~season, scales = "free_y") +
  labs(x = "Return (median): grey = no pooling, arrow tip = partial pooling", y = NULL) +
  theme_classic(base_size = 10)

# -- LOO: which model predicts left-out team-games better --
log_lik <- matrix(NA, nrow(eta_mat), datos_stan$K)
for (k in seq_len(datos_stan$K)) {
  log_lik[, k] <- dnorm(datos_stan$y[k], eta_mat[, k], sigma_vec / sqrt(datos_stan$n[k]),
                        log = TRUE)
}
loo_m2 <- loo(log_lik, r_eff = relative_eff(exp(log_lik), chain_id = rep(1:4, each = nrow(log_lik) / 4)))
loo_m2
if (analysis == "all") loo_compare(list(no_pooling = m1$loo, partial_pooling = loo_m2))

# -- Level against usual pace, with quadrants --
lev <- quantiles(a_mat) %>%
  mutate(intent = colMeans(m_mat))

lev %>%
  ggplot(aes(intent, `50%`)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_vline(aes(xintercept = med),
             data = lev %>% group_by(season) %>% summarise(med = median(intent)),
             colour = "grey60") +
  geom_linerange(aes(ymin = `10%`, ymax = `90%`), colour = "grey50") +
  geom_point() +
  scale_x_reverse() +
  facet_wrap(~season) +
  labs(x = "Usual pace after makes, seconds (faster to the right)",
       y = "Level: net points per 100 vs league (80% interval)") +
  theme_classic(base_size = 12)

qp <- quadrant_probs(a_mat, m_mat, team_seasons$season, team_seasons$team_season)
named_95 <- name_extremes(qp, a_mat, beta_mat, prob = 0.95)
named_80 <- name_extremes(qp, a_mat, beta_mat, prob = 0.80)
named_95
named_80

# -- Wins equivalent of the level --
w <- wins_per_point(results)
w
poss_pg <- d %>% group_by(t) %>% summarise(poss_pg = mean(n_poss)) %>% arrange(t)
slope_t <- w$slope[match(team_seasons$season, w$season)]
wins_mat <- wins_equivalent(a_mat, poss_pg$poss_pg, slope_t)
wins <- quantiles(wins_mat)
wins %>% arrange(desc(`50%`)) %>% select(team_season, `10%`, `50%`, `90%`)


# 7. Save -------------------------------------------------------------------

write_rds(
  list(
    analysis = analysis,
    team_seasons = team_seasons,
    hyper = fit$summary(variables = hiper),
    beta = beta_mat, a = a_mat, m = m_mat, wins = wins_mat,
    quadrants = qp, named_95 = named_95, named_80 = named_80,
    shrink = shrink, loo = loo_m2,
    hyper_draws = as_draws_df(fit$draws(variables = c("beta_bar", "gamma", "sigma_beta", "tau",
                                                      "sigma_m", "sigma_a", "h"))),
    mu_mean = tibble(team_season = d$team_season, game_id = d$game_id,
                     mu = colMeans(mu_mat), dbar = d$mean_delta, n = d$n_poss),
    diagnostics = fit$diagnostic_summary()
  ),
  here("output", paste0("m2_", analysis, ".rds"))
)
