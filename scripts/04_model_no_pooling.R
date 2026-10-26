## 04. Model m1: no pooling (reference for the shrinkage comparison)
##
##   dbar_k ~ Normal(mu_k, s_s / sqrt(n_k))       observed pace after makes
##   mu_k   ~ Normal(m_t, tau)                     true pace that night (latent)
##   y_k    ~ Normal(eta_k, sigma / sqrt(n_k))     net points per 100
##   eta_k  = alpha_s + a_t + beta_t * (m_t - mu_k) + b_o + h * home_k
##
## No pooling: m_t, a_t, beta_t and b_o have fixed priors; team-seasons share nothing.
## This is not the final model. It shows how noisy each team-season is on its own, so the
## partial-pooling model (05) can be compared with it.
##
## Bayesian workflow:
##   [1] Data
##   [2] Model specification (priors + path to the .stan)
##   [3] Fit (sampling the posterior with Stan)
##   [4] MCMC check (convergence of the chains)
##   [5] Fit check (DHARMa + posterior predictive checks)
##   [6] Interpretation (technical: estimates and their uncertainty)
##
## Open the RProject (transition-after-makes.Rproj) before running this script.

# Packages ----------------------------------------------------------------

library(here)
library(tidyverse)
library(cmdstanr)    # R -> Stan interface: compiles and samples the model
library(posterior)   # draws_df: standard format for MCMC draws
library(bayesplot)   # mcmc_trace(), mcmc_rank_overlay(), ppc_*()
library(DHARMa)      # cumulative-probability residuals
library(loo)         # leave-one-out predictive comparison
source(here("R", "priors.R"))


# 1. Data -----------------------------------------------------------------

d <- read_rds(here("data", "processed", "team_games.rds"))

# Within-game SD of seconds to first action, pooled by season (see formulation, Submodel 1)
s_within <- d %>%
  filter(n_poss > 1) %>%
  group_by(season) %>%
  summarise(s = sqrt(sum((n_poss - 1) * sd_delta^2) / sum(n_poss - 1)))
s_within

# Indices: team-seasons ordered by season, 30 per season
team_seasons <- d %>%
  distinct(season, team_season) %>%
  arrange(season, team_season) %>%
  mutate(t = row_number(), season_id = as.integer(factor(season))) %>%
  group_by(season) %>%
  mutate(slot = row_number()) %>%
  ungroup()

d <- d %>%
  left_join(team_seasons %>% select(team_season, t, season_id), by = "team_season") %>%
  left_join(team_seasons %>% select(opp_season = team_season, o = t), by = "opp_season")

# Quick look: observed pace and net points per team-game
ggplot(d, aes(mean_delta, net_per100)) +
  geom_point(alpha = 0.15) +
  facet_wrap(~season) +
  labs(x = "Mean seconds to first action", y = "Net points per 100") +
  theme_classic(base_size = 12)


# 2. Model specification ----------------------------------------------------
# Hierarchical priors (R/priors.R) are reused as fixed priors for the team-season parameters.

pr <- function(name, k) priors[[k]][priors$parameter == name]

prior_values <- list(
  pr_m_mean     = pr("m_bar_s", "p1"),
  pr_m_sd       = pr("m_bar_s", "p2"),
  pr_tau_rate   = pr("tau", "p1"),
  pr_alpha_sd   = pr("alpha_s", "p2"),
  pr_a_sd       = 10,     # fixed prior for each level (no pooling)
  pr_beta_mean  = pr("beta_bar", "p1"),
  pr_beta_sd    = pr("beta_bar", "p2"),
  pr_b_sd       = 10,     # fixed prior for each opponent effect (no pooling)
  pr_h_sd       = pr("h", "p2"),
  pr_sigma_mean = pr("sigma", "p1"),
  pr_sigma_sd   = pr("sigma", "p2")
)

modelo <- cmdstan_model(here("stan", "m1_no_pooling.stan"))


# 3. Fit ------------------------------------------------------------------

datos_stan <- c(
  list(
    K = nrow(d), T = nrow(team_seasons), S = n_distinct(d$season), TS = 30,
    t = d$t, o = d$o, s = d$season_id,
    season_of = team_seasons$season_id, slot_of = team_seasons$slot,
    n = d$n_poss, dbar = d$mean_delta, s_within = s_within$s,
    y = d$net_per100, home = d$home
  ),
  prior_values
)

fit <- modelo$sample(
  data            = datos_stan,
  chains          = 4,
  parallel_chains = 4,
  iter_warmup     = 1000,
  iter_sampling   = 1000,
  seed            = 42,
  show_messages   = FALSE
)


# 4. MCMC check -----------------------------------------------------------
# rhat <= 1.01, ess_bulk and ess_tail > 400, no divergences

fit$cmdstan_diagnose()

globales <- c("alpha", "tau", "h", "sigma")
fit$summary(variables = globales)

# Worst rhat and smallest ESS over every parameter, including the 4,916 latent paces
todo <- fit$summary(variables = c("m", "beta", "a", "b", "mu", globales))
todo %>% summarise(max_rhat = max(rhat), min_ess_bulk = min(ess_bulk),
                   min_ess_tail = min(ess_tail))

muestras <- fit$draws(variables = globales, format = "draws_df")
mcmc_trace(muestras, regex_pars = c("alpha", "tau", "^h$", "sigma"))
mcmc_rank_overlay(muestras, regex_pars = c("alpha", "tau", "^h$", "sigma"))


# 5. Fit check ----------------------------------------------------------------
# Simulate net points per 100 for every team-game from 500 posterior draws and compare
# with the observed values.

eta_mat   <- fit$draws(variables = "eta", format = "draws_matrix")
sigma_vec <- as.numeric(fit$draws(variables = "sigma", format = "draws_matrix"))
mu_mat    <- fit$draws(variables = "mu", format = "draws_matrix")

set.seed(7)
use <- sample(nrow(eta_mat), 500)
ysim <- matrix(NA, nrow(d), length(use))
dsim <- matrix(NA, nrow(d), length(use))
se_dbar <- s_within$s[d$season_id] / sqrt(d$n_poss)
for (j in seq_along(use)) {
  ysim[, j] <- rnorm(nrow(d), eta_mat[use[j], ], sigma_vec[use[j]] / sqrt(d$n_poss))
  dsim[, j] <- rnorm(nrow(d), mu_mat[use[j], ], se_dbar)
}

# -- DHARMa: residuals of net points per 100 --
res <- createDHARMa(simulatedResponse = ysim, observedResponse = d$net_per100,
                    integerResponse = FALSE)
plot(res)
plotResiduals(res, form = d$mean_delta)

# -- Posterior predictive checks --
ppc_dens_overlay(y = d$net_per100, yrep = t(ysim[, 1:50])) +
  labs(x = "Net points per 100")
ppc_dens_overlay(y = d$mean_delta, yrep = t(dsim[, 1:50])) +
  labs(x = "Mean seconds to first action")


# 6. Interpretation (technical) ---------------------------------------------
# Return of each team-season on its own: net points per 100 per second faster than its usual
# pace. With no pooling these intervals show how much each team-season's data says alone.

ret <- fit$summary(variables = "beta", ~ quantile(.x, c(0.025, 0.1, 0.5, 0.9, 0.975))) %>%
  bind_cols(team_seasons)

ret %>%
  mutate(team_season = reorder(team_season, `50%`)) %>%
  ggplot(aes(`50%`, team_season)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_linerange(aes(xmin = `2.5%`, xmax = `97.5%`), colour = "grey60") +
  geom_linerange(aes(xmin = `10%`, xmax = `90%`), linewidth = 1) +
  geom_point() +
  facet_wrap(~season, scales = "free_y") +
  labs(x = "Return: net points per 100 per second faster (no pooling)", y = NULL) +
  theme_classic(base_size = 10)

# -- LOO: expected log predictive density of each team-game's net points --
# Used in 05 to compare with the partial-pooling model.
log_lik <- matrix(NA, nrow(eta_mat), nrow(d))
for (k in seq_len(nrow(d))) {
  log_lik[, k] <- dnorm(d$net_per100[k], eta_mat[, k], sigma_vec / sqrt(d$n_poss[k]), log = TRUE)
}
loo_m1 <- loo(log_lik, r_eff = relative_eff(exp(log_lik), chain_id = rep(1:4, each = 1000)))
loo_m1

# Save what the comparison with the partial-pooling model needs
dir.create(here("output"), showWarnings = FALSE)
write_rds(
  list(
    beta = fit$draws(variables = "beta", format = "draws_matrix"),
    a = fit$draws(variables = "a", format = "draws_matrix"),
    m = fit$draws(variables = "m", format = "draws_matrix"),
    loo = loo_m1,
    team_seasons = team_seasons
  ),
  here("output", "m1_draws.rds")
)
