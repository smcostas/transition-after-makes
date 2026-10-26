## 06. Game state: the same model on close possessions and on first quarters
##
## The main model (05) on all possessions mixes the choice to push with the score: teams that
## are behind speed up. Here the same model is fitted on
##   close  possessions in quarters 1-3 that start within 5 points
##   q1     first-quarter possessions only (the game starts tied)
## and the three analyses are compared. If the return keeps its sign and rough size, the
## conclusion does not depend on game state; if it changes, the score was driving it.
##
## Each fit takes about 15 minutes. Results: output/m2_close.rds, output/m2_q1.rds
##
## Open the RProject (transition-after-makes.Rproj) before running this script.

# Packages ----------------------------------------------------------------

library(here)
library(tidyverse)


# 1. Fit the two game-state analyses ----------------------------------------
# 05 reads `analysis` and saves output/m2_<analysis>.rds

for (analysis in c("close", "q1")) {
  source(here("scripts", "05_model_partial_pooling.R"), local = globalenv())
}
rm(analysis)


# 2. Diagnostics of the three fits --------------------------------------------

fits <- map(c(all = "all", close = "close", q1 = "q1"),
            ~ read_rds(here("output", paste0("m2_", .x, ".rds"))))

imap_dfr(fits, function(f, name) {
  tibble(
    analysis = name,
    divergences = sum(f$diagnostics$num_divergent),
    max_rhat = max(f$hyper$rhat),
    min_ess_bulk = min(f$hyper$ess_bulk)
  )
})


# 3. Population-level parameters across the three analyses --------------------

hyper <- imap_dfr(fits, function(f, name) {
  f$hyper %>%
    filter(variable %in% c("beta_bar", "sigma_beta", "gamma", "tau", "h")) %>%
    transmute(analysis = name, variable, median, q5, q95)
})
hyper

analysis_labels <- c(all = "All possessions", close = "Close (Q1-Q3, within 5)",
                     q1 = "First quarter")

hyper %>%
  mutate(analysis = factor(analysis_labels[analysis], levels = analysis_labels)) %>%
  filter(variable %in% c("beta_bar", "gamma")) %>%
  ggplot(aes(median, analysis)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_linerange(aes(xmin = q5, xmax = q95), linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~variable, labeller = as_labeller(c(
    beta_bar = "Mean return (within team), per second faster than usual",
    gamma = "Between teams, per second faster usual pace"
  ))) +
  labs(x = "Net points per 100 possessions after makes (median, 90% interval)", y = NULL) +
  theme_classic(base_size = 12)


# 4. Return by team-season across the three analyses ---------------------------

returns <- imap_dfr(fits, function(f, name) {
  q <- apply(f$beta, 2, quantile, probs = c(0.1, 0.5, 0.9))
  tibble(analysis = name, team_season = f$team_seasons$team_season,
         season = f$team_seasons$season, lo = q[1, ], med = q[2, ], hi = q[3, ])
})

returns %>%
  mutate(analysis = factor(analysis_labels[analysis], levels = analysis_labels),
         team_season = reorder(team_season, med)) %>%
  ggplot(aes(med, team_season, colour = analysis)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_linerange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.7)) +
  geom_point(position = position_dodge(width = 0.7), size = 1.2) +
  scale_colour_manual(values = c("grey30", "#2b8cbe", "#e6550d"), name = NULL) +
  facet_wrap(~season, scales = "free_y") +
  labs(x = "Return: net points per 100 per second faster than usual (median, 80% interval)",
       y = NULL) +
  theme_classic(base_size = 9) +
  theme(legend.position = "bottom")


# 5. Teams named in each analysis ----------------------------------------------

imap_dfr(fits, ~ .x$named_95 %>% mutate(analysis = .y, threshold = "95%")) %>%
  bind_rows(imap_dfr(fits, ~ .x$named_80 %>% mutate(analysis = .y, threshold = "80%"))) %>%
  select(analysis, threshold, season, role, team_season) %>%
  arrange(analysis, threshold, season, role)
