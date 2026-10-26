# Priors of the model (report/formulation.qmd, section Priors). Defined once here so the
# formulation, its prior predictive check and the Stan data in scripts/ use the same values.
# family: normal (p1 = mean, p2 = sd), exponential (p1 = rate), half-normal (p1, p2 of the
# normal before truncation at 0).

priors <- tibble::tribble(
  ~parameter,      ~family,       ~p1,  ~p2, ~why,
  "m_bar_s",       "normal",      16,   3,   "League mean pace; data around 16.3 s",
  "sigma_m",       "exponential", 1,    NA,  "Spread of usual paces; teams range 14-18 s",
  "tau",           "exponential", 1,    NA,  "Game-to-game variation of true pace",
  "alpha_s",       "normal",      0,    10,  "League average net points per 100; data around -2",
  "gamma",         "normal",      0,    5,   "Between-team association, per second faster",
  "sigma_a",       "exponential", 0.2,  NA,  "Spread of team-season levels (mean 5)",
  "beta_bar",      "normal",      0,    5,   "Mean return, per second faster",
  "sigma_beta",    "exponential", 0.33, NA,  "Spread of returns across team-seasons (mean 3)",
  "sigma_b",       "exponential", 0.2,  NA,  "Spread of opponent effects (mean 5)",
  "h",             "normal",      0,    5,   "Home advantage",
  "sigma",         "half-normal", 120,  30,  "SD of one possession's net points x 100; observed 124"
)
