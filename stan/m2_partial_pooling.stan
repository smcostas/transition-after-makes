// m2: partial pooling across team-seasons (main model)
//
// Submodel 1, intention (latent pace after makes):
//   dbar_k ~ Normal(mu_k, s_{s[k]} / sqrt(n_k))     observed mean seconds to first action
//   mu_k   ~ Normal(m_{t[k]}, tau)                   true pace that night
//   m_t    ~ Normal(mbar_{s[t]}, sigma_m)            usual pace of the team-season
//
// Submodel 2, outcome:
//   y_k   ~ Normal(eta_k, sigma / sqrt(n_k))         net points per 100
//   eta_k = alpha_{s[k]} + a_{t[k]} + beta_{t[k]} * (m_{t[k]} - mu_k) + b_{o[k]} + h * home_k
//   a_t    ~ Normal(gamma * (mbar_{s[t]} - m_t), sigma_a)   level; gamma = between-team
//   beta_t ~ Normal(beta_bar, sigma_beta)                     return (within-team)
//   b_o    ~ Normal(0, sigma_b)                               opponent
//
// a, beta, b and mu use non-centred parameterisations.

data {
  int<lower=1> K;                       // team-games
  int<lower=1> T;                       // team-seasons
  int<lower=1> S;                       // seasons
  array[K] int<lower=1, upper=T> t;     // offense team-season of k
  array[K] int<lower=1, upper=T> o;     // opponent team-season of k
  array[K] int<lower=1, upper=S> s;     // season of k
  array[T] int<lower=1, upper=S> season_of;   // season of team-season t
  vector<lower=1>[K] n;                 // possessions in k
  vector[K] dbar;                       // observed mean seconds to first action
  vector<lower=0>[S] s_within;          // within-game SD of seconds, pooled by season
  vector[K] y;                          // net points per 100
  vector[K] home;                       // 1 = home

  // priors (R/priors.R)
  real pr_mbar_mean;  real<lower=0> pr_mbar_sd;
  real<lower=0> pr_sigma_m_rate;
  real<lower=0> pr_tau_rate;
  real<lower=0> pr_alpha_sd;
  real pr_gamma_mean;  real<lower=0> pr_gamma_sd;
  real<lower=0> pr_sigma_a_rate;
  real pr_beta_bar_mean;  real<lower=0> pr_beta_bar_sd;
  real<lower=0> pr_sigma_beta_rate;
  real<lower=0> pr_sigma_b_rate;
  real<lower=0> pr_h_sd;
  real pr_sigma_mean;  real<lower=0> pr_sigma_sd;
}

transformed data {
  vector[K] se_dbar = s_within[s] ./ sqrt(n);
}

parameters {
  vector[S] mbar;                 // league mean pace per season
  real<lower=0> sigma_m;          // spread of usual paces
  vector[T] m;                    // usual pace of each team-season
  real<lower=0> tau;              // game-to-game variation of true pace
  vector[K] z_mu;
  vector[S] alpha;                // league average net points per 100, per season
  real gamma;                     // between-team: level per second faster usual pace
  real<lower=0> sigma_a;
  vector[T] z_a;
  real beta_bar;                  // mean return
  real<lower=0> sigma_beta;
  vector[T] z_beta;
  real<lower=0> sigma_b;
  vector[T] z_b;
  real h;
  real<lower=0> sigma;
}

transformed parameters {
  vector[K] mu = m[t] + tau * z_mu;
  vector[T] a = gamma * (mbar[season_of] - m) + sigma_a * z_a;
  vector[T] beta = beta_bar + sigma_beta * z_beta;
  vector[T] b = sigma_b * z_b;
  vector[K] eta = alpha[s] + a[t] + beta[t] .* (m[t] - mu) + b[o] + h * home;
}

model {
  // priors
  mbar ~ normal(pr_mbar_mean, pr_mbar_sd);
  sigma_m ~ exponential(pr_sigma_m_rate);
  m ~ normal(mbar[season_of], sigma_m);
  tau ~ exponential(pr_tau_rate);
  z_mu ~ std_normal();
  alpha ~ normal(0, pr_alpha_sd);
  gamma ~ normal(pr_gamma_mean, pr_gamma_sd);
  sigma_a ~ exponential(pr_sigma_a_rate);
  z_a ~ std_normal();
  beta_bar ~ normal(pr_beta_bar_mean, pr_beta_bar_sd);
  sigma_beta ~ exponential(pr_sigma_beta_rate);
  z_beta ~ std_normal();
  sigma_b ~ exponential(pr_sigma_b_rate);
  z_b ~ std_normal();
  h ~ normal(0, pr_h_sd);
  sigma ~ normal(pr_sigma_mean, pr_sigma_sd);

  // submodel 1: measurement of pace
  dbar ~ normal(mu, se_dbar);
  // submodel 2: outcome
  y ~ normal(eta, sigma ./ sqrt(n));
}
