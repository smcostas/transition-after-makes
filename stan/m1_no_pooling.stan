// m1: no pooling across team-seasons (reference model for the shrinkage comparison)
//
// Submodel 1, intention (latent pace after makes):
//   dbar_k ~ Normal(mu_k, s_{s[k]} / sqrt(n_k))     observed mean seconds to first action
//   mu_k   ~ Normal(m_{t[k]}, tau)                   true pace that night
//   m_t    ~ Normal(16, 3)                           usual pace, fixed prior (no pooling)
//
// Submodel 2, outcome:
//   y_k   ~ Normal(eta_k, sigma / sqrt(n_k))         net points per 100
//   eta_k = alpha_{s[k]} + a_{t[k]} + beta_{t[k]} * (m_{t[k]} - mu_k) + b_{o[k]} + h * home_k
//   a_t, b_o: fixed priors, sum to zero within each season (otherwise alpha, a and b are not
//   separately identified); beta_t: fixed prior, no pooling.

data {
  int<lower=1> K;                       // team-games
  int<lower=1> T;                       // team-seasons
  int<lower=1> S;                       // seasons
  int<lower=1> TS;                      // team-seasons per season (30)
  array[K] int<lower=1, upper=T> t;     // offense team-season of k
  array[K] int<lower=1, upper=T> o;     // opponent team-season of k
  array[K] int<lower=1, upper=S> s;     // season of k
  array[T] int<lower=1, upper=S> season_of;   // season of team-season t
  array[T] int<lower=1, upper=TS> slot_of;    // position of t inside its season
  vector<lower=1>[K] n;                 // possessions in k
  vector[K] dbar;                       // observed mean seconds to first action
  vector<lower=0>[S] s_within;          // within-game SD of seconds, pooled by season
  vector[K] y;                          // net points per 100
  vector[K] home;                       // 1 = home

  // priors (R/priors.R and scripts/04_model_no_pooling.R)
  real pr_m_mean;  real<lower=0> pr_m_sd;
  real<lower=0> pr_tau_rate;
  real<lower=0> pr_alpha_sd;
  real<lower=0> pr_a_sd;
  real pr_beta_mean;  real<lower=0> pr_beta_sd;
  real<lower=0> pr_b_sd;
  real<lower=0> pr_h_sd;
  real pr_sigma_mean;  real<lower=0> pr_sigma_sd;
}

transformed data {
  vector[K] se_dbar = s_within[s] ./ sqrt(n);
}

parameters {
  vector[T] m;                               // usual pace
  real<lower=0> tau;                         // game-to-game variation of true pace
  vector[K] z_mu;                            // non-centred true pace
  vector[S] alpha;                           // league average per season
  array[S] sum_to_zero_vector[TS] a_raw;     // level, sums to zero within season
  vector[T] beta;                            // return
  array[S] sum_to_zero_vector[TS] b_raw;     // opponent, sums to zero within season
  real h;                                    // home advantage
  real<lower=0> sigma;                       // SD of one possession's net points x 100
}

transformed parameters {
  vector[K] mu = m[t] + tau * z_mu;
  vector[T] a;
  vector[T] b;
  for (i in 1:T) {
    a[i] = a_raw[season_of[i]][slot_of[i]];
    b[i] = b_raw[season_of[i]][slot_of[i]];
  }
  vector[K] eta = alpha[s] + a[t] + beta[t] .* (m[t] - mu) + b[o] + h * home;
}

model {
  // priors
  m ~ normal(pr_m_mean, pr_m_sd);
  tau ~ exponential(pr_tau_rate);
  z_mu ~ std_normal();
  alpha ~ normal(0, pr_alpha_sd);
  for (j in 1:S) {
    a_raw[j] ~ normal(0, pr_a_sd);
    b_raw[j] ~ normal(0, pr_b_sd);
  }
  beta ~ normal(pr_beta_mean, pr_beta_sd);
  h ~ normal(0, pr_h_sd);
  sigma ~ normal(pr_sigma_mean, pr_sigma_sd);

  // submodel 1: measurement of pace
  dbar ~ normal(mu, se_dbar);
  // submodel 2: outcome
  y ~ normal(eta, sigma ./ sqrt(n));
}
