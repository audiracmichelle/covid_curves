// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // number of obs
  int<lower=0> D_pre; // number of covariates before internvention
  int<lower=0> D_post; // number of covariates after internvention
  int<lower=0> M; // number of counties
  int<lower=0> y[N]; // deaths
  matrix[N, D_pre] X_pre; // covars for pre-trend
  matrix[N, D_post] X_post;  // covars for post-trend, the only one here is days since intervention
  vector[N] offset;  // log of population
  int<lower=0> county_id[N];  // county indicator
  int<lower=0> nchs_id[N];  // nchs indicator
  matrix[N, 3] tpoly_pre; // days since threhsold
  matrix[N, 2] tpoly_post; // days since intervention
}


parameters {
  matrix<lower=-8,upper=8>[M, 3] rand_eff;
  matrix<lower=-8,upper=8>[6, 3] nchs_pre;
  matrix<lower=-8,upper=8>[D_pre, 3] beta_covars_pre;
  row_vector<lower=-8,upper=8>[3] baseline_pre;
  matrix<lower=-8,upper=8>[D_post, 2] beta_covars_post;
  row_vector<lower=-8,upper=8>[2] baseline_post;
  matrix<lower=-8,upper=8>[6, 2] nchs_post;
  real<lower=0.05, upper=20.0> overdisp;
  corr_matrix[3] Omega_rand_eff;
  vector<lower=0.001, upper=12.0>[3] scale_rand_eff;
}

transformed parameters {
  cov_matrix[3] Sigma_rand_eff = quad_form_diag(Omega_rand_eff, scale_rand_eff);

  vector[N] rand_eff_term = rows_dot_product(
    rand_eff[county_id, 1:3],  // random effects unfolded
    tpoly_pre
  );

  vector[N] pre_term = rows_dot_product(
    (
      + X_pre * beta_covars_pre  // interaction with covariates pre-interv
      + rep_matrix(baseline_pre, N)
      + nchs_pre[nchs_id, 1:3]
    ),
    tpoly_pre
  );
  vector[N] post_term = rows_dot_product(
    (
      X_post * beta_covars_post  // interaction with covariates post-interv
      + rep_matrix(baseline_post, N)
      + nchs_post[nchs_id, 1:2]
    ),
    tpoly_post
  );
  vector[N] log_rate_pre_interv = offset + rand_eff_term + pre_term;
  vector[N] log_rate = log_rate_pre_interv + post_term;
}

model {
  // parameter priors
  overdisp ~ exponential(0.1);
  Omega_rand_eff ~ lkj_corr(2.0);
  scale_rand_eff ~ normal(0, 10.0);
  to_vector(beta_covars_pre) ~ normal(0, 5.0);
  to_vector(beta_covars_post) ~ normal(0, 5.0);
  nchs_pre[1,1:3] ~ normal(0, 0.001);
  nchs_post[1,1:2] ~ normal(0, 0.001);
  to_vector(nchs_pre[2:6,1:2]) ~ normal(0, 5.0);
  to_vector(nchs_post[2:6,1:2]) ~ normal(0, 5.0);
  baseline_post ~ normal(0.0, 5.0);
  baseline_pre ~ normal(0.0, 5.0);
  
  // random effect priors (independent)
  for (i in 1:M)
    row(rand_eff, i) ~ multi_normal(rep_vector(0.0, 3), Sigma_rand_eff);

  // likelihood
  y ~ neg_binomial_2(exp(log_rate) + 1e-8, overdisp);
}
