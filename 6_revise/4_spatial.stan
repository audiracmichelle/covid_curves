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
  // Needed for ICAR prior, it will just penalize edge-wise differneces in the spatial effects
  int<lower=0> N_edges;
  int<lower=1, upper=N> node1[N_edges];  // node1[i] adjacent to node2[i]
  int<lower=1, upper=N> node2[N_edges];  // and node1[i] < node2[i]
  vector<lower=0>[N_edges] edge_weights;
  // Gaussian MRF models are greatly complicated when 
  // there are many connected components of the adjacency graph
  // since the random effects of each component have to add up to zero.
  // for that reason people use a different scaling factor for each connected comp.
  // for example Alaska and Hawaii are their own components.
  int<lower=0> N_comps;
  int cmemb[N];   // indicator of connected component membership
  int csizes[N_comps];   // size of each connected component
  int csorted[M];   // pointer to nodes where they are sorted by component
  int cbrks[N_comps + 1];   // where each component begins and ends in the above pointer
  vector<lower=0>[N_comps] scaling_factor;   // function of the average number of neighbors
}


parameters {
  // spatial effect related
  matrix<lower=-10,upper=10>[M, 3] rand_eff;
  matrix<lower=-10,upper=10>[M, 3] spatial_eff;   // scaled spatial effects
  // ^ note their usually coded as unscaled but their scaled here for compatibility
  vector[N_comps] logit_rho;  // weight assigned to spatial effects
  corr_matrix[3] Omega_rand_eff;
  vector<lower=0.0, upper=20.0>[3] scale_rand_eff;
  // ^ the scale will apply to both spatial and random

  // other params
  matrix<lower=-10,upper=10>[6, 3] nchs_pre;
  matrix<lower=-10,upper=10>[D_pre, 3] beta_covars_pre;
  row_vector<lower=-10,upper=10>[3] baseline_pre;
  matrix<lower=-10,upper=10>[D_post, 2] beta_covars_post;
  row_vector<lower=-10,upper=10>[2] baseline_post;
  matrix<lower=-10,upper=10>[6, 2] nchs_post;
  real<lower=0.025, upper=200.0> overdisp;
  //
}

transformed parameters {
  // covariance matrix cor random effects
  cov_matrix[3] Sigma_rand_eff = quad_form_diag(Omega_rand_eff, scale_rand_eff);
  // proportion explained by spatial effect
  vector[N_comps] rho = inv_logit(logit_rho);
  // heterogeneous effects unrolled
  vector[N] rand_eff_term = rows_dot_product(
    rand_eff[county_id, 1:3], tpoly_pre
  );
  // spatial efects unrolled
  vector[N] spatial_eff_term = rows_dot_product(
    spatial_eff[county_id, 1:3], tpoly_pre
  );
  // comcined effects weighted by the factors
  vector[N] convolved_eff_term = (
    sqrt(1.0 - rho[cmemb]) .* rand_eff_term
    + sqrt(rho[cmemb]) .* scaling_factor[cmemb] .* spatial_eff_term
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
  vector[N] log_rate_pre_interv = offset + convolved_eff_term + pre_term;
  vector[N] log_rate = log_rate_pre_interv + post_term;
}

model {
  // rand effects
  overdisp ~ exponential(0.1);
  Omega_rand_eff ~ lkj_corr(2.0);
  scale_rand_eff ~ normal(0, 10.0);
  logit_rho ~ normal(0, 1);
  // random effect priors (independent)
  for (i in 1:M) {
    row(rand_eff, i) ~ multi_normal(rep_vector(0.0, 3), Sigma_rand_eff);
  }
  // scale_spatial_eff ~ normal(0, 10.0);
  for (j in 1:3) {
    target += -0.5 * dot_self(edge_weights .* (spatial_eff[node1, j] - spatial_eff[node2, j])) / square(scale_rand_eff[j]);
    for (c in 1:N_comps)
      sum(spatial_eff[(cbrks[c] + 1):cbrks[c], j]) ~ normal(0, 0.001 * csizes[c]);

    // add extra regularization; should we?
    // seems to really help convergence
    col(spatial_eff, j) ~ normal(0.0, 100.0);
    col(rand_eff, j) ~ normal(0.0, 100.0);
  }

  // otherparams
  to_vector(beta_covars_pre) ~ normal(0, 10.0);
  to_vector(beta_covars_post) ~ normal(0, 10.0);
  nchs_pre[1,1:3] ~ normal(0, 0.001);
  nchs_post[1,1:2] ~ normal(0, 0.001);
  to_vector(nchs_pre[2:6,1:2]) ~ normal(0, 10.0);
  to_vector(nchs_post[2:6,1:2]) ~ normal(0, 10.0);
  baseline_post ~ normal(0.0, 10.0);
  baseline_pre ~ normal(0.0, 10.0);
  
  // likelihood
  y ~ neg_binomial_2(exp(log_rate) + 1e-8, overdisp);
}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    real rate_ = exp(fmax(fmin(log_rate[n], 12.0), -12.0));
    real overdisp_ = fmin(fmax(overdisp, 0.025), 200.0);
    log_lik[n] = neg_binomial_2_lpmf(y[n] | rate_, overdisp_);
  }
}
