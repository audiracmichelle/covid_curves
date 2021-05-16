library(tidyverse)
library(rstan)
library(reticulate)
np = import("numpy")
scipy_stats = import("scipy.stats")
nbinom = scipy_stats$nbinom

stan_input_data = function(county_train, type=c("stayhome", "decrease")) {
  county_train = county_train %>%
    mutate(fips_f = as.factor(fips)) %>% 
    mutate(fips_id = as.integer(fips_f))
  
  tscale = 10.0
  t1 = county_train$days_since_thresh / tscale
  t1_2 = t1^2
  tpoly_pre = cbind(1, t1, t1_2)
  
  if (type[1] == "stayhome") {
    days_since_intrv = county_train$days_since_intrv_stayhome
    days_btwn = county_train$days_btwn_stayhome_thresh
  } else {
    days_since_intrv = county_train$days_since_intrv_decrease
    days_btwn = county_train$days_btwn_decrease_thresh
  }

  t2_0 = days_since_intrv / tscale
  t2_0[is.na(t2_0)] = 0
  t2 = pmax(t2_0, 0) / tscale
  t2_2 =  t2^2
  tpoly_post = cbind(t2, t2_2)
  
  fips = levels(county_train$fips_f)
  y = county_train$y
  offset = log(county_train$pop) - mean(log(county_train$pop)) - mean(log(1 + y))
  X_pre = as.matrix(select(county_train, college, age_65_plus, black, hispanic))
  X_post = matrix(days_btwn, ncol=1) / tscale
  X_post[is.na(X_post)] = 0.0
  
  mu_X_pre = apply(X_pre, 2, mean)
  sd_X_pre = apply(X_pre, 2, sd)
  for (j in 1:ncol(X_pre))
    X_pre[ ,j] = (X_pre[ ,j] - mu_X_pre[j]) / sd_X_pre[j]
  mu = mu_X_pre
  sig = sd_X_pre
  varname = names(mu_X_pre)
  normalizing_stats = tibble(variable=varname, mean=mu, sd=sig)
  
  # mu_X_post = apply(X_post, 2, mean)
  # sd_X_post = apply(X_post, 2, sd)
  # for (j in 1:ncol(X_post))
  #   X_post[ ,j] = (X_post[ ,j] - mu_X_post[j]) / sd_X_post[j]
  # 
  # mu = c(mu_X_pre, mu_X_post)
  # sig = c(sd_X_pre, sd_X_post)
  # varname = c(names(mu_X_pre), names(mu_X_post))
  # normalizing_stats = tibble(variable=varname, mean=mu, sd=sig)
  # write_csv(normalizing_stats, "normalizing_stats.csv")
  
  list(
    N = nrow(county_train),
    D_pre = 4,
    D_post = 1,
    M = length(fips),
    X_pre = X_pre,
    X_post = X_post,
    offset = offset,
    tpoly_pre = tpoly_pre,
    tpoly_post = tpoly_post,
    y = y,
    nchs_id = as.integer(county_train$nchs),
    county_id = county_train$fips_id,
    normalizing_stats=normalizing_stats
  )
}


my_posterior_predict = function (fit, new_df) {
  new_data = stan_input_data(new_df)
  parnames = c(
    "nchs_pre", "nchs_post", "beta_covars_pre",
    "beta_covars_post", "beta_covars_post",
    "baseline_pre", "baseline_post",
    "overdisp", "rand_eff",
    "Omega_rand_eff", "scale_rand_eff",
    "log_rate_pre_interv"
  )
  pars = rstan::extract(fit, pars=parnames)
  N = length(new_data$y)
  nsamples = nrow(pars$nchs_pre)
  
  # 
  # # I will use numpy for this bloody horrible options.
  tpoly_pre = np$expand_dims(new_data$tpoly_pre, 0L)
  rand_eff_unrolled = np$array(pars$rand_eff[ ,new_data$county_id, ])
  rand_eff_term = np$sum(
    np$multiply(tpoly_pre, rand_eff_unrolled), -1L
  )
  # 
  # X_pre = new_data$X_pre
  # X_pre = np$expand_dims(X_pre, 0L)
  # X_pre = np$expand_dims(X_pre, 3L)
  # beta_covars_pre = np$expand_dims(np$array(pars$beta_covars_pre), 1L)
  # covar_baseline_pre = np$sum(np$multiply(X_pre, beta_covars_pre), -2L)
  # nchs_pre_unrolled = np$array(pars$nchs_pre[ ,new_data$nchs_id, ])
  # baseline_pre = np$expand_dims(pars$baseline_pre, 1L)
  # pre_term = np$add(np$add(baseline_pre, nchs_pre_unrolled), covar_baseline_pre)
  # pre_term = np$sum(np$multiply(pre_term, tpoly_pre), -1L)
  
  tpoly_post = np$expand_dims(new_data$tpoly_post, 0L)
  X_post = new_data$X_post
  X_post = np$expand_dims(X_post, 0L)
  X_post = np$expand_dims(X_post, 3L)
  beta_covars_post = np$expand_dims(np$array(pars$beta_covars_post), 1L)
  covar_baseline_post = np$sum(np$multiply(X_post, beta_covars_post), -2L)
  nchs_post_unrolled = np$array(pars$nchs_post[ ,new_data$nchs_id, ])
  baseline_post = np$expand_dims(pars$baseline_post, 1L)
  post_term = np$add(
    np$add(baseline_post, nchs_post_unrolled),
    covar_baseline_post
  )
  post_term = np$sum(
    np$multiply(post_term, tpoly_post), -1L
  )
  post_term = as.array(post_term)
  
  pre_term = pars$log_rate_pre_interv
  log_rate = pre_term + post_term
  rate = exp(log_rate)
  overdisp = matrix(pars$overdisp, nrow=nsamples, ncol=N)
  var = rate + rate ^ 2 / overdisp
  p = pmax((var - rate) / var, 1e-6)
  nbinom_samples = as.array(nbinom$rvs(overdisp, 1 - p))
  #
  list(
    yhat=rate,
    log_yhat=log_rate,
    log_yhat_no_rand_eff=log_rate - rand_eff_term,
    rand_eff_term=rand_eff_term,
    pre_term=pre_term,
    post_term=post_term,
    y_samples=nbinom_samples
  )
}
