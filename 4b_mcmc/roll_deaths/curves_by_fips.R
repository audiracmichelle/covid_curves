library(tidyverse)
library(progress)
library(feather)
library(rstanarm)
library(gridExtra)
library(cowplot)
library(ggthemes)
library(xtable)
library(lubridate)



curves_by_fips = function(
  model,
  df,
  intervention_offset=0
) {
  # unique list of fips
  fips = unique(df$fips)
  
  # posterior samples from model coefficients
  # samples is matrix of (# samples=1000 x # coefficents)
  samples = as.matrix(model) 
  varnames = dimnames(samples)[[2]]
  
  # pre-compute time polynomials and population var
  # note must call poly on the entire dataset so that
  # t and t^2 is the same as in training (it orthogonalizes)
  # ideally it should be defined in the data prep script
  time_poly = poly(df$days_since_thresh, 2)
  max_t = 60  # max(df$days_since_thresh)
  
  # loop through each fips and compute curves manually
  results = list()
  progress_bar = progress::progress_bar$new(total=length(fips))
  for (f in fips) {
    # data subset
    f_data = filter(df, fips==f)
    f_pop = f_data$pop[1]
    f_nchs = as.integer(f_data$nchs[1])
    f_max_date = f_data$threshold_day + max_t
    
    # extend relevant variables for extra_days
    # ---- time since threshold
    t = 0:max_t  
    poly_t = cbind(1, predict(time_poly, 0:max_t))
    
    # ---- (shifted) days since intervention polynomial
    s = as.numeric(f_data$date - f_data[["stayhome"]])
    s = s - 11 - intervention_offset
    s = min(s):(max_t + min(s))
    poly_int = cbind(s, s^2) * as.numeric(s >= 0)
    
    # overall mean, initialize prediction (main effect) vector
    nms = c("(Intercept)",
            "poly(days_since_thresh, 2)1",
            "poly(days_since_thresh, 2)2")
    mu = samples[ , nms] %*% t(poly_t)
    covar_effect = mu
    
    # nchs effect
    if (f_nchs > 1) {
      nms = paste0(
        c("nchs",
          "poly(days_since_thresh, 2)1:nchs",
          "poly(days_since_thresh, 2)2:nchs"),
        f_nchs
      )
      covar_effect = covar_effect + samples[ ,nms] %*% t(poly_t)
    }
    
    # other covariates
    covariates = c("college", "age_65_plus", "black", "hispanic")
    for (cov in covariates) {
      nms = paste0(
        c("",
          "poly(days_since_thresh, 2)1:",
          "poly(days_since_thresh, 2)2:"),
        cov
      )
      x = f_data[[cov]][1]
      covar_effect = covar_effect + samples[ ,nms] %*% t(poly_t * x) 
    }
    
    # intervention effect
    nms = c("days_since_intrv_stayhome:intrv_stayhome",
            "intrv_stayhome:I(days_since_intrv_stayhome^2)")
    interv_effect = samples[ ,nms] %*% t(poly_int)
    
    # days between intervention interaction
    nms = c("days_since_intrv_stayhome:intrv_stayhome:days_btwn_stayhome_thresh",
            "intrv_stayhome:I(days_since_intrv_stayhome^2):days_btwn_stayhome_thresh")
    days_btwn = f_data$days_btwn_stayhome_thresh[1] + intervention_offset
    interv_effect = interv_effect +
      samples[ ,nms] %*% t(poly_int * days_btwn)
    
    # nchs intervention interaction
    if (f_nchs > 1) {
      nms = paste0(
        "nchs",
        f_nchs,
        c(":days_since_intrv_stayhome:intrv_stayhome",
          ":intrv_stayhome:I(days_since_intrv_stayhome^2)")
      )
      interv_effect = interv_effect + samples[ ,nms] %*% t(poly_int)
    }
    
    # residuals/random effects
    nms = c(
      sprintf("b[(Intercept) fips:%s]", f),
      sprintf("b[poly(days_since_thresh, 2)%s fips:%s]", 1:2, f)
    )
    
    # extract stats
    # 1. days to peak
    # 2. value at peak
    # 3. cum deaths at day 20, 30
    # 4. value in May 1
    
    resid = samples[ ,nms] %*% t(poly_t) 
    lp = covar_effect + interv_effect + resid
    predicted_mean = f_pop * exp(lp)
    
    peak_pos = apply(lp, 1, which.max) - 1
    peak_val = 1e6 * exp(apply(lp, 1, max))
    
    cum_deaths = t(apply(predicted_mean, 1, cumsum))
    dates = f_data$date
    may_1 = which(dates == "2020-05-01")
    day_0 = f_data$date[f_data$days_since_thresh == 0]
    dates_extended = ymd(day_0) + 0:max_t
    days_since_interv = s
    if (dates[1] > "2020-05-01") {
      cum_deaths_may_1 = matrix(0, 1000, 1)
      may_1_t = NA
    } else {
      day_diff = min(max_t, as.integer(ymd("2020-05-01") - ymd(dates[1])))
      cum_deaths_may_1 = cum_deaths[ ,day_diff]
      may_1_t = day_diff
    }
    
    out = list(
      data = f_data,
      f_pop = f_pop,
      dates = dates_extended,
      days_since_interv = days_since_interv,
      days_since_thresh = 0:max_t,
      may_1_t=may_1_t,
      cum_deaths = cum_deaths,
      cum_deaths_may_1 = cum_deaths_may_1,
      intervention_curve = interv_effect,
      random_effect_curve = resid,
      covar_curve = covar_effect,
      linear_predictor = lp,
      disp = samples[ ,"reciprocal_dispersion"],
      peak_pos = peak_pos,
      peak_val = peak_val,
      predicted_mean = predicted_mean
    )
    results[[f]] = out
    progress_bar$tick()
  }
  
  return (results)
}


#' @title NCHS curve summaries
#' @return A list with aggregated curves by nchs 
#' @param extracted_curves The output of the curves_by_fips method
#' summaries
#' @details It returns an curve per nchs for the mean, q50, q05, q95
#' for the predicted mean, cumulative predicted and (log per capita)
#' linear predictor
nchs_effect_summaries = function(extracted_curves) {
  
  summaries = list()
  
  # these function return a time series for each county
  # funs = list(
  #   mean_mean=function(x) apply(x$predicted_mean, 2, mean),
  #   mean_q50=function(x) apply(x$predicted_mean, 2, median),
  #   mean_q95=function(x) apply(x$predicted_mean, 2, quantile, 0.95),
  #   mean_q05=function(x) apply(x$predicted_mean, 2, quantile, 0.05),
  #   mean_q25=function(x) apply(x$predicted_mean, 2, quantile, 0.25),
  #   mean_q75=function(x) apply(x$predicted_mean, 2, quantile, 0.75),
  #   cum_mean=function(x) apply(apply(x$predicted_mean, 1, cumsum), 1, mean),
  #   cum_q50=function(x) apply(apply(x$predicted_mean, 1, cumsum), 1, median),
  #   cum_q95=function(x) apply(apply(x$predicted_mean, 1, cumsum), 1, quantile, 0.95),
  #   cum_q05=function(x) apply(apply(x$predicted_mean, 1, cumsum), 1, quantile, 0.05),
  #   cum_q25=function(x) apply(apply(x$predicted_mean, 1, cumsum), 1, quantile, 0.25),
  #   cum_q75=function(x) apply(apply(x$predicted_mean, 1, cumsum), 1, quantile, 0.75),
  #   lp_mean=function(x) apply(x$linear_predictor, 2, mean),
  #   lp_q50=function(x) apply(x$linear_predictor, 2, median),
  #   lp_q95=function(x) apply(x$linear_predictor, 2, quantile, 0.95),
  #   lp_q05=function(x) apply(x$linear_predictor, 2, quantile, 0.05),
  #   lp_q25=function(x) apply(x$linear_predictor, 2, quantile, 0.25),
  #   lp_q75=function(x) apply(x$linear_predictor, 2, quantile, 0.75)
  # )
  
  # these functions returns scalars for each county
  # funs2 = list(
  #   peak_val_mean=function(x) mean(apply(x$linear_predictor, 1, which.max) - 1),
  #   peak_val_q50=function(x) quantile(apply(x$linear_predictor, 1, which.max) - 1, 0.5),
  #   peak_val_q05=function(x) quantile(apply(x$linear_predictor, 1, which.max) - 1, 0.05),
  #   peak_val_q95=function(x) quantile(apply(x$linear_predictor, 1, which.max) - 1, 0.95),
  #   peak_val_q25=function(x) quantile(apply(x$linear_predictor, 1, which.max) - 1, 0.25),
  #   peak_val_q75=function(x) quantile(apply(x$linear_predictor, 1, which.max) - 1, 0.75),
  #   peak_val_mean=function(x) mean(1e6 * exp(apply(x$linear_predictor, 1, max))),
  #   peak_val_q50=function(x) quantile(1e6 * exp(apply(x$linear_predictor, 1, max)), 0.50),
  #   peak_val_q05=function(x) quantile(1e6 * exp(apply(x$linear_predictor, 1, max)), 0.05),
  #   peak_val_q95=function(x) quantile(1e6 * exp(apply(x$linear_predictor, 1, max)), 0.95),
  #   peak_val_q25=function(x) quantile(1e6 * exp(apply(x$linear_predictor, 1, max)), 0.25),
  #   peak_val_q75=function(x) quantile(1e6 * exp(apply(x$linear_predictor, 1, max)), 0.75)
  # )
  
  # funs2 = list(
  #   peak_val_mean=function(x) mean(x$peak_pos),
  #   peak_val_sd=function(x) sd(x$peak_pos),
  #   peak_val_q50=function(x) quantile(x$peak_pos, 0.5),
  #   peak_val_q05=function(x) quantile(x$peak_pos, 0.05),
  #   peak_val_q95=function(x) quantile(x$peak_pos, 0.95),
  #   peak_val_q25=function(x) quantile(x$peak_pos, 0.25),
  #   peak_val_q75=function(x) quantile(x$peak_pos, 0.75),
  #   peak_val_iqr=function(x) quantile(x$peak_pos, 0.75) - quantile(x$peak_pos, 0.25),
  #   peak_val_mean=function(x) mean(x$peak_val),
  #   peak_val_sd=function(x) sd(x$peak_val),
  #   peak_val_q50=function(x) quantile(x$peak_val, 0.5),
  #   peak_val_q05=function(x) quantile(x$peak_val, 0.05),
  #   peak_val_q95=function(x) quantile(x$peak_val, 0.95),
  #   peak_val_q25=function(x) quantile(x$peak_val, 0.25),
  #   peak_val_q75=function(x) quantile(x$peak_val, 0.75),
  #   peak_val_qiqr=function(x) quantile(x$peak_val, 0.75) - quantile(x$peak_val, 0.25)
  # )
  
  # Answer time to peak and peak value and plot
  # Note: [ ,-1] to remove nchs
  # N = table(distinct(select(df, fips, nchs))$nchs)
  #
  # # apply function to each extracted curve
  # nchs = map_chr(extracted_curves, ~ .x$data$nchs[1])
  # max_t = ncol(extracted_curves[[1]]$predicted_mean) - 1
  #
  # summaries = list()
  # progress_bar = progress::progress_bar$new(
  #   total=length(funs) + length(funs2)
  # )
  #
  # for (i in seq_along(funs)) {
  #   fname = names(funs)[i]
  #   f = funs[[i]]
  #   summaries[[fname]] = map(extracted_curves, f) %>%
  #     bind_rows %>%
  #     t %>%
  #     as_tibble() %>%
  #     `names<-`(sprintf("t%02d", 0:max_t)) %>%
  #     mutate(nchs=nchs) %>%
  #     group_by(nchs) %>%
  #     summarise_all(median)
  #   progress_bar$tick()
  # }
  #
  # for (i in seq_along(funs2)) {
  #   fname = names(funs2)[i]
  #   f = funs2[[i]]
  #
  #   summaries[[paste0(fname, "_median")]] = tibble(
  #     nchs=nchs,
  #     var=map_dbl(extracted_curves, f)
  #   ) %>%
  #     group_by(nchs) %>%
  #     summarise_all(median) %>%
  #     pull(var)
  #
  #   summaries[[paste0(fname, "_iqr")]] = tibble(
  #     nchs=nchs,
  #     var=map_dbl(extracted_curves, f)
  #   ) %>%
  #     group_by(nchs) %>%
  #     summarise_all(function(x) quantile(x, 0.75) - quantile(x, 0.25)) %>%
  #     pull(var)
  #
  #
  #   summaries[[paste0(fname, "_mean")]] = tibble(
  #     nchs=nchs,
  #     var=map_dbl(extracted_curves, f)
  #   ) %>%
  #     group_by(nchs) %>%
  #     summarise_all(mean) %>%
  #     pull(var)
  #
  #
  #   summaries[[paste0(fname, "_sd")]] = tibble(
  #     nchs=nchs,
  #     var=map_dbl(extracted_curves, f)
  #   ) %>%
  #     group_by(nchs) %>%
  #     summarise_all(sd) %>%
  #     pull(var)
  #
  #   progress_bar$tick()
  # }
  
  # Strategy 2: Aggregate lp curves per sample
  
  nchs = map_chr(extracted_curves, ~ .x$data$nchs[1])
  max_t = 60   # ncol(extracted_curves[[1]]$predicted_mean) - 1
  
  agg_lp = array(0, c(1000, max_t + 1, 6))
  agg_cumdeaths = array(0, c(1000, max_t + 1, 6))
  agg_cumdeaths_may_1 = array(0, c(1000, 6))
  nchs_count = table(nchs)
  total_pop = numeric(6)
  total_pop_may1 = numeric(6)
  
  for (i in seq_along(nchs)) {
    k = as.integer(nchs[i])
    n = nchs_count[k]
    lp = extracted_curves[[i]]$linear_predictor
    
    agg_lp[ , , k] = agg_lp[ , , k] + lp / n
    
    f_pop = extracted_curves[[i]]$f_pop
    cumdeaths = extracted_curves[[i]]$cum_deaths
    
    agg_cumdeaths[ , , k] = agg_cumdeaths[ , , k] + cumdeaths
    total_pop[k] = total_pop[k] + f_pop
    
    may_1_t = extracted_curves[[i]]$may_1_t
    if (!is.na(may_1_t)) {
      cumdeaths_may_1 = cumdeaths[ ,may_1_t]
      agg_cumdeaths_may_1[ , k] = agg_cumdeaths_may_1[ , k] + cumdeaths_may_1
      total_pop_may1[k] = total_pop_may1[k] + f_pop
    }
  }
  
  summaries$agg_lp = agg_lp
  summaries$agg_cumdeaths = agg_cumdeaths
  summaries$agg_cumdeaths_may_1 = agg_cumdeaths_may_1
  
  # aggregate totals
  summaries$agg_lp_total = matrix(0, 1000, max_t + 1)
  summaries$agg_cumdeaths_total = matrix(0, 1000, max_t + 1)
  summaries$agg_cumdeaths_may_1_total = numeric(1000)
  
  for (i in 1:6) {
    summaries$agg_lp_total = summaries$agg_lp_total + summaries$agg_lp[ , , i]
    summaries$agg_cumdeaths_total = (
      summaries$agg_cumdeaths_total + summaries$agg_cumdeaths[ , , i]
    )
    summaries$agg_cumdeaths_may_1_total = (
      summaries$agg_cumdeaths_may_1_total + summaries$agg_cumdeaths_may_1[ , i]
    )
  }
  
  return (summaries)
}

