library(tidyverse)
library(progress)
library(feather)
library(rstanarm)
library(gridExtra)
library(cowplot)
library(ggthemes)
library(xtable)


#' @title Curves from Stan model coefficients
#' @return A list indexed by fips with summaries
#' @details The returned list splits the fitted curve
#' into three components:
#' (1) covariate_effect
#' (2) intervention_effect (including interactions)
#' (3) random effect
#' These "effects" are in logarithmic space
#' The function also returns two aggregated curves
#' (4) linear_predictor (addition from previous)
#' (5) predicted mean (exponentiated linear predictor)
#' times the population,
#' Finally, it returns a lits of summaries with additional
#' information
curves_by_fips = function(
  model,
  df,
  intervention_offset=0
) {
  # unique list of fips
  fips = unique(df$fips)
  
  # posterior samples from model coefficients
  samples = as.matrix(model) 
  varnames = dimnames(samples)[[2]]
  
  # pre-compute time polynomials and population var
  # note must call poly on the entire dataset so that
  # t and t^2 is the same as in training (it orthogonalizes)
  # ideally it should be defined in the data prep script
  time_poly = poly(df$days_since_thresh, 2)
  max_t = max(df$days_since_thresh)
  
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
    s = as.numeric(
      f_data$date - f_data[["decrease_50_total_visiting"]]
    )
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
    nms = c("days_since_intrv_decrease:intrv_decrease",
            "intrv_decrease:I(days_since_intrv_decrease^2)")
    interv_effect = samples[ ,nms] %*% t(poly_int)
    
    # days between intervention interaction
    nms = c("days_since_intrv_decrease:intrv_decrease:days_btwn_decrease_thresh",
            "intrv_decrease:I(days_since_intrv_decrease^2):days_btwn_decrease_thresh")
    days_btwn = f_data$days_btwn_decrease_thresh[1] + intervention_offset
    interv_effect = interv_effect +
      samples[ ,nms] %*% t(poly_int * days_btwn)
    
    # nchs intervention interaction
    if (f_nchs > 1) {
      nms = paste0(
        "nchs",
        f_nchs,
        c(":days_since_intrv_decrease:intrv_decrease",
          ":intrv_decrease:I(days_since_intrv_decrease^2)")
      )
      interv_effect = interv_effect + samples[ ,nms] %*% t(poly_int)
    }
    
    # residuals/random effects
    nms = c(
      sprintf("b[(Intercept) fips:%s]", f),
      sprintf("b[poly(days_since_thresh, 2)%s fips:%s]", 1:2, f)
    )
    
    resid = samples[ ,nms] %*% t(poly_t) 
    
    out = list(
      data = f_data,
      intervention_curve = interv_effect,
      random_effect_curve = resid,
      covar_curve = covar_effect,
      linear_predictor = covar_effect + interv_effect + resid,
      disp = samples[ ,"reciprocal_dispersion"]
    )
    out$predicted_mean = f_pop * exp(out$linear_predictor)
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
  # list of function to be applied
  funs = list(
    mean_mean=function(x) apply(x$predicted_mean, 2, mean),
    mean_q50=function(x) apply(x$predicted_mean, 2, median),
    mean_q95=function(x) apply(x$predicted_mean, 2, quantile, 0.95),
    mean_q05=function(x) apply(x$predicted_mean, 2, quantile, 0.05),
    mean_q25=function(x) apply(x$predicted_mean, 2, quantile, 0.25),
    mean_q75=function(x) apply(x$predicted_mean, 2, quantile, 0.75),
    cum_mean=function(x) apply(apply(x$predicted_mean, 1, cumsum), 1, mean),
    cum_q50=function(x) apply(apply(x$predicted_mean, 1, cumsum), 1, median),
    cum_q95=function(x) apply(apply(x$predicted_mean, 1, cumsum), 1, quantile, 0.95),
    cum_q05=function(x) apply(apply(x$predicted_mean, 1, cumsum), 1, quantile, 0.05),
    cum_q25=function(x) apply(apply(x$predicted_mean, 1, cumsum), 1, quantile, 0.25),
    cum_q75=function(x) apply(apply(x$predicted_mean, 1, cumsum), 1, quantile, 0.75),
    lp_mean=function(x) apply(x$linear_predictor, 2, mean),
    lp_q50=function(x) apply(x$linear_predictor, 2, median),
    lp_q95=function(x) apply(x$linear_predictor, 2, quantile, 0.95),
    lp_q05=function(x) apply(x$linear_predictor, 2, quantile, 0.05),
    lp_q25=function(x) apply(x$linear_predictor, 2, quantile, 0.25),
    lp_q75=function(x) apply(x$linear_predictor, 2, quantile, 0.75)
  )
  
  # apply function to each extracted curve
  nchs = map_chr(extracted_curves, ~ .x$data$nchs[1])
  max_t = ncol(extracted_curves[[1]]$predicted_mean) - 1
  
  summaries = list()
  progress_bar = progress::progress_bar$new(total=length(funs))
  for (i in seq_along(funs)) {
    fname = names(funs)[i]
    f = funs[[i]]
    summaries[[fname]] = map(extracted_curves, f) %>% 
      bind_rows %>% 
      t %>% 
      as_tibble() %>% 
      `names<-`(sprintf("t%02d", 0:max_t)) %>% 
      mutate(nchs=nchs) %>% 
      group_by(nchs) %>% 
      summarise_all(mean) 
    progress_bar$tick()
  }
  
  return (summaries)
}

main = function() {
    # Read data
    model = readRDS("./model.rds")
    county_fit = readRDS("./county_fit.rds")
    source("../../plot_foo.R")
    df = read_feather("../../county_train_decrease.feather")
    
    # === actual intervention ===
    
    # Summary for intervention as observed
    cat("computing curve summaries by fips no offset...\n")
    curve_summaries = curves_by_fips(model, df)
    cat("summarizing by nchs...\n")
    nchs_summaries = nchs_effect_summaries(curve_summaries)
    
    # Answer time to peak and peak value and plot
    # Note: [ ,-1] to remove nchs
    N = table(distinct(select(df, fips, nchs))$nchs)
    peaks_pos = apply(nchs_summaries$lp_mean[ ,-1], 1, which.max) - 1
    # in terms of 100,000 people
    peaks_val = apply(1e6 * exp(nchs_summaries$lp_mean[ ,-1]), 1, max)
    peak_flatness = c()  # zero is very flat
    for (i in 1:6) {
      idx = peaks_pos[i] + 1
      x = as.numeric(nchs_summaries$lp_mean[i, -1])
      peak_nbrs = x[(idx - 1):(idx + 1)]
      # flatness is negative second derivative
      peak_flatness[i] = - (sum(peak_nbrs) - 3 * peak_nbrs[2])
    }
    
    # Per capita plot per nchs
    q50 = nchs_summaries$lp_q50 %>% 
      gather(t, median, -nchs) %>% 
      mutate(t=as.integer(substring(t, 2)))
    q95 = nchs_summaries$lp_q95 %>% 
      gather(t, q95, -nchs) %>%
      mutate(t=as.integer(substring(t, 2)))
    q05 = nchs_summaries$lp_q05 %>% 
      gather(t, q05, -nchs) %>%  
      mutate(t=as.integer(substring(t, 2)))
    plotdata = reduce(
      list(q50, q95, q05), left_join, by=c("nchs", "t")
    ) %>% 
      filter(t <= 30)
    
    # === late intervention ===
    up = 7
    
    # Summary for intervention as observed
    cat("computing curve summaries by fips late intervention...\n")
    curve_summaries = curves_by_fips(
      model,
      df,
      intervention_offset=up
    )
    cat("summarizing by nchs...\n")
    nchs_summaries = nchs_effect_summaries(curve_summaries)
    
    # Answer time to peak and peak value and plot
    # Note: [ ,-1] to remove nchs
    N = table(distinct(select(df, fips, nchs))$nchs)
    peaks_pos_late = apply(nchs_summaries$lp_mean[ ,-1], 1, which.max) - 1
    # in terms of 100,000 people
    peaks_val_late = apply(1e6 * exp(nchs_summaries$lp_mean[ ,-1]), 1, max)
    peak_flatness_late = c()  # zero is very flat
    for (i in 1:6) {
      idx = peaks_pos_late[i] + 1
      x = as.numeric(nchs_summaries$lp_mean[i, -1])
      peak_nbrs = x[(idx - 1):(idx + 1)]
      # flatness is negative second derivative
      peak_flatness_late[i] = - (sum(peak_nbrs) - 3 * peak_nbrs[2])
    }
    
    # Per capita plot per nchs
    q50_late = nchs_summaries$lp_q50 %>% 
      gather(t, median, -nchs) %>% 
      mutate(t=as.integer(substring(t, 2)))
    q95_late = nchs_summaries$lp_q95 %>% 
      gather(t, q95, -nchs) %>%
      mutate(t=as.integer(substring(t, 2)))
    q05_late = nchs_summaries$lp_q05 %>% 
      gather(t, q05, -nchs) %>%  
      mutate(t=as.integer(substring(t, 2)))
    
    plotdata_late = reduce(
      list(q50_late, q95_late, q05_late), left_join, by=c("nchs", "t")
    ) %>% 
      filter(t <= 30)
    
    # === early intervention ===
    down = -14
    
    # Summary for intervention as observed
    cat("computing curve summaries by fips early intervention...\n")
    curve_summaries = curves_by_fips(
      model,
      df,
      intervention_offset=down
    )
    cat("summarizing by nchs...\n")
    nchs_summaries = nchs_effect_summaries(curve_summaries)
    
    # Answer time to peak and peak value and plot
    # Note: [ ,-1] to remove nchs
    N = table(distinct(select(df, fips, nchs))$nchs)
    peaks_pos_early = apply(nchs_summaries$lp_mean[ ,-1], 1, which.max) - 1
    # in terms of 100,000 people
    peaks_val_early = apply(1e6 * exp(nchs_summaries$lp_mean[ ,-1]), 1, max)
    peak_flatness_early = c()  # zero is very flat
    for (i in 1:6) {
      idx = peaks_pos_early[i] + 1
      x = as.numeric(nchs_summaries$lp_mean[i, -1])
      peak_nbrs = x[(idx - 1):(idx + 1)]
      # flatness is negative second derivative
      peak_flatness_early[i] = - (sum(peak_nbrs) - 3 * peak_nbrs[2])
    }
    
    # Per capita plot per nchs
    q50_early = nchs_summaries$lp_q50 %>% 
      gather(t, median, -nchs) %>% 
      mutate(t=as.integer(substring(t, 2)))
    q95_early = nchs_summaries$lp_q95 %>% 
      gather(t, q95, -nchs) %>%
      mutate(t=as.integer(substring(t, 2)))
    q05_early = nchs_summaries$lp_q05 %>% 
      gather(t, q05, -nchs) %>%  
      mutate(t=as.integer(substring(t, 2)))
    
    plotdata_early = reduce(
      list(q50_early, q95_early, q05_early), left_join, by=c("nchs", "t")
    ) %>% 
      filter(t <= 30)
    
    # plots ===========================
    
    plotdata_all = bind_rows(
      mutate(plotdata, timing="actual"),
      mutate(plotdata_late, timing="late"),
      mutate(plotdata_early, timing="early")
    ) %>% 
      rename(NCHS=nchs) %>% 
      mutate(
        median=median/log(10) + 6,
        q05=pmin(pmax(q05/log(10) + 6, -2), 1),
        q95=pmin(pmax(q95/log(10) + 6, -2), 1)
      )
    
    ggplot(plotdata_all) +
      geom_line(aes(x=t, y=median, color=timing), size=1.0) +
      geom_ribbon(
        aes(x=t, ymin=q05, ymax=q95, fill=timing),
        alpha=0.4
      ) +
      facet_wrap(~ NCHS, labeller=label_both) +
      labs(fill="Timing",
           x="Days since threhsold",
           y="Daily deaths per 1 million") +
      guides(color=FALSE, fill=FALSE) +
      scale_y_continuous(
        limits=c(-2, 1.1),
        breaks=c(-2, -1, 0, 1),
        labels=c("0.01", "0.1", "0", "10")
      ) +
      theme_minimal_hgrid() +
      scale_color_manual(values=c("blue", "green", "orange")) +
      scale_fill_manual(values=c("blue", "green", "orange")) +
      theme(legend.position = "top")
  
  ggsave(
    "img/nchs_timing_decrease.pdf",
    width=16,
    height=8,
    units="cm"
  )
  
  table_peaks_pos = tibble(
    NCHS=1:6,
    Early=peaks_pos_early,
    Actual=peaks_pos,
    Late=peaks_pos_late
  )
  
  cat("Table peak days since treshhold\n")
  print(xtable(table_peaks_pos, include.rownames=FALSE))
  
  table_peaks_val = tibble(
    NCHS=1:6,
    Early=peaks_val_early,
    Actual=peaks_val,
    Late=peaks_val_late
  )
  
  cat("Table vaue at peak\n")
  print(xtable(table_peaks_val, include.rownames=FALSE))
  
}

main()

