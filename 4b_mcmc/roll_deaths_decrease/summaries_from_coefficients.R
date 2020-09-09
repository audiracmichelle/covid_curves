library(tidyverse)
library(lubridate)
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
  max_t = 60 # max(df$days_since_thresh)
  
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
    lp = covar_effect + interv_effect + resid
    predicted_mean = f_pop * exp(lp)
    peak_pos = apply(lp, 1, which.max) - 1
    peak_val = 1e6 * exp(apply(lp, 1, max))
    
    
    cum_deaths = t(apply(predicted_mean, 1, cumsum))
    dates = f_data$date
    may_1 = which(dates == "2020-05-01")
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
  # list of function to be applied
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
  # 
  # apply function to each extracted curve
  nchs = map_chr(extracted_curves, ~ .x$data$nchs[1])
  max_t = 60 #  ncol(extracted_curves[[1]]$predicted_mean) - 1
  
  summaries = list()
  # progress_bar = progress::progress_bar$new(total=length(funs))
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
  #     summarise_all(mean) 
  #   progress_bar$tick()
  # }
  # 
  # Strategy 2: Aggregate lp curves per sample
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

differences_summaries = function(curve_summary_list, df) {

  max_t = 60 # max(df$days_since_thresh)
  
  agg_cumdeaths_late_minus_actual = array(0, c(1000, max_t + 1, 6))
  agg_cumdeaths_actual_minus_early = array(0, c(1000, max_t + 1, 6))
  agg_cumdeaths_may_1_late_minus_actual = array(0, c(1000, 6))
  agg_cumdeaths_may_1_actual_minus_early = array(0, c(1000, 6))

  nchs = map_chr(curve_summary_list$actual, ~ .x$data$nchs[1])
  nchs_count = table(nchs)
  total_pop = numeric(6)
  total_pop_may1 = numeric(6)
  
  summaries = list()
  
  for (i in seq_along(nchs)) {
    k = as.integer(nchs[i])
    n = nchs_count[k]
    cd_actual = curve_summary_list$actual[[i]]$cum_deaths
    cd_late = curve_summary_list$late[[i]]$cum_deaths
    cd_early = curve_summary_list$early[[i]]$cum_deaths
 
    f_pop = curve_summary_list$actual[[i]]$f_pop
    
    agg_cumdeaths_late_minus_actual[ , , k] = (
      agg_cumdeaths_late_minus_actual[ , , k] + (cd_late - cd_actual)
    )
    agg_cumdeaths_actual_minus_early[ , , k] = (
      agg_cumdeaths_actual_minus_early[ , , k] + (cd_actual - cd_early)
    )
    total_pop[k] = total_pop[k] + f_pop
    
    may_1_t = curve_summary_list$actual[[i]]$may_1_t
    if (!is.na(may_1_t)) {
      agg_cumdeaths_may_1_late_minus_actual[ , k] = (
        agg_cumdeaths_may_1_late_minus_actual[ , k] + (cd_late[ ,may_1_t] - cd_actual[ ,may_1_t])
      )
      agg_cumdeaths_may_1_actual_minus_early[ , k] = (
        agg_cumdeaths_may_1_actual_minus_early[ , k] +(cd_actual[ ,may_1_t] - cd_early[ ,may_1_t])
      )
      total_pop_may1[k] = total_pop_may1[k] + f_pop
    }
  }
  
  summaries$agg_cumdeaths_late_minus_actual = agg_cumdeaths_late_minus_actual
  summaries$agg_cumdeaths_actual_minus_early = agg_cumdeaths_actual_minus_early
  summaries$agg_cumdeaths_may_1_late_minus_actual = agg_cumdeaths_may_1_late_minus_actual
  summaries$agg_cumdeaths_may_1_actual_minus_early = agg_cumdeaths_may_1_actual_minus_early
  
  
  # aggregate totals
  summaries$agg_cumdeaths_late_minus_actual_total = matrix(0, 1000, max_t + 1)
  summaries$agg_cumdeaths_actual_minus_early_total = matrix(0, 1000, max_t + 1)
  summaries$agg_cumdeaths_may_1_late_minus_actual_total = numeric(1000)
  summaries$agg_cumdeaths_may_1_actual_minus_early_total = numeric(1000)
  
  for (i in 1:6) {
    summaries$agg_cumdeaths_late_minus_actual_total = (
      summaries$agg_cumdeaths_late_minus_actual_total + agg_cumdeaths_late_minus_actual[ , , i]
    )
    summaries$agg_cumdeaths_actual_minus_early_total = (
      summaries$agg_cumdeaths_actual_minus_early_total + agg_cumdeaths_actual_minus_early[ , , i]
    )
    summaries$agg_cumdeaths_may_1_late_minus_actual_total = (
      summaries$agg_cumdeaths_may_1_late_minus_actual_total + agg_cumdeaths_may_1_late_minus_actual[ , i]
    )
    summaries$agg_cumdeaths_may_1_actual_minus_early_total = (
      summaries$agg_cumdeaths_may_1_actual_minus_early_total + agg_cumdeaths_may_1_actual_minus_early[ , i]
    )
  }

  return (summaries)
}

# Read data
model = readRDS("./model.rds")
county_fit = readRDS("./county_fit.rds")
source("../../plot_foo.R")
df = read_feather("../../county_train_decrease.feather")

# === actual intervention ===
curve_summary_list = list()
nchs_summary_list = list()

# Summary for intervention as observed
cat("computing curve summaries by fips no offset...\n")
curve_summaries = curves_by_fips(model, df)
cat("summarizing by nchs...\n")
nchs_summaries = nchs_effect_summaries(curve_summaries)
nchs_summary_list[["actual"]] = nchs_summaries
curve_summary_list[["actual"]] = curve_summaries

# Answer time to peak and peak value and plot
# Note: [ ,-1] to remove nchs
# N = table(distinct(select(df, fips, nchs))$nchs)
# peaks_pos = apply(nchs_summaries$lp_mean[ ,-1], 1, which.max) - 1
# # in terms of 100,000 people
# peaks_val = apply(1e6 * exp(nchs_summaries$lp_mean[ ,-1]), 1, max)
# peak_flatness = c()  # zero is very flat
# for (i in 1:6) {
#   idx = peaks_pos[i] + 1
#   x = as.numeric(nchs_summaries$lp_mean[i, -1])
#   peak_nbrs = x[(idx - 1):(idx + 1)]
#   # flatness is negative second derivative
#   peak_flatness[i] = - (sum(peak_nbrs) - 3 * peak_nbrs[2])
# }

# Per capita plot per nchs
# q50 = nchs_summaries$lp_q50 %>% 
#   gather(t, median, -nchs) %>% 
#   mutate(t=as.integer(substring(t, 2)))
# q95 = nchs_summaries$lp_q95 %>% 
#   gather(t, q95, -nchs) %>%
#   mutate(t=as.integer(substring(t, 2)))
# q05 = nchs_summaries$lp_q05 %>% 
#   gather(t, q05, -nchs) %>%  
#   mutate(t=as.integer(substring(t, 2)))
# plotdata = reduce(
#   list(q50, q95, q05), left_join, by=c("nchs", "t")
# ) %>% 
#   filter(t <= 30)

# === late intervention ===
up = 10

# Summary for intervention as observed
cat("computing curve summaries by fips late intervention...\n")
curve_summaries = curves_by_fips(
  model,
  df,
  intervention_offset=up
)
curve_summary_list[["late"]] = curve_summaries
cat("summarizing by nchs...\n")
nchs_summaries = nchs_effect_summaries(curve_summaries)
nchs_summary_list[["late"]] = nchs_summaries

# Answer time to peak and peak value and plot
# Note: [ ,-1] to remove nchs
# N = table(distinct(select(df, fips, nchs))$nchs)
# peaks_pos_late = apply(nchs_summaries$lp_mean[ ,-1], 1, which.max) - 1
# # in terms of 100,000 people
# peaks_val_late = apply(1e6 * exp(nchs_summaries$lp_mean[ ,-1]), 1, max)
# peak_flatness_late = c()  # zero is very flat
# for (i in 1:6) {
#   idx = peaks_pos_late[i] + 1
#   x = as.numeric(nchs_summaries$lp_mean[i, -1])
#   peak_nbrs = x[(idx - 1):(idx + 1)]
#   # flatness is negative second derivative
#   peak_flatness_late[i] = - (sum(peak_nbrs) - 3 * peak_nbrs[2])
# }

# Per capita plot per nchs
# q50_late = nchs_summaries$lp_q50 %>% 
#   gather(t, median, -nchs) %>% 
#   mutate(t=as.integer(substring(t, 2)))
# q95_late = nchs_summaries$lp_q95 %>% 
#   gather(t, q95, -nchs) %>%
#   mutate(t=as.integer(substring(t, 2)))
# q05_late = nchs_summaries$lp_q05 %>% 
#   gather(t, q05, -nchs) %>%  
#   mutate(t=as.integer(substring(t, 2)))
# 
# plotdata_late = reduce(
#   list(q50_late, q95_late, q05_late), left_join, by=c("nchs", "t")
# ) %>% 
#   filter(t <= 30)

# === early intervention ===
down = -10

# Summary for intervention as observed
cat("computing curve summaries by fips early intervention...\n")
curve_summaries = curves_by_fips(
  model,
  df,
  intervention_offset=down
)
curve_summary_list[["early"]] = curve_summaries
cat("summarizing by nchs...\n")
nchs_summaries = nchs_effect_summaries(curve_summaries)
nchs_summary_list[["early"]] = nchs_summaries

# Answer time to peak and peak value and plot
# Note: [ ,-1] to remove nchs
# N = table(distinct(select(df, fips, nchs))$nchs)
# peaks_pos_early = apply(nchs_summaries$lp_mean[ ,-1], 1, which.max) - 1
# # in terms of 100,000 people
# peaks_val_early = apply(1e6 * exp(nchs_summaries$lp_mean[ ,-1]), 1, max)
# peak_flatness_early = c()  # zero is very flat
# for (i in 1:6) {
#   idx = peaks_pos_early[i] + 1
#   x = as.numeric(nchs_summaries$lp_mean[i, -1])
#   peak_nbrs = x[(idx - 1):(idx + 1)]
#   # flatness is negative second derivative
#   peak_flatness_early[i] = - (sum(peak_nbrs) - 3 * peak_nbrs[2])
# }
# 
# # Per capita plot per nchs
# q50_early = nchs_summaries$lp_q50 %>% 
#   gather(t, median, -nchs) %>% 
#   mutate(t=as.integer(substring(t, 2)))
# q95_early = nchs_summaries$lp_q95 %>% 
#   gather(t, q95, -nchs) %>%
#   mutate(t=as.integer(substring(t, 2)))
# q05_early = nchs_summaries$lp_q05 %>% 
#   gather(t, q05, -nchs) %>%  
#   mutate(t=as.integer(substring(t, 2)))
# 
# plotdata_early = reduce(
#   list(q50_early, q95_early, q05_early), left_join, by=c("nchs", "t")
# ) %>% 
#   filter(t <= 30)

# plots ===========================

# plotdata_all = bind_rows(
#   mutate(plotdata, timing="actual"),
#   mutate(plotdata_late, timing="late"),
#   mutate(plotdata_early, timing="early")
# ) %>% 
#   rename(NCHS=nchs) %>% 
#   mutate(
#     median=median/log(10) + 6,
#     q05=pmin(pmax(q05/log(10) + 6, -1), 1.5),
#     q95=pmin(pmax(q95/log(10) + 6, -1), 1.5)
#   )
# 
# ggplot(plotdata_all) +
#   geom_line(aes(x=t, y=median, color=timing), size=1.0) +
#   geom_ribbon(
#     aes(x=t, ymin=q05, ymax=q95, fill=timing),
#     alpha=0.4
#   ) +
#   facet_wrap(~ NCHS, labeller=label_both) +
#   labs(fill="Timing",
#        x="Days since threshold",
#        y="Daily deaths per 1 million") +
#   guides(color=FALSE, fill=FALSE) +
#   scale_y_continuous(
#     limits=c(-1, 1.5),
#     breaks=c(-1, 0, 1),
#     labels=c("0.1", "1", "10")
#   ) +
#   theme_minimal_hgrid() +
#   scale_color_manual(values=c("#0072B2", "#009E73", "#D55E00")) +
#   scale_fill_manual(values=c("#0072B2", "#009E73", "#D55E00")) +
#   theme(legend.position = "top")
# 
# ggsave(
#   "img/nchs_timing_decrease.pdf",
#   width=16,
#   height=8,
#   units="cm"
# )
# 
# table_peaks_pos = tibble(
#   NCHS=1:6,
#   Early=peaks_pos_early,
#   Actual=peaks_pos,
#   Late=peaks_pos_late
# )
# 
# cat("Table peak days since treshhold\n")
# print(xtable(table_peaks_pos, include.rownames=FALSE))
# 
# table_peaks_val = tibble(
#   NCHS=1:6,
#   Early=peaks_val_early,
#   Actual=peaks_val,
#   Late=peaks_val_late
# )
# 
# cat("Table vaue at peak\n")
# print(xtable(table_peaks_val, include.rownames=FALSE))
# 

nchs_counts = df %>% 
  distinct(nchs, fips) %>% 
  group_by(nchs) %>% 
  count() %>% 
  pull(n)

summary_list = list(
  "early"=nchs_summaries_early,
  "actual"=nchs_summaries_actual,
  "late"=nchs_summaries_late
)


nchs_counts = df %>% 
  distinct(nchs, fips) %>% 
  group_by(nchs) %>% 
  count() %>% 
  pull(n)

summary_list = nchs_summary_list


cd_diffs = differences_summaries(curve_summary_list, df)

stats = list()

for (j in 1:3) {
  summary_name = names(summary_list)[j]
  summary = summary_list[[j]]
  
  stats_j = list()
  for (i in 1:6) {
    data_list = list(
      pos = apply(summary$agg_lp[ , , i], 1, which.max) - 1,
      val = 1e6 * exp(apply(summary$agg_lp[ , , i], 1, max)),
      cd10 = summary$agg_cumdeaths[ , 10, i],
      cd20 = summary$agg_cumdeaths[ , 20, i],
      cd30 = summary$agg_cumdeaths[ , 30, i],
      cdmay1 = summary$agg_cumdeaths_may_1[ , i],
      cd10_lma = cd_diffs$agg_cumdeaths_late_minus_actual[ , 10, i],
      cd20_lma = cd_diffs$agg_cumdeaths_late_minus_actual[ , 20, i],
      cd30_lma = cd_diffs$agg_cumdeaths_late_minus_actual[ , 30, i],
      cdmay1_lma = cd_diffs$agg_cumdeaths_may_1_late_minus_actual[ , i],
      cd10_ame = cd_diffs$agg_cumdeaths_actual_minus_early[ , 10, i],
      cd20_ame = cd_diffs$agg_cumdeaths_actual_minus_early[ , 20, i],
      cd30_ame = cd_diffs$agg_cumdeaths_actual_minus_early[ , 30, i],
      cdmay1_ame = cd_diffs$agg_cumdeaths_may_1_actual_minus_early[ , i]
    )
    
    for (k in seq_along(data_list)) {
      D = data_list[[k]]
      Dname = names(data_list)[k]
      
      if (i == 1) { # initialize vectors
        stats_j[[Dname]] = list()
        stats_j[[Dname]]$mean = numeric(6)
        stats_j[[Dname]]$sd = numeric(6)
        stats_j[[Dname]]$median = numeric(6)
        stats_j[[Dname]]$iqr = numeric(6)
        stats_j[[Dname]]$quantiles = matrix(0, 5, 6)
      }
      
      stats_j[[Dname]]$mean[i] = mean(D)
      stats_j[[Dname]]$sd[i] = sd(D)
      quants = quantile(D, c(0.05, 0.25, 0.50, 0.75, 0.95))
      stats_j[[Dname]]$quantiles[ ,i] = quants
      stats_j[[Dname]]$iqr[i] = quants["75%"] - quants["25%"]
      stats_j[[Dname]]$median[i] = quants["50%"]
    }
  }
  
  stats[[summary_name]] = stats_j
  
}


table_peaks = tibble(
  NCHS=1:6,
  Early_pos=sprintf(
    "%s (%s)",
    stats$early$pos$median,
    stats$early$pos$iqr
  ),
  Actual_pos=sprintf(
    "%s (%s)",
    stats$actual$pos$median,
    stats$actual$pos$iqr
  ),
  Late_pos=sprintf(
    "%s (%s)",
    stats$late$pos$median,
    stats$late$pos$iqr
  ),
  Early_val=sprintf(
    "%.2f (%.2f)",
    stats$early$val$median,
    stats$early$val$iqr
  ),
  Actual_val=sprintf(
    "%.2f (%.2f)",
    stats$actual$val$median,
    stats$actual$val$iqr
  ),
  Late_val=sprintf(
    "%.2f (%.2f)",
    stats$late$val$median,
    stats$late$val$iqr
  )
)


cat("Table peak days since treshhold\n")
print(xtable(table_peaks[ ,-1], include.rownames=FALSE))

# 
# table_peaks_val  = tibble(
#   NCHS=1:6,
#   Early=sprintf(
#     "%.2f [%.2f, %.2f]",
#     nchs_summaries_early$peak_val_q50$peak_val_q50,
#     nchs_summaries_early$peak_val_q05$peak_val_q05,
#     nchs_summaries_early$peak_val_q95$peak_val_q95
#   ),
#   Actual=sprintf(
#     "%.2f [%.2f, %.2f]",
#     nchs_summaries_actual$peak_val_q50$peak_val_q50,
#     nchs_summaries_actual$peak_val_q05$peak_val_q05,
#     nchs_summaries_actual$peak_val_q95$peak_val_q95
#   ),
#   Late=sprintf(
#     "%.2f [%.2f, %.2f]",
#     nchs_summaries_late$peak_val_q50$peak_val_q50,
#     nchs_summaries_late$peak_val_q05$peak_val_q05,
#     nchs_summaries_late$peak_val_q95$peak_val_q95
#   )
# )

# cum deaths stats

nchs_pop = df %>% 
  select(fips, nchs, pop) %>% 
  distinct() %>% 
  group_by(nchs) %>% 
  summarize(pop=sum(pop)) %>% 
  pull(pop)

cm_tbl = tibble(NCHS=1:6, pop=nchs_pop)
for (stat_field in c("cd10", "cd20", "cd30", "cdmay1")) {
  for (d in c("early", "actual", "late")) {
    fname = paste(stat_field, d, sep="_")
    cm_tbl[[fname]] = sprintf(
      "%.2f [%.2f, %.2f]",
      stats[[d]][[stat_field]]$median,
      stats[[d]][[stat_field]]$quantiles[1, ],
      stats[[d]][[stat_field]]$quantiles[5, ]
    )
  }
}
cm_tbl


cm_tbl2 = tibble(NCHS=1:6, pop=nchs_pop)
for (stat_field in c("cd10", "cd20", "cd30", "cdmay1")) {
  for (d in c("early", "actual", "late")) {
    fname = paste(stat_field, d, sep="_")
    cm_tbl2[[paste0(fname, "_median")]] = stats[[d]][[stat_field]]$median
    cm_tbl2[[paste0(fname, "_mean")]] = stats[[d]][[stat_field]]$mean
    cm_tbl2[[paste0(fname, "_q05")]] = stats[[d]][[stat_field]]$quantiles[1, ]
    cm_tbl2[[paste0(fname, "_q95")]] = stats[[d]][[stat_field]]$quantiles[5, ]
  }
}
cm_tbl2


write_csv(cm_tbl2, "cumstats_rolldeaths_decrease.csv")


cm_tbl3 = tibble(NCHS=1:6, pop=nchs_pop)
for (stat_field in c("cd10_lma", "cd20_lma", "cd30_lma", "cdmay1_lma", "cd10_ame", "cd20_ame", "cd30_ame", "cdmay1_ame")) {
  for (d in c("actual")) {
    fname = stat_field
    cm_tbl3[[paste0(fname, "_median")]] = stats[[d]][[stat_field]]$median
    cm_tbl3[[paste0(fname, "_mean")]] = stats[[d]][[stat_field]]$mean
    cm_tbl3[[paste0(fname, "_q05")]] = stats[[d]][[stat_field]]$quantiles[1, ]
    cm_tbl3[[paste0(fname, "_q95")]] = stats[[d]][[stat_field]]$quantiles[5, ]
  }
}
cm_tbl3

write_csv(cm_tbl3, "cumstats_rolldeaths_decrease_diffs.csv")




tbl_list = list()
tbl_list[[1]] = tibble(field=c("cd10", "cd20", "cd30", "cd_may1"))
for (x in c("actual", "early", "late")) {
  tbl_ = tibble(
    median=c(
      apply(nchs_summary_list[[x]]$agg_cumdeaths_total, 2, median)[ c(10, 20, 30)],
      median(nchs_summary_list[[x]]$agg_cumdeaths_may_1_total)
    ),
    mean=c(
      apply(nchs_summary_list[[x]]$agg_cumdeaths_total, 2, median)[ c(10, 20, 30)],
      median(nchs_summary_list[[x]]$agg_cumdeaths_may_1_total)
    ),
    q05=c(
      apply(nchs_summary_list[[x]]$agg_cumdeaths_total, 2, quantile, 0.05)[ c(10, 20, 30)],
      quantile(nchs_summary_list[[x]]$agg_cumdeaths_may_1_total, 0.05)
    ),
    q95=c(
      apply(nchs_summary_list[[x]]$agg_cumdeaths_total, 2, quantile, 0.95)[ c(10, 20, 30)],
      quantile(nchs_summary_list[[x]]$agg_cumdeaths_may_1_total, 0.95)
    )
  )
  names(tbl_) = paste(names(tbl_), x, sep="_")
  tbl_list[[length(tbl_list) + 1]] = tbl_
}

for (field in c("late_minus_actual", "actual_minus_early")) {
  fcm = paste0("agg_cumdeaths_", field, "_total")
  fmay1 = paste0("agg_cumdeaths_may_1_", field, "_total")
  tbl_ = tibble(
    median=c(
      apply(cd_diffs[[fcm]], 2, median)[ c(10, 20, 30)],
      median(cd_diffs[[fmay1]])
    ),
    mean=c(
      apply(cd_diffs[[fcm]], 2, mean)[ c(10, 20, 30)],
      mean(cd_diffs[[fmay1]])
    ),
    q05=c(
      apply(cd_diffs[[fcm]], 2, quantile, 0.05)[ c(10, 20, 30)],
      quantile(cd_diffs[[fmay1]], 0.05)
    ),
    q95=c(
      apply(cd_diffs[[fcm]], 2, quantile, 0.95)[ c(10, 20, 30)],
      quantile(cd_diffs[[fmay1]], 0.95)
    )
  )
  names(tbl_) = paste(names(tbl_), field, sep="_")
  tbl_list[[length(tbl_list) + 1]] = tbl_
}

tbl_diffs = bind_cols(tbl_list)


write_csv(tbl_diffs, "cumstats_rolldeaths_decrease_diffs_totals.csv")


