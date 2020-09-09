library(tidyverse)
library(progress)
library(feather)
library(rstanarm)
library(gridExtra)
library(cowplot)
library(ggthemes)
library(xtable)
library(lubridate)

source("curves_by_fips.R")


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



differences_summaries = function(curve_summary_list, df) {
  
  max_t = 60 #  max(df$days_since_thresh)
  
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
df = read_feather("../../county_train_stayhome.feather")
curve_summary_list = list()
nchs_summary_list = list()

# Summary for intervention as observed
cat("computing curve summaries by fips no offset...\n")
curve_summaries = curves_by_fips(model, df)  # extracted_curves = curve_summaries
curve_summary_list[["actual"]] = curve_summaries
cat("summarizing by nchs...\n")
nchs_summaries = nchs_effect_summaries(curve_summaries)
nchs_summary_list[["actual"]] = nchs_summaries

# Answer time to peak and peak value and plot
# Note: [ ,-1] to remove nchs
# N = table(distinct(select(df, fips, nchs))$nchs)


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

# Per capita plot per nchs
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
#     q05=pmin(pmax(q05/log(10) + 6, -1), log10(20)),
#     q95=pmin(pmax(q95/log(10) + 6, -1), log10(20))
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
#   guides(color=FALSE) +
#   scale_y_continuous(
#     limits=c(-1, log10(20)),
#     breaks=c(-1, 0, 1),
#     labels=c("0.1", "1", "10")
#   ) +
#   theme_minimal_hgrid() +
#   scale_color_manual(values=c("#0072B2", "#009E73", "#D55E00")) +
#   scale_fill_manual(values=c("#0072B2", "#009E73", "#D55E00")) +
#   theme(legend.position = "top")
# 
# 
# ggsave(
#   "img/nchs_timing_stayhome.pdf",
#   width=16,
#   height=10,
#   units="cm"
# )

# table_peaks_pos  = tibble(
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
    cm_tbl2[[paste0(fname, "_q05")]] = stats[[d]][[stat_field]]$quantiles[1, ]
    cm_tbl2[[paste0(fname, "_q95")]] = stats[[d]][[stat_field]]$quantiles[5, ]
  }
}
cm_tbl2


write_csv(cm_tbl2, "cumstats_rolldeaths.csv")

cm_tbl3 = tibble(NCHS=1:6, pop=nchs_pop)
for (stat_field in c("cd10_lma", "cd20_lma", "cd30_lma", "cdmay1_lma", "cd10_ame", "cd20_ame", "cd30_ame", "cdmay1_ame")) {
  for (d in c("actual")) {
    fname = stat_field
    cm_tbl3[[paste0(fname, "_median")]] = stats[[d]][[stat_field]]$median
    cm_tbl3[[paste0(fname, "_q05")]] = stats[[d]][[stat_field]]$quantiles[1, ]
    cm_tbl3[[paste0(fname, "_q95")]] = stats[[d]][[stat_field]]$quantiles[5, ]
  }
}
cm_tbl3

write_csv(cm_tbl3, "cumstats_rolldeaths_diffs.csv")


# totals table
tbl_list = list()
tbl_list[[1]] = tibble(field=c("cd10", "cd20", "cd30", "cd_may1"))
for (x in c("actual", "early", "late")) {
  tbl_ = tibble(
    median=c(
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

write_csv(tbl_diffs, "cumstats_rolldeaths_diffs_totals.csv")

