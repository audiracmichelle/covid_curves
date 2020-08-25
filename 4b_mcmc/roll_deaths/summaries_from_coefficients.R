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

    resid = samples[ ,nms] %*% t(poly_t) 
    lp = covar_effect + interv_effect + resid
    peak_pos = apply(lp, 1, which.max) - 1
    peak_val = 1e6 * exp(apply(lp, 1, max))
    
    out = list(
      data = f_data,
      intervention_curve = interv_effect,
      random_effect_curve = resid,
      covar_curve = covar_effect,
      linear_predictor = lp,
      disp = samples[ ,"reciprocal_dispersion"],
      peak_pos = peak_pos,
      peak_val = peak_val
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

  # these function return a time series for each county
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

  funs2 = list(
    peak_val_mean=function(x) mean(x$peak_pos),
    peak_val_sd=function(x) sd(x$peak_pos),
    peak_val_q50=function(x) quantile(x$peak_pos, 0.5),
    peak_val_q05=function(x) quantile(x$peak_pos, 0.05),
    peak_val_q95=function(x) quantile(x$peak_pos, 0.95),
    peak_val_q25=function(x) quantile(x$peak_pos, 0.25),
    peak_val_q75=function(x) quantile(x$peak_pos, 0.75),
    peak_val_iqr=function(x) quantile(x$peak_pos, 0.75) - quantile(x$peak_pos, 0.25),
    peak_val_mean=function(x) mean(x$peak_val),
    peak_val_sd=function(x) sd(x$peak_val),
    peak_val_q50=function(x) quantile(x$peak_val, 0.5),
    peak_val_q05=function(x) quantile(x$peak_val, 0.05),
    peak_val_q95=function(x) quantile(x$peak_val, 0.95),
    peak_val_q25=function(x) quantile(x$peak_val, 0.25),
    peak_val_q75=function(x) quantile(x$peak_val, 0.75),
    peak_val_qiqr=function(x) quantile(x$peak_val, 0.75) - quantile(x$peak_val, 0.25)
  )
  
  # Answer time to peak and peak value and plot
  # Note: [ ,-1] to remove nchs
  N = table(distinct(select(df, fips, nchs))$nchs)
  
  # apply function to each extracted curve
  nchs = map_chr(extracted_curves, ~ .x$data$nchs[1])
  max_t = ncol(extracted_curves[[1]]$predicted_mean) - 1
  
  summaries = list()
  progress_bar = progress::progress_bar$new(
    total=length(funs) + length(funs2)
  )
  
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
      summarise_all(median) 
    progress_bar$tick()
  }
  
  for (i in seq_along(funs2)) {
    fname = names(funs2)[i]
    f = funs2[[i]]
    
    summaries[[paste0(fname, "_median")]] = tibble(
      nchs=nchs,
      var=map_dbl(extracted_curves, f)
    ) %>% 
      group_by(nchs) %>% 
      summarise_all(median) %>% 
      pull(var)
    
    summaries[[paste0(fname, "_iqr")]] = tibble(
      nchs=nchs,
      var=map_dbl(extracted_curves, f)
    ) %>% 
      group_by(nchs) %>% 
      summarise_all(function(x) quantile(x, 0.75) - quantile(x, 0.25)) %>% 
      pull(var)

    
    summaries[[paste0(fname, "_mean")]] = tibble(
      nchs=nchs,
      var=map_dbl(extracted_curves, f)
    ) %>% 
      group_by(nchs) %>% 
      summarise_all(mean) %>% 
      pull(var)
    
    
    summaries[[paste0(fname, "_sd")]] = tibble(
      nchs=nchs,
      var=map_dbl(extracted_curves, f)
    ) %>% 
      group_by(nchs) %>% 
      summarise_all(sd) %>% 
      pull(var)
    
    progress_bar$tick()
  }
  
  # Strategy 2: Aggregate lp curves per sample
  agg_lp = array(0, c(1000, max_t + 1, 6))
  nchs_count = table(nchs)
  for (i in seq_along(nchs)) {
    k = as.integer(nchs[i])
    n = nchs_count[k]
    lp = extracted_curves[[i]]$linear_predictor
    agg_lp[ , , k] = agg_lp[ , , k] + lp / n
  }
  
  summaries$agg_lp = agg_lp
  
  return (summaries)
}

# Read data
model = readRDS("./model.rds")
county_fit = readRDS("./county_fit.rds")
source("../../plot_foo.R")
df = read_feather("../../county_train_stayhome.feather")

# === actual intervention ===

# Summary for intervention as observed
cat("computing curve summaries by fips no offset...\n")
curve_summaries = curves_by_fips(model, df)
cat("summarizing by nchs...\n")
nchs_summaries = nchs_effect_summaries(curve_summaries)
nchs_summaries_actual = nchs_summaries

# Answer time to peak and peak value and plot
# Note: [ ,-1] to remove nchs
N = table(distinct(select(df, fips, nchs))$nchs)


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
up = 10

# Summary for intervention as observed
cat("computing curve summaries by fips late intervention...\n")
curve_summaries = curves_by_fips(
  model,
  df,
  intervention_offset=up
)
cat("summarizing by nchs...\n")
nchs_summaries = nchs_effect_summaries(curve_summaries)
nchs_summaries_late = nchs_summaries

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
down = -10

# Summary for intervention as observed
cat("computing curve summaries by fips early intervention...\n")
curve_summaries = curves_by_fips(
  model,
  df,
  intervention_offset=down
)
cat("summarizing by nchs...\n")
nchs_summaries = nchs_effect_summaries(curve_summaries)
nchs_summaries_early = nchs_summaries

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
    q05=pmin(pmax(q05/log(10) + 6, -1), log10(20)),
    q95=pmin(pmax(q95/log(10) + 6, -1), log10(20))
  )

ggplot(plotdata_all) +
  geom_line(aes(x=t, y=median, color=timing), size=1.0) +
  geom_ribbon(
    aes(x=t, ymin=q05, ymax=q95, fill=timing),
    alpha=0.4
  ) +
  facet_wrap(~ NCHS, labeller=label_both) +
  labs(fill="Timing",
       x="Days since threshold",
       y="Daily deaths per 1 million") +
  guides(color=FALSE) +
  scale_y_continuous(
    limits=c(-1, log10(20)),
    breaks=c(-1, 0, 1),
    labels=c("0.1", "1", "10")
  ) +
  theme_minimal_hgrid() +
  scale_color_manual(values=c("#0072B2", "#009E73", "#D55E00")) +
  scale_fill_manual(values=c("#0072B2", "#009E73", "#D55E00")) +
  theme(legend.position = "top")


ggsave(
  "img/nchs_timing_stayhome.pdf",
  width=16,
  height=10,
  units="cm"
)

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

summary_list = list(
  "early"=nchs_summaries_early,
  "actual"=nchs_summaries_actual,
  "late"=nchs_summaries_late
)

peaks = list()

for (j in 1:3) {
  summary_name = names(summary_list)[j]
  summary = summary_list[[j]]

  pos_mean = pos_sd = pos_q05 = pos_q95 = pos_q50 = pos_q25 = pos_q75 = pos_iqr = numeric(6)
  val_mean = val_sd = val_q05 = val_q95 = val_q50 = val_q25 = val_q75 = val_iqr = numeric(6)

  for (i in 1:6) {
    D = summary$agg_lp[ , , i]
    pos = apply(D, 1, which.max) - 1
    val = 1e6 * exp(apply(D, 1, max))
    pos_mean[i] = mean(pos)
    pos_sd[i] = sd(pos)
    pos_q05[i] = quantile(pos, 0.05)
    pos_q95[i] = quantile(pos, 0.95)
    pos_q50[i] = quantile(pos, 0.50)
    pos_q25[i] = quantile(pos, 0.25)
    pos_q75[i] = quantile(pos, 0.75)
    pos_iqr[i] = pos_q75[i] - pos_q25[i]
    val_mean[i] = mean(val)
    val_sd[i] = sd(val)
    val_q05[i] = quantile(val, 0.05)
    val_q95[i] = quantile(val, 0.95)
    val_q50[i] = quantile(val, 0.50)
    val_q25[i] = quantile(val, 0.25)
    val_q75[i] = quantile(val, 0.75)
    val_iqr[i] = val_q75[i] - val_q25[i]
  }
  
  peaks[[summary_name]] = list(
    pos_mean=pos_mean,
    pos_sd=pos_sd,
    pos_q05=pos_q05,
    pos_q95=pos_q95,
    pos_q50=pos_q50,
    pos_q25=pos_q25,
    pos_q75=pos_q75,
    pos_iqr=pos_iqr,
    val_mean=val_mean,
    val_sd=val_sd,
    val_q05=val_q05,
    val_q95=val_q95,
    val_q50=val_q50,
    val_q25=val_q25,
    val_q75=val_q75,
    val_iqr=val_iqr
  )
  
}


table_peaks  = tibble(
  NCHS=1:6,
  Early_pos=sprintf(
    "%s (%s)",
    peaks$early$pos_q50,
    peaks$early$pos_iqr
  ),
  Actual_pos=sprintf(
    "%s (%s)",
    peaks$actual$pos_q50,
    peaks$actual$pos_iqr
  ),
  Late_pos=sprintf(
    "%s (%s)",
    peaks$late$pos_q50,
    peaks$late$pos_iqr
  ),
  Early_val=sprintf(
    "%.2f (%.2f)",
    peaks$early$val_q50,
    peaks$early$val_iqr
  ),
  Actual_val=sprintf(
    "%.2f (%.2f)",
    peaks$actual$val_q50,
    peaks$actual$val_iqr
  ),
  Late_val=sprintf(
    "%.2f (%.2f)",
    peaks$late$val_q50,
    peaks$late$val_iqr
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

/