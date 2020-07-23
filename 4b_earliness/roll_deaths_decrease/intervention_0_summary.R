library(tidyverse)
library(magrittr)
library(feather)
library(rstanarm)
library(gridExtra)

## Read data
model <- readRDS("./model.rds")
county_fit <- readRDS("./county_fit.rds")
source("../../plot_foo.R")

## Read county_train
county_pred <- read_feather("../../county_train_decrease.feather")
#length(unique(county_pred$fips))

## obtain distribution values from fit sampling
county_pred %<>% 
  mutate(
    fit_mu = apply(county_fit, 2, mean),
    fit_med = apply(county_fit, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    fit_lo = apply(county_fit, 2, quantile, probs = 0.05),
    fit_hi = apply(county_fit, 2, quantile, probs = 0.95))

## modify values to obtain counterfactual
county_pred$intrv_decrease <- 0
county_pred$intrv_stayhome <- 0

# not sure what should go here
county_pred$days_since_intrv_decrease <- county_pred$days_since_thresh
county_pred$days_since_intrv_stayhome <- county_pred$days_since_thresh

# county_pred %>%
#   select(fips, date, days_since_thresh, intrv_decrease_fit, intrv_decrease) %>%
#   arrange(fips, date) %>%
#   head(1000) %>% view

## get posteriors
county_ctr <- model %>% 
  posterior_predict(county_pred, draws = 500)

saveRDS(county_ctr, "./county_ctr.rds")

#table(county_pred$nchs, county_pred$intrv_decrease_fit)

## generate nchs summaries

county_pred %<>% 
  mutate(
    ctr_mu = apply(county_ctr, 2, mean),
    ctr_med = apply(county_ctr, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    ctr_lo = apply(county_ctr, 2, quantile, probs = 0.05),
    ctr_hi = apply(county_ctr, 2, quantile, probs = 0.95))

for(c in 1:6) {
    fips_ <- county_pred %>% 
      distinct(fips, nchs, pop) %>% 
      filter(nchs == c) %>% 
      arrange(desc(pop)) %>% 
      pull(fips)

    name_ <- county_pred %>%
      filter(fips %in% fips_) %>% 
      distinct(fips, state, county) %>%
      mutate(name = paste(state, county))

  county_plots <- lapply(fips_, 
                         function(x) county_pred %>% 
                           filter(fips == x) %>% 
                           gg_intrv_sampling(
                           name = name_$name[name_$fips == x], 
                           lag_decrease = 12, 
                           lag_stayhome = 12))
  county_plots <- marrangeGrob(county_plots, 
                               nrow = 6, ncol = 2, 
                               left = "", top = "")
  ggsave(paste("./intervention_0_summary/", 
               "sampling_nchs_", c, ".pdf", sep = ""), 
         county_plots, width = 15, height = 25, units = "cm")
}

## generate cumulative effect summary
county_fit_effect <- matrix(nrow = 500, ncol = 0)
county_ctr_effect <- matrix(nrow = 500, ncol = 0)
for(f in unique(county_pred$fips)){
  county_idx <- which(county_pred$fips == f)
  fit <- county_fit[, county_idx]
  fit <- t(apply(fit, 1, cumsum))
  ctr <- county_ctr[, county_idx]
  ctr <- t(apply(ctr, 1, cumsum))
  
  county_fit_effect <- cbind(county_fit_effect, fit)
  county_ctr_effect <- cbind(county_ctr_effect, ctr)
}

county_pred %<>% 
  group_by(fips) %>% 
  mutate(y_eff = cumsum(y)) %>%
  ungroup()

county_pred %<>% 
  mutate(
    fit_mu_eff = apply(county_fit_effect, 2, mean),
    fit_med_eff = apply(county_fit_effect, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    fit_lo_eff = apply(county_fit_effect, 2, quantile, probs = 0.05),
    fit_hi_eff = apply(county_fit_effect, 2, quantile, probs = 0.95))

county_pred %<>% 
  mutate(
    ctr_mu_eff = apply(county_ctr_effect, 2, mean),
    ctr_med_eff = apply(county_ctr_effect, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    ctr_lo_eff = apply(county_ctr_effect, 2, quantile, probs = 0.05),
    ctr_hi_eff = apply(county_ctr_effect, 2, quantile, probs = 0.95))

for(c in 1:6) {
  fips_ <- county_pred %>% 
    distinct(fips, nchs, pop) %>% 
    filter(nchs == c) %>% 
    arrange(desc(pop)) %>% 
    pull(fips)
  
  name_ <- county_pred %>%
    filter(fips %in% fips_) %>% 
    distinct(fips, state, county) %>%
    mutate(name = paste(state, county))
  
  county_plots <- lapply(fips_, 
                         function(x) county_pred %>% 
                           filter(fips == x) %>% 
                           gg_intrv_effect(
                             name = name_$name[name_$fips == x],  
                             lag_decrease = 12))
  county_plots <- marrangeGrob(county_plots, 
                               nrow = 6, ncol = 2, 
                               left = "", top = "")
  ggsave(paste("./intervention_0_summary/", 
               "effect_nchs_", c, ".pdf", sep = ""), 
         county_plots, width = 15, height = 25, units = "cm")
}
