library(tidyverse)
library(magrittr)
library(feather)
library(gridExtra)

source("../plot_foo.R")

## Read data
county_fit <- readRDS("./roll_deaths/county_fit.rds")
county_ctr <- readRDS("./roll_deaths/county_ctr.rds")
county_pred <- read_feather("../county_train_stayhome.feather")
#length(unique(county_pred$fips))

## Get fit and ctr values
county_pred %<>% 
  mutate(
    fit_mu = apply(county_fit, 2, mean),
    fit_med = apply(county_fit, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    fit_lo = apply(county_fit, 2, quantile, probs = 0.05),
    fit_hi = apply(county_fit, 2, quantile, probs = 0.95))

county_pred %<>% 
  mutate(
    ctr_mu = apply(county_ctr, 2, mean),
    ctr_med = apply(county_ctr, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    ctr_lo = apply(county_ctr, 2, quantile, probs = 0.05),
    ctr_hi = apply(county_ctr, 2, quantile, probs = 0.95))

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

compare <- county_pred %>% 
  select(date, fips, state, county, nchs, pop, 
         y, y_eff,
         decrease_50_total_visiting, stayhome,
         fit_med_eff, ctr_med_eff) %>% 
  rename(fit_med_eff_stayhome = fit_med_eff, 
         ctr_med_eff_stayhome = ctr_med_eff)

## Read data
county_fit <- readRDS("./roll_deaths_decrease/county_fit.rds")
county_ctr <- readRDS("./roll_deaths_decrease/county_ctr.rds")
county_pred <- read_feather("../county_train_decrease.feather")
#length(unique(county_pred$fips))

## Get fit and ctr values
county_pred %<>% 
  mutate(
    fit_mu = apply(county_fit, 2, mean),
    fit_med = apply(county_fit, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    fit_lo = apply(county_fit, 2, quantile, probs = 0.05),
    fit_hi = apply(county_fit, 2, quantile, probs = 0.95))

county_pred %<>% 
  mutate(
    ctr_mu = apply(county_ctr, 2, mean),
    ctr_med = apply(county_ctr, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    ctr_lo = apply(county_ctr, 2, quantile, probs = 0.05),
    ctr_hi = apply(county_ctr, 2, quantile, probs = 0.95))

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

compare %<>% left_join(county_pred %>% 
  select(date, fips, state, county, nchs, pop,
         y, y_eff,
         decrease_50_total_visiting, stayhome,
         fit_med_eff, ctr_med_eff) %>% 
  rename(fit_med_eff_decrease = fit_med_eff, 
         ctr_med_eff_decrease = ctr_med_eff))

for(c in 1:6) {
  fips_ <- compare %>% 
    distinct(fips, nchs, pop) %>% 
    filter(nchs == c) %>% 
    arrange(desc(pop)) %>% 
    pull(fips)
  
  name_ <- compare %>%
    filter(fips %in% fips_) %>% 
    distinct(fips, state, county) %>%
    mutate(name = paste(state, county))
  
  county_plots <- lapply(fips_, 
                         function(x) compare %>% 
                           filter(fips == x) %>% 
                           gg_compare_effect(
                             name = name_$name[name_$fips == x]))
  county_plots <- marrangeGrob(county_plots, 
                               nrow = 6, ncol = 2, 
                               left = "", top = "")
  ggsave(paste("./compare_summary/", 
               "sampling_nchs_", c, ".pdf", sep = ""), 
         county_plots, width = 15, height = 25, units = "cm")
}

  
  
  
  
  
