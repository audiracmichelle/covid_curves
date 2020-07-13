library(tidyverse)
library(magrittr)
library(feather)
library(rstanarm)
library(gridExtra)

## Read data
county_pred <- read_feather("../../county_train.feather")
model <- readRDS("./model.rds")
county_fit <- readRDS("./county_fit.rds")
source("../../plot_foo.R")

## define y
#length(unique(county_pred$fips))
county_pred %<>%  
  mutate(y = roll_deaths, 
         intrv_decrease = (date - decrease_40_total_visiting >= 12) * 1, 
         intrv_stayhome = (date - stayhome >= 12) * 1, 
         days_since_intrv_decrease = as.numeric(date - intrv_decrease - 12 + 1), 
         days_since_intrv_stayhome = as.numeric(date - stayhome - 12 + 1)) %>%
    filter(!is.na(y), 
         !is.na(decrease_40_total_visiting), 
         !is.na(stayhome), 
         days_since_intrv_stayhome <= 17)
#length(unique(county_pred$fips))

## obtain distribution values from fit sampling
county_pred %<>% 
  mutate(
    fit_mu = apply(county_fit, 2, mean),
    fit_med = apply(county_fit, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    fit_lo = apply(county_fit, 2, quantile, probs = 0.05),
    fit_hi = apply(county_fit, 2, quantile, probs = 0.95))

## modify values to obtain counterfactual
county_pred$intrv_decrease <- 1
county_pred$intrv_stayhome <- 1

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
  ggsave(paste("./intervention_1_summary/", 
               "sampling_nchs_", c, ".pdf", sep = ""), 
         county_plots, width = 15, height = 25, units = "cm")
}