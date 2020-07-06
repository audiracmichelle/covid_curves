library(tidyverse)
library(magrittr)
library(feather)
library(rstanarm)
library(gridExtra)

## Read data
county_pred <- read_feather("../../county_train.feather") %>%
  mutate(t = days_since_thresh)
model <- readRDS("./model.rds")
county_fit <- readRDS("./county_fit.rds")
source("../../plot_foo.R")


## define y
county_pred$y <- county_pred$roll_deaths
county_pred$cum_y <- county_pred$roll_deaths
#dim(county_pred)
county_pred %<>% 
  filter(!is.na(roll_deaths))
#dim(county_pred)

## modify values to obtain counterfactual
county_pred$intervention_fit <- county_pred$intervention
county_pred$days_btwn_fit <- county_pred$days_btwn_stayhome_thresh
county_pred$speed_btwn_fit <- county_pred$speed_btwn_stayhome_thresh

county_pred1 = county_pred %>% 
  mutate(days_btwn_stayhome_thresh = days_btwn_stayhome_thresh - 7) %>%
  mutate(intervention = as.numeric(date >= stayhome + 12 - 7))

county_pred3 = county_pred %>% 
  mutate(days_btwn_stayhome_thresh = days_btwn_stayhome_thresh + 7) %>%
  mutate(intervention = as.numeric(date >= stayhome + 12 + 7))


## get posteriors

county_ctr1 <- model %>% 
  posterior_predict(county_pred1, draws = 500)
county_ctr3 <- model %>% 
  posterior_predict(county_pred3, draws = 500)

county_pred %<>% 
  mutate(
    fit_mu = apply(county_fit, 2, mean),
    fit_med = apply(county_fit, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    fit_lo = apply(county_fit, 2, quantile, probs = 0.05),
    fit_hi = apply(county_fit, 2, quantile, probs = 0.95))

county_pred %<>% 
  mutate(
    ctr1_mu = apply(county_ctr1, 2, mean),
    ctr1_med = apply(county_ctr1, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    ctr1_lo = apply(county_ctr1, 2, quantile, probs = 0.05),
    ctr1_hi = apply(county_ctr1, 2, quantile, probs = 0.95),
    ctr3_mu = apply(county_ctr3, 2, mean),
    ctr3_med = apply(county_ctr3, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    ctr3_lo = apply(county_ctr3, 2, quantile, probs = 0.05),
    ctr3_hi = apply(county_ctr3, 2, quantile, probs = 0.95))

## generate nchs summaries
for(c in 1:6) {
  fips_ <- county_pred %>% 
    filter(index == 1, nchs == c) %>% 
    arrange(desc(days_btwn_stayhome_thresh), desc(pop)) %>% 
    pull(fips)
  
  county_plots <- lapply(fips_, 
                         function(x) county_pred %>% 
                           filter(fips == x) %>% 
                           gg_days_btwn_sampling())
  county_plots <- marrangeGrob(county_plots, 
                               nrow = 6, ncol = 2, 
                               left = "", top = "")
  ggsave(paste("./speed_btwn_summary/", 
               "sampling_nchs_", c, ".pdf", sep = ""), 
         county_plots, width = 15, height = 25, units = "cm")
  
}

## generate cumulative effect summary
county_fit_effect <- matrix(nrow = 500, ncol = 0)
county_ctr1_effect <- matrix(nrow = 500, ncol = 0)
county_ctr3_effect <- matrix(nrow = 500, ncol = 0)
for(f in unique(county_pred$fips)){
  county_idx <- which(county_pred$fips == f)
  fit <- county_fit[, county_idx]
  fit <- t(apply(fit, 1, cumsum))
  ctr1 <- county_ctr1[, county_idx]
  ctr1 <- t(apply(ctr1, 1, cumsum))
  ctr3 <- county_ctr3[, county_idx]
  ctr3 <- t(apply(ctr3, 1, cumsum))
  
  county_fit_effect <- cbind(county_fit_effect, fit)
  county_ctr1_effect <- cbind(county_ctr1_effect, ctr1)
  county_ctr3_effect <- cbind(county_ctr3_effect, ctr3) 
}

county_pred %<>% 
  mutate(
    fit_mu = apply(county_fit_effect, 2, mean),
    fit_med = apply(county_fit_effect, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    fit_lo = apply(county_fit_effect, 2, quantile, probs = 0.05),
    fit_hi = apply(county_fit_effect, 2, quantile, probs = 0.95))

county_pred %<>% 
  mutate(
    ctr1_mu = apply(county_ctr1_effect, 2, mean),
    ctr1_med = apply(county_ctr1_effect, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    ctr1_lo = apply(county_ctr1_effect, 2, quantile, probs = 0.05),
    ctr1_hi = apply(county_ctr1_effect, 2, quantile, probs = 0.95),
    ctr3_mu = apply(county_ctr3_effect, 2, mean),
    ctr3_med = apply(county_ctr3_effect, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    ctr3_lo = apply(county_ctr3_effect, 2, quantile, probs = 0.05),
    ctr3_hi = apply(county_ctr3_effect, 2, quantile, probs = 0.95))

## generate nchs summaries
for(c in 1:6) {
  fips_ <- county_pred %>% 
    filter(index == 1, nchs == c) %>% 
    arrange(desc(days_btwn_stayhome_thresh), desc(pop)) %>% 
    pull(fips)
  
  county_plots <- lapply(fips_, 
                         function(x) county_pred %>% 
                           filter(fips == x) %>% 
                           gg_days_btwn_effect())
  county_plots <- marrangeGrob(county_plots, 
                               nrow = 6, ncol = 2, 
                               left = "", top = "")
  ggsave(paste("./speed_btwn_summary/", 
               "effect_nchs_", c, ".pdf", sep = ""), 
         county_plots, width = 15, height = 25, units = "cm")
  
}