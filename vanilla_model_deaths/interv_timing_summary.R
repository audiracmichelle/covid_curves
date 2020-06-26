library(tidyverse)
library(magrittr)
library(feather)
library(rstanarm)
library(gridExtra)

## Read data
county_pred <- read_feather("../county_train.feather")
model <- readRDS("./model.rds")
county_fit <- readRDS("./county_fit.rds")
coef_clusters <- readRDS("./coef_clusters.rds")
source("../plot_foo.R")

## modify values to obtain counterfactual
county_pred$intervention_fit <- county_pred$intervention
county_pred$intervention <- (county_pred$days_since_thresh >= 20 + 12) * 1
#county_pred$intervention <- 0

# county_pred %>%
#   select(fips, date, days_since_thresh, intervention_fit,intervention) %>%
#   arrange(fips, date) %>%
#   head(1000) %>% view

## get posteriors

county_ctr <- model %>% 
  posterior_predict(county_pred, draws = 200)

county_pred %<>% 
  mutate(
    fit_mu = apply(county_fit, 2, mean),
    fit_med = apply(county_fit, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    fit_lo = apply(county_fit, 2, quantile, probs = 0.025),
    fit_hi = apply(county_fit, 2, quantile, probs = 0.975)) 

county_pred %<>% 
  mutate(
    ctr_mu = apply(county_ctr, 2, mean),
    ctr_med = apply(county_ctr, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    ctr_lo = apply(county_ctr, 2, quantile, probs = 0.025),
    ctr_hi = apply(county_ctr, 2, quantile, probs = 0.975))

## generate cluster summaries

for(c in 1:5) {
  fips_ <- coef_clusters %>% 
    left_join(distinct(county_pred, fips, pop)) %>% 
    filter(cluster == c) %>% 
    arrange(desc(pop)) %>% 
    pull(fips)
  
  county_plots <- lapply(fips_, 
                         function(x) county_pred %>% 
                           filter(fips == x) %>% 
                           gg_vanilla_sampling())
  county_plots <- marrangeGrob(county_plots, 
                               nrow = 6, ncol = 2, 
                               left = "", top = "")
  ggsave(paste("./interv_timing_summary/", 
               "sampling_cluster_", c, ".pdf", sep = ""), 
         county_plots, width = 15, height = 25, units = "cm")
  
}

