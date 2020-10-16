library(tidyverse)
library(magrittr)
library(feather)
library(rstanarm)
library(gridExtra)

## Read data
model <- readRDS("./model.rds")
county_future_fit <- readRDS("./county_future_fit.rds")
source("../../plot_foo.R")

## Read county_future
county_pred <- read_feather("../../county_future_decrease.feather")
#dim(county_pred)
#summary(county_pred$date[county_pred$index_desc == 1])

county_pred %<>%
  filter(date <= as.Date("2020-05-05"))
#dim(county_pred)

## obtain distribution values from fit sampling
county_pred %<>% 
  mutate(
    fit_mu = apply(county_future_fit, 2, mean),
    fit_med = apply(county_future_fit, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    fit_lo = apply(county_future_fit, 2, quantile, probs = 0.05),
    fit_hi = apply(county_future_fit, 2, quantile, probs = 0.95))

## modify values to obtain counterfactual
county_pred1 = county_pred %>% 
  mutate(days_btwn_stayhome_thresh = 0) %>%
  mutate(intrv_stayhome = as.numeric(date >= threshold_day + 12))

county_pred1$days_since_intrv_stayhome <- county_pred1$days_since_thresh -12 + 1

## get posteriors
county_future_ctr1 <- model %>% 
  posterior_predict(county_pred1, draws = 500)

saveRDS(county_future_ctr1, "./county_since_ctr1.rds")

## generate nchs summaries
county_pred %<>% 
  mutate(
    ctr1_mu = apply(county_future_ctr1, 2, mean),
    ctr1_med = apply(county_future_ctr1, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    ctr1_lo = apply(county_future_ctr1, 2, quantile, probs = 0.05),
    ctr1_hi = apply(county_future_ctr1, 2, quantile, probs = 0.95))

for(c in 1:3) {
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
                           gg_days_since_sampling(
                           name = name_$name[name_$fips == x]))
  county_plots <- marrangeGrob(county_plots, 
                               nrow = 6, ncol = 2, 
                               left = "", top = "")
  ggsave(paste("./days_since_summary/", 
               "sampling_nchs_", c, ".pdf", sep = ""), 
         county_plots, width = 15, height = 25, units = "cm")
}
