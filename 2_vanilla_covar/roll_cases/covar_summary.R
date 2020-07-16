library(tidyverse)
library(magrittr)
library(feather)
library(rstanarm)
library(gridExtra)

## Read data
county_pred <- read_feather("../../county_train_cases.feather")
model <- readRDS("./model.rds")
county_fit <- readRDS("./county_fit.rds")
source("../../plot_foo.R")

## define y
#length(unique(county_pred$fips))
county_pred %<>%  
  mutate(y = roll_cases, 
         intrv_decrease = (date - decrease_50_total_visiting >= 5) * 1, 
         days_since_intrv_decrease = as.numeric(date - decrease_50_total_visiting - 5 + 1), 
         age_65_plus = log(1e4 * age_65_plus / pop), 
         black = log(1e4 * black / pop), 
         hispanic = log(1e4 * hispanic / pop)
         ) %>%
    filter(!is.na(y), 
         !is.na(decrease_50_total_visiting), 
         days_since_intrv_decrease <= 17)
#length(unique(county_pred$fips))

## obtain distribution values from fit sampling
county_pred %<>% 
  mutate(
    fit_mu = apply(county_fit, 2, mean),
    fit_med = apply(county_fit, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    fit_lo = apply(county_fit, 2, quantile, probs = 0.05),
    fit_hi = apply(county_fit, 2, quantile, probs = 0.95))

## modify values to obtain counterfactual
covar_ = pull(unique(county_pred[, c("fips", params$covar)]), params$covar)

county_pred1 <- county_pred
county_pred3 <- county_pred

county_pred1 %<>% mutate(!!params$covar:=quantile(covar_, 0.10))
county_pred3 %<>% mutate(!!params$covar:=quantile(covar_, 0.90))

county_ctr1 <- model %>% 
  posterior_predict(county_pred1, draws = 200)
county_ctr3 <- model %>% 
  posterior_predict(county_pred3, draws = 200)

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

    val_ <- unique(county_pred[county_pred$fips %in% fips_, c("fips", params$covar)])

  county_plots <- lapply(fips_, 
                         function(x) county_pred %>% 
                           filter(fips == x) %>% 
                           gg_covar_sampling(
                           name = name_$name[name_$fips == x], 
                           covar_val = val_[val_$fips == x, params$covar],
                           lag = 12))
  county_plots <- marrangeGrob(county_plots, 
                               nrow = 6, ncol = 2, 
                               left = "", 
                               top = paste(params$covar, 
                                    "q10=", round(quantile(covar_, 0.10)),
                                    'q90=', round(quantile(covar_, 0.90))))
  ggsave(paste("./covar_summary/", 
               params$covar, "_sampling_nchs_", c, ".pdf", sep = ""), 
         county_plots, width = 15, height = 25, units = "cm")
}
