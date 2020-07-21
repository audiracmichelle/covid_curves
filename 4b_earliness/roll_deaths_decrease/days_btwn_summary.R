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

up = 7
down = -14

## define y
#length(unique(county_pred$fips))
county_pred %<>%  
  mutate(y = roll_deaths,  
         #intrv_stayhome = (date - stayhome >= 12) * 1,  
         intrv_decrease = (date - decrease_50_total_visiting >= 12) * 1,
         #days_since_intrv_stayhome = as.numeric(date - stayhome - 12 + 1), 
         days_since_intrv_decrease = as.numeric(date - decrease_50_total_visiting - 12 + 1),
         age_45_64 = log(1e4 * age_45_64 / pop), 
         age_65_plus = log(1e4 * age_65_plus / pop), 
         black = log(1e4 * black / pop), 
         hispanic = log(1e4 * hispanic / pop), 
         days_btwn_decrease_thresh = as.numeric(decrease_50_total_visiting - threshold_day)
         ) %>%
    filter(!is.na(y), 
         #!is.na(stayhome), 
         !is.na(decrease_50_total_visiting), 
         #days_since_intrv_stayhome <= 17
         days_since_intrv_decrease <= 17
         )
#length(unique(county_pred$fips))

## obtain distribution values from fit sampling
county_pred %<>% 
  mutate(
    fit_mu = apply(county_fit, 2, mean),
    fit_med = apply(county_fit, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    fit_lo = apply(county_fit, 2, quantile, probs = 0.05),
    fit_hi = apply(county_fit, 2, quantile, probs = 0.95))

## modify values to obtain counterfactual
county_pred1 = county_pred %>% 
  mutate(days_btwn_decrease_thresh = days_btwn_decrease_thresh + up) %>%
  mutate(intrv_decrease = as.numeric(date >= decrease_50_total_visiting + 12 + up))

county_pred3 = county_pred %>% 
  mutate(days_btwn_decrease_thresh = days_btwn_decrease_thresh + down) %>%
  mutate(intrv_decrease = as.numeric(date >= decrease_50_total_visiting + 12 + down))

# not sure what should go here
county_pred1$days_since_intrv_decrease <- county_pred1$days_since_intrv_decrease - up
county_pred3$days_since_intrv_decrease <- county_pred3$days_since_intrv_decrease - down

## get posteriors
county_ctr1 <- model %>% 
  posterior_predict(county_pred1, draws = 500)
county_ctr3 <- model %>% 
  posterior_predict(county_pred3, draws = 500)

## generate nchs summaries

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

  county_plots <- lapply(fips_, 
                         function(x) county_pred %>% 
                           filter(fips == x) %>% 
                           gg_days_btwn_sampling(
                           name = name_$name[name_$fips == x], 
                           up = up, 
                           down = down, 
                           lag_decrease = 12))
  county_plots <- marrangeGrob(county_plots, 
                               nrow = 6, ncol = 2, 
                               left = "", top = "")
  ggsave(paste("./days_btwn_summary/", 
               "sampling_nchs_", c, ".pdf", sep = ""), 
         county_plots, width = 15, height = 25, units = "cm")
}

# ## generate cumulative effect summary
# county_fit_effect <- matrix(nrow = 500, ncol = 0)
# county_ctr1_effect <- matrix(nrow = 500, ncol = 0)
# county_ctr3_effect <- matrix(nrow = 500, ncol = 0)
# for(f in unique(county_pred$fips)){
#   print(f)
#   county_idx <- which(county_pred$fips == f)
#   fit <- county_fit[, county_idx]
#   fit <- t(apply(fit, 1, cumsum))
#   ctr1 <- county_ctr1[, county_idx]
#   ctr1 <- t(apply(ctr1, 1, cumsum))
#   ctr3 <- county_ctr3[, county_idx]
#   ctr3 <- t(apply(ctr3, 1, cumsum))
  
#   county_fit_effect <- cbind(county_fit_effect, fit)
#   county_ctr1_effect <- cbind(county_ctr1_effect, ctr1)
#   county_ctr3_effect <- cbind(county_ctr3_effect, ctr3) 
# }

# county_pred %<>% 
#   mutate(
#     fit_mu = apply(county_fit_effect, 2, mean),
#     fit_med = apply(county_fit_effect, 2, quantile, probs = 0.5), # use posterior median to hand skewness
#     fit_lo = apply(county_fit_effect, 2, quantile, probs = 0.05),
#     fit_hi = apply(county_fit_effect, 2, quantile, probs = 0.95))

# county_pred %<>% 
#   mutate(
#     ctr1_mu = apply(county_ctr1_effect, 2, mean),
#     ctr1_med = apply(county_ctr1_effect, 2, quantile, probs = 0.5), # use posterior median to hand skewness
#     ctr1_lo = apply(county_ctr1_effect, 2, quantile, probs = 0.05),
#     ctr1_hi = apply(county_ctr1_effect, 2, quantile, probs = 0.95),
#     ctr3_mu = apply(county_ctr3_effect, 2, mean),
#     ctr3_med = apply(county_ctr3_effect, 2, quantile, probs = 0.5), # use posterior median to hand skewness
#     ctr3_lo = apply(county_ctr3_effect, 2, quantile, probs = 0.05),
#     ctr3_hi = apply(county_ctr3_effect, 2, quantile, probs = 0.95))

# for(c in 1:6) {
#   fips_ <- county_pred %>% 
#     distinct(fips, nchs, pop) %>% 
#     filter(nchs == c) %>% 
#     arrange(desc(pop)) %>% 
#     pull(fips)
  
#   name_ <- county_pred %>%
#     filter(fips %in% fips_) %>% 
#     distinct(fips, state, county) %>%
#     mutate(name = paste(state, county))
  
#   county_plots <- lapply(fips_, 
#                          function(x) county_pred %>% 
#                            filter(fips == x) %>% 
#                            gg_days_btwn_sampling(
#                              name = name_$name[name_$fips == x], 
#                              up = up, 
#                              down = down, 
#                              lag_decrease = 12))
#   county_plots <- marrangeGrob(county_plots, 
#                                nrow = 6, ncol = 2, 
#                                left = "", top = "")
#   ggsave(paste("./days_btwn_summary/", 
#                "effect_nchs_", c, ".pdf", sep = ""), 
#          county_plots, width = 15, height = 25, units = "cm")
# }