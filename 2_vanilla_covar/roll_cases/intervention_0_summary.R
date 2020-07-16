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
                           lag_decrease = 5, 
                           lag_stayhome = 5))
  county_plots <- marrangeGrob(county_plots, 
                               nrow = 6, ncol = 2, 
                               left = "", top = "")
  ggsave(paste("./intervention_0_summary/", 
               "sampling_nchs_", c, ".pdf", sep = ""), 
         county_plots, width = 15, height = 25, units = "cm")
}

# ## aggregate by nchs
# nchs_pred <- county_pred %>% 
#   group_by(nchs) %>% 
#   mutate(days_btwn_decrease_thresh = median(days_btwn_decrease_thresh)) %>% 
#   group_by(nchs, days_since_thresh, days_btwn_decrease_thresh) %>% 
#   summarise(y = log(median(1e-2 + county_pred$y / county_pred$pop  * 1e5)), 
#             fit_mu = NA, fit_med = NA, fit_lo = NA, fit_hi = NA, 
#             ctr1_mu = NA, ctr1_med = NA, ctr1_lo = NA, ctr1_hi = NA,
#             ctr3_mu = NA, ctr3_med = NA, ctr3_lo = NA, ctr3_hi = NA)

# county_log_fit = county_fit
# county_log_ctr = county_ctr
# for (r in 1:500) {
#   county_log_fit[r, ] = log(1e-2 + county_log_fit[r, ] / county_pred$pop * 1e5)
#   county_log_ctr[r, ] = log(1e-2 + county_log_ctr[r, ] / county_pred$pop * 1e5)
# }

# for(n in unique(nchs_pred$nchs)){
#   days <- nchs_pred$days_since_thresh[which(nchs_pred$nchs == n)]
#   for(d in days) {
#     county_idx <- which(county_pred$nchs == n & 
#                           county_pred$days_since_thresh == d)
#     if(length(county_idx) > 1) {
#       fit <- apply(county_log_fit[, county_idx], 1 , mean)
#       ctr <- apply(county_log_ctr[, county_idx], 1 , mean)
#     } else {
#       fit <- county_log_fit[, county_idx]
#       ctr <- county_log_ctr1[, county_idx]
#     }
#     nchs_idx <- which(nchs_pred$nchs == n & 
#                         nchs_pred$days_since_thresh == d)
    
#     nchs_pred$fit_mu[nchs_idx] <- mean(fit)
#     nchs_pred$fit_med[nchs_idx] <- quantile(fit, 0.5)
#     nchs_pred$fit_lo[nchs_idx] <- quantile(fit, 0.05)
#     nchs_pred$fit_hi[nchs_idx] <- quantile(fit, 0.95)
    
#     nchs_pred$ctr_mu[nchs_idx] <- mean(ctr)
#     nchs_pred$ctr_med[nchs_idx] <- quantile(ctr, 0.5)
#     nchs_pred$ctr_lo[nchs_idx] <- quantile(ctr, 0.05)
#     nchs_pred$ctr_hi[nchs_idx] <- quantile(ctr, 0.95)
#   }
# }

# county_plots <- lapply(1:6, 
#                        function(x) nchs_pred %>% 
#                          filter(nchs == x)%>% 
#                          gg_intrv_agg_sampling(name = x, 
#                                            intrv_name = "decrease", 
#                                            lag = 5))
# county_plots <- marrangeGrob(county_plots, 
#                              nrow = 6, ncol = 2, 
#                              left = "", top = "")
# ggsave(paste("./intervention_0_summary/", 
#              "nchs_sampling.pdf", sep = ""), 
#        county_plots, width = 15, height = 25, units = "cm")
