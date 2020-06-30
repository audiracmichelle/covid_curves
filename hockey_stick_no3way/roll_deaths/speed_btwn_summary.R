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
county_pred$y <- county_pred$roll_deaths
#dim(county_pred)
county_pred %<>% 
  filter(!is.na(roll_deaths))
#dim(county_pred)

## modify values to obtain counterfactual
county_pred$intervention_fit <- county_pred$intervention
county_pred$days_btwn_fit <- county_pred$days_btwn_stayhome_thresh
county_pred$speed_btwn_fit <- county_pred$speed_btwn_stayhome_thresh

county_pred1 <- county_pred
county_pred3 <- county_pred

county_pred1 = county_pred1 %>% 
  mutate(days_btwn_stayhome_thresh = -7) %>% 
  mutate(speed_btwn_stayhome_thresh = cut(days_btwn_stayhome_thresh,
                                          c(-Inf, -10, -3, 3, Inf)))
county_pred3 = county_pred3 %>% 
  mutate(days_btwn_stayhome_thresh = 5) %>% 
  mutate(speed_btwn_stayhome_thresh = cut(days_btwn_stayhome_thresh,
                                          c(-Inf, -10, -3, 3, Inf)))

# shift intervention up or down
county_pred1$intervention <- (county_pred1$days_since_thresh >= -7 + 12) * 1
county_pred3$intervention <- (county_pred3$days_since_thresh >= 5 + 12) * 1
# county_pred1$intervention = as.numeric((county_pred1$date - county_pred1$stayhome) >= 
#                                          (12 + county_pred1$days_btwn_stayhome_thresh - county_pred$days_btwn_stayhome_thresh))
# county_pred3$intervention = as.numeric((county_pred3$date - county_pred3$stayhome) >= 
#                                          (12 + county_pred3$days_btwn_stayhome_thresh - county_pred$days_btwn_stayhome_thresh))
#this should shift the "intervention" date by the #days between threshold and intervention

# county_pred1 %>%
#   select(fips, date, stayhome,
#          intervention_fit, intervention,
#          days_btwn_fit, days_btwn_stayhome_thresh,
#          speed_btwn_fit, speed_btwn_stayhome_thresh) %>%
#   arrange(fips, date) %>%
#   head(1000) %>% view

# county_pred3 %>%
#   select(fips, date, days_since_thresh,
#          intervention_fit, intervention,
#          days_btwn_fit, days_btwn_stayhome_thresh) %>%
#   arrange(fips, date) %>%
#   head(1000) %>% view

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
