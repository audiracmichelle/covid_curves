library(tidyverse)
library(magrittr)
library(feather)
library(rstanarm)
library(gridExtra)

## Read data
county_pred <- read_feather("../../county_train.feather") %>%
  mutate(t = days_since_thresh, t2 = days_since_thresh^2)
model <- readRDS("./model.rds")
county_fit <- readRDS("./county_fit.rds")
source("../../plot_foo.R")

up = 7
down = -14

## define y
county_pred$y <- county_pred$roll_deaths
county_pred$cum_y <- county_pred$cum_deaths
#dim(county_pred)
county_pred %<>% 
  filter(!is.na(roll_deaths))
#dim(county_pred)

## modify values to obtain counterfactual
county_pred$intervention_fit <- county_pred$intervention
county_pred$days_btwn_fit <- county_pred$days_btwn_stayhome_thresh

county_pred1 = county_pred %>% 
  mutate(days_btwn_stayhome_thresh = days_btwn_stayhome_thresh + down) %>%
  mutate(intervention = as.numeric(date >= stayhome + 12 + down))

county_pred3 = county_pred %>% 
  mutate(days_btwn_stayhome_thresh = days_btwn_stayhome_thresh + up) %>%
  mutate(intervention = as.numeric(date >= stayhome + 12 + up))

## get posteriors

county_ctr1 <- model %>% 
  posterior_predict(county_pred1, draws = 500)
county_ctr3 <- model %>% 
  posterior_predict(county_pred3, draws = 500)


## generate nchs sampling summaries

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

for(c in 1:6) {
  fips_ <- county_pred %>% 
    filter(index == 1, nchs == c) %>% 
    arrange(desc(days_btwn_stayhome_thresh), desc(pop)) %>% 
    pull(fips)
  
  county_plots <- lapply(fips_, 
                         function(x) county_pred %>% 
                           filter(fips == x) %>% 
                           gg_days_btwn_sampling(up = up, down = down))
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

for(c in 1:6) {
  fips_ <- county_pred %>% 
    filter(index == 1, nchs == c) %>% 
    arrange(desc(days_btwn_stayhome_thresh), desc(pop)) %>% 
    pull(fips)
  
  county_plots <- lapply(fips_, 
                         function(x) county_pred %>% 
                           filter(fips == x) %>% 
                           gg_days_btwn_effect(up=up, down=down))
  county_plots <- marrangeGrob(county_plots, 
                               nrow = 6, ncol = 2, 
                               left = "", top = "")
  ggsave(paste("./speed_btwn_summary/", 
               "effect_nchs_", c, ".pdf", sep = ""), 
         county_plots, width = 15, height = 25, units = "cm")
  
}

## aggregate by nchs
nchs_pred <- county_pred %>% 
  group_by(nchs, days_since_thresh) %>% 
  summarise(log_y = median(log(1e-2 + county_pred$y / county_pred$pop  * 1e5)), 
            fit_mu = NA, fit_med = NA, fit_lo = NA, fit_hi = NA, 
            ctr1_mu = NA, ctr1_med = NA, ctr1_lo = NA, ctr1_hi = NA,
            ctr3_mu = NA, ctr3_med = NA, ctr3_lo = NA, ctr3_hi = NA,)

county_log_fit = county_fit
county_log_ctr1 = county_ctr1
county_log_ctr3 = county_ctr3
for (r in 1:500) {
  county_log_fit[r, ] = log(1e-2 + county_log_fit[r, ] / county_pred$pop * 1e5)
  county_log_ctr1[r, ] = log(1e-2 + county_log_ctr1[r, ] / county_pred$pop * 1e5)
  county_log_ctr3[r, ] = log(1e-2 + county_log_ctr3[r, ] / county_pred$pop * 1e5)
}

for(n in unique(nchs_pred$nchs)){
  days <- nchs_pred$days_since_thresh[which(nchs_pred$nchs == n)]
  for(d in days) {
    county_idx <- which(county_pred$nchs == n & 
                          county_pred$days_since_thresh == d)
    if(length(county_idx) > 1) {
      fit <- apply(county_log_fit[, county_idx], 1 , mean)
      ctr1 <- apply(county_log_ctr1[, county_idx], 1 , mean)
      ctr3 <- apply(county_log_ctr3[, county_idx], 1 , mean)
    } else {
      fit <- county_log_fit[, county_idx]
      ctr1 <- county_log_ctr1[, county_idx]
      ctr3 <- county_log_ctr3[, county_idx]
    }
    nchs_idx <- which(nchs_pred$nchs == n & 
                        nchs_pred$days_since_thresh == d)
    
    nchs_pred$fit_mu[nchs_idx] <- mean(fit)
    nchs_pred$fit_med[nchs_idx] <- quantile(fit, 0.5)
    nchs_pred$fit_lo[nchs_idx] <- quantile(fit, 0.05)
    nchs_pred$fit_hi[nchs_idx] <- quantile(fit, 0.95)
    
    nchs_pred$ctr1_mu[nchs_idx] <- mean(ctr1)
    nchs_pred$ctr1_med[nchs_idx] <- quantile(ctr1, 0.5)
    nchs_pred$ctr1_lo[nchs_idx] <- quantile(ctr1, 0.05)
    nchs_pred$ctr1_hi[nchs_idx] <- quantile(ctr1, 0.95)
    
    nchs_pred$ctr3_mu[nchs_idx] <- mean(ctr3)
    nchs_pred$ctr3_med[nchs_idx] <- quantile(ctr3, 0.5)
    nchs_pred$ctr3_lo[nchs_idx] <- quantile(ctr3, 0.05)
    nchs_pred$ctr3_hi[nchs_idx] <- quantile(ctr3, 0.95)
  }
}

county_plots <- lapply(1:6, 
                       function(x) nchs_pred %>% 
                         filter(nchs == x) %>% 
                         gg_nchs_sampling(n_ = x, up=up, down=down))
county_plots <- marrangeGrob(county_plots, 
                             nrow = 6, ncol = 2, 
                             left = "", top = "")
ggsave(paste("./speed_btwn_summary/", 
             "nchs_sampling.pdf", sep = ""), 
       county_plots, width = 15, height = 25, units = "cm")
