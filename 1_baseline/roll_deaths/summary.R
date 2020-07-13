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
  mutate(y = roll_deaths) %>%
    filter(!is.na(y))
#length(unique(county_pred$fips))

## obtain distribution values from fit sampling
county_pred %<>% 
  mutate(
    fit_mu = apply(county_fit, 2, mean),
    fit_med = apply(county_fit, 2, quantile, probs = 0.5), # use posterior median to hand skewness
    fit_lo = apply(county_fit, 2, quantile, probs = 0.05),
    fit_hi = apply(county_fit, 2, quantile, probs = 0.95))

## generate nchs summaries

for(c in 1:6) {
    fips_ <- county_pred %>% 
      distinct(fips, nchs, popdensity) %>% 
      filter(nchs == c) %>% 
      arrange(desc(popdensity)) %>% 
      pull(fips)
  
    name_ <- county_pred %>%
      filter(fips %in% fips_) %>% 
      distinct(fips, state, county) %>%
      mutate(name = paste(state, county))

  county_plots <- lapply(fips_, 
                         function(x) county_pred %>% 
                           filter(fips == x) %>% 
                           gg_fit_sampling(
                           name = name_$name[name_$fips == x], 
                           lag_decrease = 5, 
                           lag_stayhome = 5))
  county_plots <- marrangeGrob(county_plots, 
                               nrow = 6, ncol = 2, 
                               left = "", top = "")
  ggsave(paste("./summary/", 
               "sampling_nchs_", c, ".pdf", sep = ""), 
         county_plots, width = 15, height = 25, units = "cm")
}

