library(tidyverse)
library(magrittr)
library(feather)
library(rstanarm)
#library(gridExtra)

## Read data
model <- readRDS("./model.rds")

## Read county_future
county_pred <- read_feather("../../county_future_stayhome.feather")
#dim(county_pred)
#summary(county_pred$date[county_pred$index_desc == 1])

county_pred %<>%
  filter(date <= as.Date("2020-05-05"))
#dim(county_pred)

## modify values to obtain counterfactual
for (t in seq(2,14,2)) {
  up = t
  down = -t

county_pred1 = county_pred %>% 
  mutate(days_btwn_stayhome_thresh = days_btwn_stayhome_thresh + up) %>%
  mutate(intrv_stayhome = as.numeric(date >= stayhome + 12 + up))

county_pred3 = county_pred %>% 
  mutate(days_btwn_stayhome_thresh = days_btwn_stayhome_thresh + down) %>%
  mutate(intrv_stayhome = as.numeric(date >= stayhome + 12 + down))

county_pred1$days_since_intrv_stayhome <- county_pred1$days_since_intrv_stayhome - up
county_pred3$days_since_intrv_stayhome <- county_pred3$days_since_intrv_stayhome - down

## get posteriors
county_future_ctr1 <- model %>% 
  posterior_predict(county_pred1, draws = 500)
county_future_ctr3 <- model %>% 
  posterior_predict(county_pred3, draws = 500)

saveRDS(county_future_ctr1, 
        paste0("./sweetspot/county_late_", 
               t, ".rds"))
saveRDS(county_future_ctr3, 
        paste0("./sweetspot/county_early_", 
               t, ".rds"))
}