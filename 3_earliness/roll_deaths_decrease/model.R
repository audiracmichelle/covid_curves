library(tidyverse)
library(magrittr)
library(feather)
library(rstanarm)

## Read county_train
county_train <- read_feather("../../county_train.feather")

## define y
#length(unique(county_train$fips))
county_train %<>%  
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
#length(unique(county_train$fips))

# county_train %>%
#   select(fips, date, days_since_thresh, intrv_stayhome) %>%
#   arrange(fips, date) %>%
#   head(1000) %>% view

## Train model
model = stan_glmer.nb(
  y ~
    poly(days_since_thresh, 2) * (nchs + college + age_65_plus + black + hispanic) + 
    (poly(days_since_thresh, 2) | fips) +
    days_since_intrv_decrease:intrv_decrease + 
    I(days_since_intrv_decrease^2):intrv_decrease + 
    days_since_intrv_decrease:intrv_decrease:days_btwn_decrease_thresh +
    I(days_since_intrv_decrease^2):intrv_decrease:days_btwn_decrease_thresh
  ,
  offset = log(pop),
  data=county_train,
  algorithm="meanfield",
  iter = 50000,
  adapt_iter = 2500,
  QR=TRUE
)

saveRDS(model, paste("./model.rds", sep = ""))

#### #### 
## county_fit

county_fit <- model %>%
  posterior_predict(county_train, draws = 500)

saveRDS(county_fit, "./county_fit.rds")
