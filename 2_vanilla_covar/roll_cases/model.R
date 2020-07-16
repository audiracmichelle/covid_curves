library(tidyverse)
library(magrittr)
library(feather)
library(rstanarm)

## Read county_train
county_train <- read_feather("../../county_train_cases.feather")

## define y
#length(unique(county_train$fips))
county_train %<>%  
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
#length(unique(county_train$fips))

# county_train %>%
#   select(fips, date, days_since_thresh, intrv_decrease) %>%
#   arrange(fips, date) %>%
#   head(1000) %>% view

## Train model
model = stan_glmer.nb(
  y ~
    poly(days_since_thresh, 2) * (nchs + college + age_65_plus + black + hispanic) + 
    (poly(days_since_thresh, 2) | fips) +
    days_since_intrv_decrease:intrv_decrease + 
    I(days_since_intrv_decrease^2):intrv_decrease
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
