library(tidyverse)
library(magrittr)
library(feather)
library(rstanarm)

## Read county_train
county_train <- read_feather("../../county_train_cases.feather")

## define y
county_train %<>% 
  mutate(y = roll_cases) %>%  
  filter(!is.na(y), 
         days_since_thresh <= 60)

# county_train %>%
#   select(fips, date, days_since_thresh, intrv_decrease, intrv_stayhome) %>%
#   arrange(fips, date) %>%
#   head(1000) %>% view

## Train model
model = stan_glmer.nb(
  y ~
    # random effects
    (poly(days_since_thresh, 2) | fips) +
    # 2 interaction
    poly(days_since_thresh, 2) * (intrv_decrease) + 
    poly(days_since_thresh, 2) * (intrv_stayhome)
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
