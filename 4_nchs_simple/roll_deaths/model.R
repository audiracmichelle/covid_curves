library(tidyverse)
library(magrittr)
library(feather)
library(rstanarm)

#### #### 
## model

## Read county_train
county_train <- read_feather("../../county_train.feather") %>%
  mutate(t=days_since_thresh, t2=days_since_thresh^2)

## define y
county_train$y <- county_train$roll_deaths
#dim(county_train)
county_train %<>% 
  filter(!is.na(roll_deaths))
#dim(county_train)

## Train model
options(mc.cores=2)
model = stan_glmer.nb(
  y ~
    # 1 baseline
    # potential bug in rstan: we use days_since_tresh
    # instead of t
    poly(days_since_thresh, 2) +
    (poly(days_since_thresh, 2) | fips) +
    # 2 interaction
    t:intervention +
    t2:intervention +
    # 3 earliness
    t:intervention:days_btwn_stayhome_thresh +
    t2:intervention:days_btwn_stayhome_thresh +
    # 4 nchs simple
    nchs +
    t:nchs +
    t2:nchs # +
    # 5 nchs interaction
    # t:intervention:nchs +
    # t2:intervention:nchs
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
