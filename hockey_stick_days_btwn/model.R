library(tidyverse)
library(magrittr)
library(feather)
library(rstanarm)

#### #### 
## model

## Read county_train
county_train <- read_feather("../county_train.feather")

## Train model
options(mc.cores=2)
model = stan_glmer.nb(
  deaths ~
    poly(days_since_thresh, 2) * (nchs) + 
    poly(days_since_thresh,2):(intervention + intervention:(days_btwn_stayhome_thresh + nchs)) + 
    (poly(days_since_thresh, 2) | fips),
  offset = log(pop),
  data=county_train,
  algorithm="meanfield",
  iter = 15000,
  adapt_iter = 1000,
  QR=TRUE)

saveRDS(model, paste("./model.rds", sep = ""))

#### ####
## county_fit

county_fit <- model %>%
  posterior_predict(county_train, draws = 200)

saveRDS(county_fit, "./county_fit.rds")
