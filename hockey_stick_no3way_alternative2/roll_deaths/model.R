library(tidyverse)
library(magrittr)
library(feather)
library(rstanarm)

#### #### 
## model

## Read county_train
county_train <- read_feather("../../county_train.feather") %>%
  mutate(t = days_since_thresh, t_squared=days_since_thresh^2)

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
    # global mean and random effects
    poly(days_since_thresh, 2) + (poly(days_since_thresh, 2) | fips) +
    # interaction without mean shift
    t:intervention + t_squared:intervention +
    # nchs
    nchs + t:nchs + t_squared:nchs +
    # effect of intervention speed +
    t:intervention:days_btwn_stayhome_thresh +
    t_squared:intervention:days_btwn_stayhome_thresh
  ,
  offset = log(pop),
  data=county_train,
  algorithm="meanfield",
  iter = 150000,
  adapt_iter = 1000,
  QR=TRUE
)

saveRDS(model, paste("./model.rds", sep = ""))

#### #### 
## county_fit

county_fit <- model %>%
  posterior_predict(county_train, draws = 500)

saveRDS(county_fit, "./county_fit.rds")
