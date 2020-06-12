library(tidyverse)
library(magrittr)
library(feather)
library(lubridate)

## Read data
county_train <- read_feather("./county_features.feather")

## Include only deaths up to May 15
county_train %<>% 
  filter(!is.na(threshold_day), 
         threshold_day <= as.Date("2020-05-15"),
         date <= as.Date("2020-05-15"))

## Require at least 25 days since threshold
remove_fips <- county_train %>% 
  group_by(fips) %>% 
  summarise(max_days = max(days_since_thresh)) %>% 
  filter(max_days < 25) %>% 
  pull(fips)

county_train <- county_train[!county_train$fips %in% remove_fips, ]
#length(unique(county_train$fips))

## Make smaller dataset
cum_deaths_ <- county_train %>% 
  group_by(fips) %>% 
  summarise(cum_deaths = max(cum_deaths)) %>% 
  ungroup() %>% pull(cum_deaths)
#summary(cum_deaths_)

remove_fips <- county_train %>% 
  group_by(fips) %>% 
  summarise(cum_deaths = max(cum_deaths)) %>% 
  filter(cum_deaths < quantile(cum_deaths_, 0.6)) %>% 
  pull(fips)

county_train <- county_train[!county_train$fips %in% remove_fips, ]
#length(unique(county_train$fips))

## Create intervention dummy covariate
county_train %<>% 
  mutate(intervention = (date - stayhome >= 12) * 1)

## Remove rows with negative days_since_thresh
county_train %<>%
  filter(days_since_thresh >= 0)

## Remove counties that did not put the intervention in place
county_train %<>%
  filter(!is.na(stayhome))
#length(unique(county_train$fips))

## Number of timestamps per days since thresholds with intervention
# county_train %>%
#   filter(intervention == 1) %>%
#   ggplot(aes(x = days_since_thresh)) +
#   geom_bar()

## Number of timestamps per days since thresholds without intervention
# county_train %>%
#   filter(intervention == 0) %>%
#   ggplot(aes(x = days_since_thresh)) +
#   geom_bar()

# county_train %>%
#   filter(state == "New York",
#          grepl("county", county)) %>% view

write_feather(county_train, "./county_train.feather")
