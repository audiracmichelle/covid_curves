library(tidyverse)
library(magrittr)
library(feather)
library(xts)

## Read data
source("./county_features.R")
county_train <- county_features
rm("county_features")

## Remove timestamps with negative counts
#length(unique(county_train$fips))
county_train <- county_train[-which(county_train$deaths < 0), ]
#length(unique(county_train$fips))

## Compute rolling means
county_train %<>% 
  group_by(fips) %>% 
  arrange(date) %>% 
  mutate(roll_deaths = rollmean(deaths, 7, fill = NA)) %>% 
  ungroup() %>% 
  mutate(roll_deaths = round(as.numeric(roll_deaths)))

#---do not do this
## Compute cumulative cases and deaths
#summary(county_train$cum_cases)
# county_train %<>%  
#   group_by(fips) %>% 
#   arrange(date) %>% 
#   mutate(cum_cases = cumsum(cases), 
#          cum_deaths = cumsum(deaths)) %>% 
#   ungroup()
#summary(county_train$cum_cases)

## Include only deaths up to May 15
county_train %<>% 
  filter(!is.na(threshold_day), 
         threshold_day <= as.Date("2020-05-21"),
         date <= as.Date("2020-05-21"))

## Require at least 10 days since threshold
remove_fips <- county_train %>% 
  group_by(fips) %>% 
  summarise(max_days = max(days_since_thresh)) %>% 
  filter(max_days < 10) %>% 
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

## Create index columns

county_train %<>% 
  group_by(fips) %>% 
  arrange(date) %>% 
  mutate(index = row_number(), 
         index_desc = sort(index, decreasing = TRUE)) %>% 
  ungroup()

rm(list=c("cum_deaths_", 
          "remove_fips"))
write_feather(county_train, "./county_train.feather")
