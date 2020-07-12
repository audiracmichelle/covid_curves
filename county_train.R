library(xts)

## Read data
source("./county_features.R")
county_train_cases <- county_features %>% left_join(county_cases_desc)
county_train <- county_features %>% left_join(county_deaths_desc)
rm(list = c("county_features",
            "county_cases_desc",
            "county_deaths_desc"))

# use the threshold_day to get the time counter 
county_train_cases %<>%
  mutate(days_since_thresh = as.numeric(date - threshold_day))

county_train %<>%
  mutate(days_since_thresh = as.numeric(date - threshold_day))

## Remove timestamps with negative counts
#length(unique(county_train_cases$fips)); dim(county_train_cases)
county_train_cases <- county_train_cases[-which(county_train_cases$cases < 0), ]
#length(unique(county_train_cases$fips)); dim(county_train_cases)

#length(unique(county_train$fips)); dim(county_train)
county_train <- county_train[-which(county_train$deaths < 0), ]
#length(unique(county_train$fips)); dim(county_train)

## Compute rolling means
county_train_cases %<>% 
  group_by(fips) %>% 
  arrange(date) %>% 
  mutate(roll_cases = rollmean(cases, 7, fill = NA)) %>% 
  ungroup() %>% 
  mutate(roll_cases = round(as.numeric(roll_cases)))

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

#---do not do this
## Include only up to May 15
#sum(is.na(county_train_cases$threshold_day[county_train_cases$index == 1]))
# max(county_train_cases$date)
# county_train_cases %<>% 
#   filter(threshold_day <= as.Date("2020-05-15"),
#          date <= as.Date("2020-05-15"))
#
#sum(is.na(county_train$threshold_day[county_train$index == 1]))
# county_train %<>% 
#   filter(!is.na(threshold_day), 
#          threshold_day <= as.Date("2020-05-15"),
#          date <= as.Date("2020-05-15"))

## Require at least 10 days since threshold
remove_fips <- county_train_cases %>% 
  group_by(fips) %>% 
  summarise(max_days = max(days_since_thresh)) %>% 
  filter(max_days < 10) %>% 
  pull(fips)
county_train_cases <- county_train_cases[!county_train_cases$fips %in% remove_fips, ]
#length(unique(county_train_cases$fips))

remove_fips <- county_train %>% 
  group_by(fips) %>% 
  summarise(max_days = max(days_since_thresh)) %>% 
  filter(max_days < 10) %>% 
  pull(fips)
county_train <- county_train[!county_train$fips %in% remove_fips, ]
#length(unique(county_train$fips))

## Make smaller dataset
cum_cases_ <- county_train_cases %>% 
  group_by(fips) %>% 
  summarise(cum_cases = max(cum_cases)) %>% 
  ungroup() %>% pull(cum_cases)
#summary(cum_cases_)
remove_fips <- county_train_cases %>% 
  group_by(fips) %>% 
  summarise(cum_cases = max(cum_cases)) %>% 
  filter(cum_cases < quantile(cum_cases_, 0.5)) %>% 
  pull(fips)
county_train_cases <- county_train_cases[!county_train_cases$fips %in% remove_fips, ]
#length(unique(county_train_cases$fips))

cum_deaths_ <- county_train %>% 
  group_by(fips) %>% 
  summarise(cum_deaths = max(cum_deaths)) %>% 
  ungroup() %>% pull(cum_deaths)
#summary(cum_deaths_)
remove_fips <- county_train %>% 
  group_by(fips) %>% 
  summarise(cum_deaths = max(cum_deaths)) %>% 
  filter(cum_deaths < quantile(cum_deaths_, 0.5)) %>% 
  pull(fips)
county_train <- county_train[!county_train$fips %in% remove_fips, ]
#length(unique(county_train$fips))

## Remove rows with negative days_since_thresh
# county_train_cases$fips[county_train_cases$days_since_thresh < 0]
# county_train_cases %>% 
#   filter(fips == "06073") %>% 
#   select(fips, county, state, date,
#          cum_cases, cum_cases_per_cap, threshold_day, days_since_thresh,
#          decrease_40_total_visiting, days_btwn_decrease_thresh) %>% view

#sum(county_train_cases$days_since_thresh < 0)
county_train_cases %<>%
  filter(days_since_thresh >= 0)

#sum(county_train$days_since_thresh < 0)
county_train %<>%
  filter(days_since_thresh >= 0)

#---do not do this
## Remove counties that did not put the intervention in place
# county_train_cases %>%
#   filter(index == 1) %>%
#   pull(decrease_40_total_visiting) %>% is.na %>% sum
# county_train_cases %<>%
#   filter(!is.na(decrease_40_total_visiting))
#length(unique(county_train_cases$fips))
#
# county_train %>%
#   filter(index == 1) %>%
#   pull(stayhome) %>% is.na %>% sum
# county_train %<>%
#   filter(!is.na(stayhome))
#length(unique(county_train$fips))

## Create index columns
county_train_cases %<>% 
  group_by(fips) %>% 
  arrange(date) %>% 
  mutate(index = row_number(), 
         index_desc = sort(index, decreasing = TRUE)) %>% 
  ungroup()
county_train_cases <- arrange(county_train_cases, fips, index)

county_train %<>% 
  group_by(fips) %>% 
  arrange(date) %>% 
  mutate(index = row_number(), 
         index_desc = sort(index, decreasing = TRUE)) %>% 
  ungroup()
county_train <- arrange(county_train, fips, index)

# county_train_cases %>%
#   filter(state == "New York",
#          grepl("county", county)) %>% 
#   arrange(fips, date) %>% view

rm(list=c("cum_cases_",
          "cum_deaths_", 
          "remove_fips"))
write_feather(county_train_cases, "./county_train_cases.feather")
write_feather(county_train, "./county_train.feather")
