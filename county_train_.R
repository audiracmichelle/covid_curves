library(xts)

## Read data
source("./county_features.R")
county_train <- county_features %>% left_join(county_deaths_desc)
rm(list = c("county_features",
            "county_cases_desc",
            "county_deaths_desc"))

# use the threshold_day to get the time counter 
county_train %<>%
  mutate(days_since_thresh = as.numeric(date - threshold_day))

## Remove timestamps with negative counts
#length(unique(county_train$fips)); dim(county_train)
county_train <- county_train[-which(county_train$deaths < 0), ]
#length(unique(county_train$fips)); dim(county_train)

## Compute rolling means
county_train %<>% 
  group_by(fips) %>% 
  arrange(date) %>% 
  mutate(roll_deaths = rollmean(deaths, 7, fill = NA)) %>% 
  ungroup() %>% 
  mutate(roll_deaths = round(as.numeric(roll_deaths)))

## Remove rows with negative days_since_thresh
#sum(county_train$days_since_thresh < 0)
county_train %<>%
  filter(days_since_thresh >= 0)

## define y
#length(unique(county_train$fips))
county_train %<>%  
  mutate(y = roll_deaths,  
         intrv_stayhome = (date - stayhome >= 12) * 1,  
         intrv_decrease = (date - decrease_50_total_visiting >= 12) * 1,
         days_since_intrv_stayhome = as.numeric(date - stayhome - 12 + 1), 
         days_since_intrv_decrease = as.numeric(date - decrease_50_total_visiting - 12 + 1),
         age_65_plus = log(1e4 * age_65_plus / pop), 
         black = log(1e4 * black / pop), 
         hispanic = log(1e4 * hispanic / pop), 
         days_btwn_stayhome_thresh = as.numeric(stayhome - threshold_day), 
         days_btwn_decrease_thresh = as.numeric(decrease_50_total_visiting - threshold_day)
  ) %>%
  filter(!is.na(y) 
  )
#length(unique(county_train$fips))

## Require at least 17 days after intervention + lag
county_train_stayhome <- county_train %>% 
  filter(!is.na(stayhome), days_since_intrv_stayhome <= 17)
#length(unique(county_train_stayhome$fips))

county_train_decrease <- county_train %>% 
  filter(!is.na(decrease_50_total_visiting), days_since_intrv_decrease <= 17)
#length(unique(county_train_decrease$fips))

## Require counties to have a baseline level in safegraph data
county_train_stayhome %<>% filter(!is.na(baseline_total_visiting))
#length(unique(county_train_stayhome$fips))

county_train_decrease %<>% filter(!is.na(baseline_total_visiting))
#length(unique(county_train_decrease$fips))

## Require at least 7 days since threshold
remove_fips <- county_train_stayhome %>% 
  group_by(fips) %>% 
  summarise(max_days = max(days_since_thresh)) %>% 
  filter(max_days < 7) %>% 
  pull(fips)
county_train_stayhome <- county_train_stayhome[!county_train_stayhome$fips %in% remove_fips, ]
#length(unique(county_train_stayhome$fips))

remove_fips <- county_train_decrease %>% 
  group_by(fips) %>% 
  summarise(max_days = max(days_since_thresh)) %>% 
  filter(max_days < 7) %>% 
  pull(fips)
county_train_decrease <- county_train_decrease[!county_train_decrease$fips %in% remove_fips, ]
#length(unique(county_train_decrease$fips))

## Require at least 5 deaths
cum_deaths_ <- county_train_stayhome %>% 
  group_by(fips) %>% 
  summarise(cum_deaths = max(cum_deaths)) %>% 
  ungroup() %>% pull(cum_deaths)
#summary(cum_deaths_); quantile(cum_deaths_, 0.6)
cum_deaths_ <- county_train_decrease %>% 
  group_by(fips) %>% 
  summarise(cum_deaths = max(cum_deaths)) %>% 
  ungroup() %>% pull(cum_deaths)
#summary(cum_deaths_); quantile(cum_deaths_, 0.6)

remove_fips <- county_train_stayhome %>% 
  group_by(fips) %>% 
  summarise(cum_deaths = max(cum_deaths)) %>% 
  filter(cum_deaths <= 5) %>% 
  pull(fips)
county_train_stayhome <- county_train_stayhome[!county_train_stayhome$fips %in% remove_fips, ]
#length(unique(county_train_stayhome$fips))

remove_fips <- county_train_decrease %>% 
  group_by(fips) %>% 
  summarise(cum_deaths = max(cum_deaths)) %>% 
  filter(cum_deaths <= 5) %>% 
  pull(fips)
county_train_decrease <- county_train_decrease[!county_train_decrease$fips %in% remove_fips, ]
#length(unique(county_train_decrease$fips))

rm(list=c("cum_deaths_", 
          "remove_fips"))

## Create index columns
county_train_stayhome %<>% 
  group_by(fips) %>% 
  arrange(date) %>% 
  mutate(index = row_number(), 
         index_desc = sort(index, decreasing = TRUE)) %>% 
  ungroup()
county_train_stayhome <- arrange(county_train_stayhome, fips, index)

county_train_decrease %<>% 
  group_by(fips) %>% 
  arrange(date) %>% 
  mutate(index = row_number(), 
         index_desc = sort(index, decreasing = TRUE)) %>% 
  ungroup()
county_train_decrease <- arrange(county_train_decrease, fips, index)

write_feather(county_train_stayhome, "./county_train_stayhome.feather")
write_feather(county_train_decrease, "./county_train_decrease.feather")
