library(xts)

## Read data
source("./county_features.R")
county_deaths_desc_ <- read_feather("./county_deaths_desc_.feather")
county_train <- county_features %>% left_join(county_deaths_desc_)
rm(list = c("county_features",
            "county_cases_desc",
            "county_deaths_desc", 
            "county_deaths_desc_"))

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
#dim(county_train)

## Keep only fips in train sets
county_train_stayhome <- read_feather('./county_train_stayhome.feather')
#dim(county_train_stayhome)
#summary(county_train_stayhome$date[county_train_stayhome$index_desc == 1])

fips_stayhome <- unique(county_train_stayhome$fips)
#length(fips_stayhome)

county_future_stayhome <- county_train %>% 
  filter(fips %in% fips_stayhome) %>% 
  group_by(fips) %>% 
  arrange(date) %>% 
  mutate(index = row_number(), 
         index_desc = sort(index, decreasing = TRUE)) %>% 
  ungroup()
county_future_stayhome <- arrange(county_future_stayhome, fips, index)
#dim(county_future_stayhome)
#summary(county_future_stayhome$date[county_future_stayhome$index_desc == 1])

## Keep only fips in train sets
county_train_decrease <- read_feather('./county_train_decrease.feather')
#dim(county_train_decrease)
#summary(county_train_decrease$date[county_train_decrease$index_desc == 1])

fips_decrease <- unique(county_train_decrease$fips)
#length(fips_decrease)

county_future_decrease <- county_train %>% 
  filter(fips %in% fips_decrease) %>% 
  group_by(fips) %>% 
  arrange(date) %>% 
  mutate(index = row_number(), 
         index_desc = sort(index, decreasing = TRUE)) %>% 
  ungroup()
county_future_decrease <- arrange(county_future_decrease, fips, index)
#dim(county_future_decrease)
summary(county_future_decrease$date[county_future_decrease$index_desc == 1])

write_feather(county_future_stayhome, "./county_future_stayhome.feather")
write_feather(county_future_decrease, "./county_future_decrease.feather")
