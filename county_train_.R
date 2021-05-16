library(xts)

####
## Read data
source("./county_features.R")
county_deaths_desc_ <- read_feather("./county_deaths_desc_.feather")
county_train <- county_features %>% left_join(county_deaths_desc_)
rm(list = c("county_features",
            "county_cases_desc",
            "county_deaths_desc", 
            "county_deaths_desc_"))

####
## Use the threshold_day to get the time counter 
county_train %<>%
  mutate(days_since_thresh = as.numeric(date - threshold_day))

####
## Remove timestamps with negative counts
#length(unique(county_train$fips)); dim(county_train)
county_train$deaths[which(county_train$deaths < 0)] <- NA
#length(unique(county_train$fips)); dim(county_train)

####
## Compute rolling means
county_train %<>% 
  group_by(fips) %>% 
  arrange(date) %>% 
  mutate(roll_deaths = rollmean(deaths, 7, na.rm = TRUE, fill = NA)) %>% 
  ungroup() %>% 
  mutate(roll_deaths = round(as.numeric(roll_deaths)))

####
## Remove rows with negative days_since_thresh
#sum(county_train$days_since_thresh < 0)
county_train %<>%
  filter(days_since_thresh >= 0)

####
## define y
#length(unique(county_train$fips))
county_train %<>%  
  mutate(y = roll_deaths,  
         days_since_intrv_stayhome = as.numeric(date - stayhome + 1), 
         days_since_intrv_decrease = as.numeric(date - decrease_50_total_visiting + 1),
         age_65_plus = log(1e4 * age_65_plus / pop), 
         black = log(1e4 * black / pop), 
         hispanic = log(1e4 * hispanic / pop), 
         days_btwn_stayhome_thresh = as.numeric(stayhome - threshold_day), 
         days_btwn_decrease_thresh = as.numeric(decrease_50_total_visiting - threshold_day)
  ) %>%
  filter(!is.na(y) 
  )
#length(unique(county_train$fips))

####
## Create index columns
county_train <- county_train %<>% 
  group_by(fips) %>% 
  arrange(date) %>% 
  mutate(index = row_number(), 
         index_desc = sort(index, decreasing = TRUE)) %>% 
  ungroup()
county_train <- arrange(county_train, fips, index)

county_train %<>%
  select(fips, county, state, date,
         cum_cases, cum_deaths, 
         index, index_desc,
         pop, popdensity, 
         stayhome, gt50, gt500, schools, restaurants, entertainment, 
         contains("baseline"), contains("decrease"), 
         threshold_day, days_btwn_stayhome_thresh, days_btwn_decrease_thresh, 
         decrease_on_stayhome, min_decrease, 
         days_since_thresh, days_since_intrv_stayhome, days_since_intrv_decrease, 
         college, age_65_plus, black, hispanic,
         y)

write_feather(county_train, "./county_train_.feather")
