## Read
if(!"county_clean" %in% ls()) source("./county_clean.R")
county_features <- county_clean
rm("county_clean")

## get county_descriptors
#length(unique(county_features$fips))
county_descriptors <- county_features %>% 
  filter(index == 1) %>% 
  select(-date, 
         -cum_cases, -cum_deaths, 
         -cases, -deaths, 
         -cum_cases_per_sq_mi, -cum_deaths_per_sq_mi, 
         -cum_cases_per_cap, -cum_deaths_per_cap, 
         -index, -index_desc)

## join with cc
cc <- read_feather("./cc.feather")

# length(setdiff(x = unique(county_descriptors$fips), y = unique(cc$fips)))

county_descriptors <- left_join(county_descriptors, cc)

## join with safegr data
safegr <- read_feather("./safegr.feather")

# length(setdiff(x = unique(county_descriptors$fips), y = unique(safegr$fips)))

county_descriptors %<>% 
  left_join(safegr %>% 
              select(fips, key, baseline, decrease_40, decrease_50) %>% 
              filter(key %in% c("school_visits", 
                                "recreation_visiting", 
                                "total_visiting")) %>% 
              pivot_wider(names_from = key, values_from = -c(fips, key)))

## join with nchs data

nchs <- read_feather("./nchs.feather")

nchs %<>% 
  select(-state, -county)

county_descriptors %<>% 
  left_join(nchs)
#dim(county_descriptors)

rm(list=c("cc", 
          "safegr", 
          "nchs"))

## create county_deaths_desc
## get deaths threshold, when did deaths exceed 3 per 10 million?
county_features$threshold_day <- county_features$date
county_features$threshold_day[county_features$cum_deaths_per_cap < 3/1e7] <- as.Date("2020-12-31")

county_features %<>%
  group_by(fips) %>%
  mutate(threshold_day = min(threshold_day)) %>% 
  ungroup()

county_features$threshold_day[county_features$threshold_day == as.Date("2020-12-31")] <- NA 

county_deaths_desc <- county_descriptors %>% 
  left_join(distinct(county_features, fips, threshold_day)) %>%
  mutate(days_btwn_stayhome_thresh = as.numeric(stayhome - threshold_day), 
         days_btwn_decrease_thresh = as.numeric(decrease_40_total_visiting - threshold_day))
#length(unique(county_deaths_desc$fips))

## create county_cases_desc
## get cases threshold, when did cases exceed 30 per 10 million?
county_features$threshold_day <- county_features$date
county_features$threshold_day[county_features$cum_cases_per_cap < 30/1e7] <- as.Date("2020-12-31")

county_features %<>%
  group_by(fips) %>%
  mutate(threshold_day = min(threshold_day)) %>% 
  ungroup()

county_features$threshold_day[county_features$threshold_day == as.Date("2020-12-31")] <- NA 

county_cases_desc <- county_descriptors %>% 
  left_join(distinct(county_features, fips, threshold_day)) %>%
  mutate(days_btwn_stayhome_thresh = as.numeric(stayhome - threshold_day), 
         days_btwn_decrease_thresh = as.numeric(decrease_40_total_visiting - threshold_day))
#length(unique(county_cases_desc$fips))

## get difference in days between cases threshold and deaths threashold for each county
xx <- county_cases_desc %>% 
  rename(threshold_cases = threshold_day) %>%
  select(fips, threshold_cases) %>% 
  left_join(county_deaths_desc %>% 
              rename(threshold_deaths = threshold_day) %>% 
              select(fips, threshold_deaths)) %>% 
  mutate(diff_thresh = as.numeric(threshold_deaths - threshold_cases)) %>% 
  select(fips, diff_thresh)

county_cases_desc %<>%
  left_join(xx)

county_deaths_desc %<>%
  left_join(xx)

rm(list = c("county_descriptors", 
            "xx"))

## get county_features
#dim(county_features)
county_features %<>% 
  select(fips, county, state, 
         date, 
         cum_cases, cum_deaths, 
         cases, deaths, cum_deaths_per_cap, cum_deaths_per_sq_mi, cum_cases_per_cap, cum_cases_per_sq_mi, 
         index, index_desc)
#dim(county_features)

# county_features %>%
#   filter(state == "New York",
#          grepl("county", county)) %>% view()

# county_deaths_desc %>%
#   filter(state == "Texas",
#          county == "Travis") %>%
#   select(fips, county, state, stayhome, threshold_day, days_btwn_stayhome_thresh) %>%
#   View()

# county_cases_desc %>%
#   filter(state == "Texas",
#          county == "Travis") %>%
#   select(fips, county, state, stayhome, threshold_day, days_btwn_stayhome_thresh) %>%
#   View()
