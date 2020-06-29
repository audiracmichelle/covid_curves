## Read
if(!"county_clean" %in% ls()) source("./county_clean.R")
county_features <- county_clean
rm("county_clean")

## Deaths threshold
# when did deaths exceed 3 per 10 million?
county_features$threshold_day <- county_features$date
county_features$threshold_day[county_features$cum_deaths_per_cap < 3/1e7] <- as.Date("2020-12-31")

county_features %<>%
  group_by(fips) %>%
  mutate(threshold_day = min(threshold_day)) %>% 
  ungroup()

county_features$threshold_day[county_features$threshold_day == as.Date("2020-12-31")] <- NA 

# county_features %>%
#   filter(state == "New York",
#          grepl("county", county)) %>%
#   select("date","county","state","fips","cum_cases","cum_deaths","threshold_day") %>% view
 
# use the threshold_day to get the time counter 
county_features %<>%
  mutate(days_since_thresh = as.numeric(date - threshold_day))

## Define variable that is the number of days after the 
## threshold that an area instituded a stay at home
county_features %<>%
  group_by(fips) %>%
  mutate(days_btwn_stayhome_thresh = as.numeric(stayhome - threshold_day)) %>%
  ungroup()

## join with nchs data

nchs <- read_feather("./nchs.feather")

nchs %<>% 
  select(-state, -county)

county_features %<>% 
  left_join(nchs)

#length(unique(county_features$fips))

# county_features %>%
#   filter(state == "Texas",
#          county == "Travis") %>%
#   View()

rm(list=c("nchs"))
