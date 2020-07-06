library(tidyverse)
library(magrittr)
library(feather)

#drive url: https://drive.google.com/open?id=1g4q8Kzphax-VRjiBSYEztn1e55GNgcnd

## Visits
#id <- "14N3UbhEnw2Th2IoAkvHm8qJ_2ozB-k2D"
#visits <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
visits <- read_csv("https://www.dropbox.com/s/ulph1gv18qss832/visits_by_category_bycounty_baselined.csv?dl=1")

visits %<>% 
  rename(fips = fips5d, 
         key = top_category, 
         value = count) %>%
  select(fips, date, key, value)

## total visits
visits$key <- as.factor(visits$key)
levels(visits$key) <- c("college_visits",
                        "bar_visits",             
                        "school_visits",                 
                        "hospital_visits",          
                        "grocery_visits",                                      
                        "public_recreation_visits",
                        "restaurant_visits")

## remove counties with duplicated entries
remove_fips <- visits %>% 
  group_by(fips, date, key) %>% 
  summarise(n = n()) %>% 
  filter(n > 1) %>% 
  pull(fips) %>% unique()

visits %<>% 
  filter(!fips %in% remove_fips)

visits_ <- visits %>% 
  distinct(fips, date)

for(k in levels(visits$key)){
  v <- visits %>% 
    filter(key == k) %>% 
    select(-key) 
  names(v)[3] <- k
  visits_ %<>% left_join(v) 
}
visits <- visits_ %>%
  arrange(fips, date)

rm(list = "v")
rm(list = "visits_")

visits$total_visiting <- apply(cbind(visits$college_visits, 
                                        visits$bar_visits, 
                                        visits$school_visits, 
                                        visits$public_recreation_visits, 
                                        visits$restaurant_visits), 
                                  1, sum, na.rm = TRUE)

visits$school_college_visiting <- apply(cbind(visits$college_visits, 
                                        visits$school_visits), 
                                  1, sum, na.rm = TRUE)

visits$recreation_visiting <- apply(cbind(visits$bar_visits, 
                                        visits$public_recreation_visits, 
                                        visits$restaurant_visits), 
                                  1, sum, na.rm = TRUE)

visits %<>% 
  gather(key, value, -fips, -date) 

visits %<>% 
  filter(!is.na(value))

#id <- "1Gq_BX0zbkAHyQfNnnr458jC7lB2gjOj5" # google file ID
#sd_metrics <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
sd_metrics <- read_csv("https://www.dropbox.com/s/nb23ase5udsvdyn/sd-metrics_bycounty_baselined.csv?dl=1")

sd_metrics %<>% 
  rename(fips = fips5d) %>%
  select(fips, date, 
         median_home_dwell_time, 
         distance_traveled_from_home, 
         full_time_work_behavior_devices) %>% 
  gather(key, value, -date, -fips)

sd_metrics %<>%
  mutate(key = gsub("median_home_dwell_time", "hometime_sd", key),
         key = gsub("full_time_work_behavior_devices", "work_fulltime_sd", key),
         key = gsub("distance_traveled_from_home", "dist_traveled_sd", key))

safegr_raw <- rbind(sd_metrics, visits) %>% 
  arrange(fips, key, date)
#length(unique(safegr_raw$fips))

rm(list = "sd_metrics")
rm(list = "visits")

## Keep only the counties whose threshold has been reached
source("./county_features_cases.R")
#length(unique(county_features$fips))

safegr_raw %<>% 
  filter(fips %in% county_features$fips)
#length(unique(safegr_raw$fips))

safegr_raw %<>%  
  group_by(fips, key) %>% 
  arrange(date) %>% 
  mutate(index = row_number(), 
         index_desc = sort(index, decreasing = TRUE)) %>% 
  ungroup()

safegr_raw %<>% 
  arrange(fips, key, date)

safegr_raw %<>% 
  filter(key %in% c("school_visits", 
                    "college_visits", 
                    "recreation_visiting", 
                    "total_visiting", 
                    "hometime_sd"))

write_feather(safegr_raw, "./safegr_raw.feather") # in .gitignore, too big for github
# could move copy to dropbox
