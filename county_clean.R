library(tidyverse)
library(magrittr)
library(feather)

## Read 
county_clean <- read_feather("./county_raw.feather")

## Filter counties with cases
county_clean %<>% 
  filter(!is.na(county))

## Clean popdensity
popdensity_ = county_clean %>% 
  distinct(fips, popdensity) %>% 
  pull(popdensity)
#summary(popdensity_)

county_clean %<>% 
  filter(!is.na(popdensity),
         popdensity > quantile(popdensity_, 0.1, na.rm = TRUE)
  )

## Create daily case and death count columns
county_clean %<>%  
  group_by(fips) %>% 
  arrange(date) %>% 
  mutate(cases = diff(c(0,cum_cases)), 
         deaths = diff(c(0,cum_deaths))) %>% 
  ungroup()

## Remove timestamps with negative counts
#length(unique(county_clean$fips))
county_clean <- county_clean[-which(county_clean$deaths < 0), ]
#length(unique(county_clean$fips))

## Create index columns

county_clean %<>% 
  group_by(fips) %>% 
  arrange(date) %>% 
  mutate(index = row_number(), 
         index_desc = sort(index, decreasing = TRUE)) %>% 
  ungroup()

#---do not do this
## Compute cumulative cases and deaths
#summary(county_clean$cum_cases)
# county_clean %<>%  
#   group_by(fips) %>% 
#   arrange(date) %>% 
#   mutate(cum_cases = cumsum(cases), 
#          cum_deaths = cumsum(deaths)) %>% 
#   ungroup()
#summary(county_clean$cum_cases)

## Create per capita and per sq deaths and cases
county_clean <- county_clean  %>% 
  arrange(fips, date) %>%
  mutate(cum_deaths_per_cap = cum_deaths / pop,
         cum_deaths_per_sq_mi = cum_deaths / sq_mi, 
         cum_cases_per_cap = cum_cases / pop,
         cum_cases_per_sq_mi = cum_cases / sq_mi)

write_feather(county_clean, "./county_clean.feather")
