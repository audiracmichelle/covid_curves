library(tidyverse)
library(magrittr)
library(feather)

cc <- read_csv("https://www.dropbox.com/s/yrw14r46bvcxmnh/cc-est2019-alldata.csv?dl=1")
head(cc) %>% view
summary(cc)

## extract only 2019
table(cc$YEAR)
cc %<>%
  filter(YEAR == 12) %>% 
  select(-YEAR)

## define fips
cc %<>% 
  mutate(fips = paste(STATE, COUNTY, sep = ""))

## select columns
table(cc$SUMLEV)
cc %<>%
  select(fips, AGEGRP, 
         TOT_POP, WA_MALE, WA_FEMALE, BA_MALE, BA_FEMALE, H_MALE, H_FEMALE) %>% 
  mutate(white = WA_MALE + WA_FEMALE, 
         black = BA_MALE + BA_FEMALE, 
         hispanic = H_MALE + H_FEMALE)

## group age groups
table(cc$AGEGRP)
cc %<>%
  filter(AGEGRP >= 5)

cc$AGEGRP[cc$AGEGRP %in% 5:10] <- 20
cc$AGEGRP[cc$AGEGRP %in% 11:13] <- 45
cc$AGEGRP[cc$AGEGRP %in% 14:18] <- 65
cc$AGEGRP <- cut(cc$AGEGRP, c(0,20,45,65,100), right = FALSE)

cc %<>% 
  group_by(fips, AGEGRP) %>% 
  summarise_all(sum)

levels(cc$AGEGRP) <- c("age_0_19", 
                       "age_20_44", 
                       "age_45_64", 
                       "age_65_plus")

## write data

cc_agegrp <- cc %>% 
  select(fips, AGEGRP, TOT_POP) %>% 
  pivot_wider(names_from = AGEGRP, values_from = TOT_POP)
length(unique(cc_agegrp$fips))

cc_ethnic <- cc %>% 
  select(fips, white, black, hispanic, -AGEGRP) %>% 
  group_by(fips) %>% 
  summarise_all(sum)
length(unique(cc_ethnic$fips))

cc <- left_join(cc_agegrp, cc_ethnic) 
head(cc) %>% view

write_feather(cc, "./cc.feather")
