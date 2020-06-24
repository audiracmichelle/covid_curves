library(tidyverse)
library(magrittr)
library(readr)
library(feather)

nchs <- read_fwf("./NCHSURCodes2013.txt", 
                 fwf_cols(fips = c(1, 5), 
                          state = c(7, 8), 
                          county = c(10, 45), 
                          cbsa = c(47, 96), 
                          cbsa_popul = c(98, 105), 
                          popul = c(107, 114), 
                          nchs = c(116, 116), 
                          nchs2006 = c(118, 118)))

nchs %<>% 
  mutate(cbsa_popul = as.numeric(cbsa_popul), 
         popul = as.numeric(popul), 
         nchs = as.factor(nchs), 
         nchs2006 = as.factor(nchs2006))

write_feather(nchs, "./nchs.feather")
