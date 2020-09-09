library(tidyverse)
library(cowplot)

source("curves_by_fips.R")

model = readRDS("./model.rds")
county_fit = readRDS("./county_fit.rds")
source("../../plot_foo.R")
df = read_feather("../../county_train_stayhome.feather")

curves = list()
curves$actual = curves_by_fips(model, df)
curves$late = curves_by_fips(model, df, intervention_offset=10)
curves$early = curves_by_fips(model, df, intervention_offset=-10)

nchs_map = distinct(select(df, fips, nchs))
nchs_map = setNames(as.integer(as.character(nchs_map$nchs)), nchs_map$fips)

max_t = length(curves$actual[[1]]$dates)

cum_deaths = list()
deaths = list()
cum_since_interv
cum_deaths$actual = array(0, c(1000, max_t, 6))
cum_deaths$late = array(0, c(1000, max_t, 6))
cum_deaths$early = array(0, c(1000, max_t, 6))

deaths

