library(tidyverse)
library(magrittr)
library(feather)
library(lubridate)
library(collections)
library(pracma)
library(igraph)
options(mc.cores = 4)
source("../utils.R")


## Read county_train
county_train <- read_feather("../../county_train_.feather") %>%   # 1454 counties
  filter(date <= ymd("20200420")) %>%   # 1021 counties
  group_by(fips) %>%
  filter(
    max(days_since_thresh) >= 7,  # min data points, 909 counties
    max(cum_deaths) >= 1 # there as an outbreak, 400 counties
  ) %>%  
  # filter(state != "New York") %>% 
  ungroup()
valid_fips = unique(county_train$fips)

# writeLines(unique(county_train$fips), "fips_in_data.txt")
edges = read_csv("../fips_adjacency.csv") %>% 
  # filter(isnbr) %>% %>% 
  filter(dist <= 250) %>% 
  filter(src_lab %in% valid_fips, tgt_lab %in% valid_fips)

model_data = stan_input_graph_data(county_train, edges, lag=14, type="decrease")
print(paste("Number of connected components: ", model_data$N_comps))

model = rstan::stan_model("../hockey_stick_hyperprior_spatial.stan")


# first run with variational inference,
# it should take ~ 20 mins and 15k iters to coverge with 0.002 precision
fit = rstan::vb(
  model, 
  data=model_data,
  adapt_engaged=FALSE,
  eta = 0.25,
  iter=100000,
  tol_rel_obj=0.002,
  adapt_iter=250,
  init="0",
  output_samples=250
)


parnames = c(
  "nchs_pre", "nchs_post", "beta_covars_pre",
  "beta_covars_post", "beta_covars_post",
  "baseline_pre", "baseline_post",
  "overdisp", "rand_eff",
  "Omega_rand_eff", "scale_rand_eff"
)
pars = rstan::extract(fit, pars=parnames)

# create list of parameter inialization=
nchains = 2
init_lists = map(1:nchains, function(i) {
  map(pars, function(par) {
    if (length(dim(par))==1)
      return (par[i])
    if (length(dim(par))==2)
      return (par[i, ])
    if (length(dim(par))==3)
      return (par[i, , ])
    print("error")
  })
})
# annoying but must do below becaue R indexing kills a dimension
for (i in 1:nchains)
  init_lists[[i]]$beta_covars_post = matrix(init_lists[[i]]$beta_covars_post, nrow=1)

# now pass solution
# fit2 = rstan::sampling(
#   model,
#   data=model_data,
#   chains=nchains,
#   iter=1000,
#   warmup=500,
#   init=init_lists
# )

# revised_0 uses the joint dataset
# saveRDS(fit, paste("./model_full_rstan_var_revised_0.rds", sep = ""))

# revised_1 uses a spatial model
saveRDS(fit, paste("./model_full_rstan_var_revised_spatial_v2.rds", sep = ""))

# this one uses the old dataset
# saveRDS(fit, paste("./model_full_rstan_var.rds", sep = ""))

# saveRDS(fit2, paste("./model_full_rstan_mcmc.rds", sep = ""))
# fit = readRDS("./model_full_rstan_var.rds")

# model = readRDS(paste("./model_full_rstan.rds", sep = ""))

#### #### 
## county_fit

# county_fit <- model 
#   posterior_predict(county_train, draws = 500)
# county_fit_var = rstan::extract(fit, pars="y_new")$y_new[-(1:500), ]

# county_fit = rstan::extract(fit2, pars="y_new")$y_new
# county_lp = exp(rstan::extract(fit2, pars="log_rate")$log_rate)

# saveRDS(county_fit_var, "./county_fit_var.rds")
# saveRDS(county_lp_var, "./county_fit_lp_var.rds")

# saveRDS(county_fit, "./county_fit.rds")
# saveRDS(county_lp, "./county_fit_lp.rds")

# let's validate for some location and then call it a day
# it's working !
county_lp_var = exp(rstan::extract(fit, pars="log_rate")$log_rate)
f1 = "06037"
# f1 = "36081"
ix = which(county_train$fips == f1)

yi = county_train$y[ix]
yhati = apply(county_lp_var[ ,ix], 2, median)
yhati_95 = apply(county_lp_var[ ,ix], 2, quantile, .95)
yhati_05 = apply(county_lp_var[ ,ix], 2, quantile, .05)

# ymeani = apply(county_fit_var[ ,ix], 2, mean)
plot(yi)
lines(yhati, col="red")
lines(yhati_95, col="blue", lty=2)
lines(yhati_05, col="blue", lty=2)
dbtwn = county_train[ix, ]
dbtwn = dbtwn[dbtwn$days_since_intrv_decrease >= 0, ]
dbtwn = dbtwn$days_since_thresh[1]
abline(v=dbtwn + 14, lty=3, col="gray")
title(sprintf("FIPS %s", f1))


predicted = my_posterior_predict(fit, county_train, eval_pre=FALSE, lag=14)

pre_term = apply(predicted$pre_term[ ,ix], 2, median)
post_term = apply(predicted$post_term[ ,ix], 2, median)
log_yhat = apply(predicted$log_yhat[, ix], 2, median)

plotdata = tibble(
  no_intervention=pre_term,
  # intervention_effect=post_term,
  intervention=log_yhat,
  date=county_train$date[ix]
) %>% 
  pivot_longer(-date)

ggplot(plotdata) +
  geom_line(aes(x=date, y=value, color=name)) +
  geom_vline(aes(xintercept=date[1] + dbtwn - 1 + 14), color="black", lty=2) +
  theme_minimal() +
  labs(
    title=sprintf("FIPS %s", f1),
    subtitle="Counterfactual with/without intervention in logscale"
  )


overdisp = rstan::extract(fit, pars="overdisp")$overdisp
hist(overdisp, col=alpha("blue", 0.5), main="overdisp posterior")

rho = sigmoid(rstan::extract(fit, pars="logit_rho")$logit_rho)
rho_med = apply(rho, 2, median)
hist(1 - rho_med, col=alpha("blue", 0.5), main="distribution of spatial weights per component")

