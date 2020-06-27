library(tidyverse)
library(magrittr)
library(feather)
library(rstanarm)

#### #### 
## model

## Read county_train
county_train <- read_feather("../county_train.feather")

## Train model
options(mc.cores=2)
model = stan_glmer.nb(
  deaths ~
    poly(days_since_thresh, 2) * (nchs) + 
    poly(days_since_thresh,2):(intervention + intervention:(nchs)) + 
    (poly(days_since_thresh, 2) | fips),
  offset = log(pop),
  data=county_train,
  algorithm="meanfield",
  iter = 15000,
  adapt_iter = 1000,
  QR=TRUE)

saveRDS(model, paste("./model.rds", sep = ""))

#### #### 
## county_fit

county_fit <- model %>%
  posterior_predict(county_train, draws = 200)

saveRDS(county_fit, "./county_fit.rds")

#### #### 
## coef_clusters

## extract coefficients for each county
#head(model$coefficients, 100)
coef_tbl <- tibble(values=model$coefficients,
                   names=names(model$coefficients)) %>%
  filter(grepl("fips", names)) %>% 
  mutate(param=case_when(
    grepl("Intercept", names) ~ "intercept",
    grepl("2)1", names) ~ "linear",
    grepl("2)2", names) ~ "quadratic"
  )) %>% 
  mutate(fips=gsub(".*:|]", "", names)) %>% 
  pivot_wider(id_cols="fips",
              values_from="values",
              names_from="param")
#coef_tbl

## transform tibble to dataframe to compute clusters
coef_df <- data.frame(select(coef_tbl, -fips), row.names = coef_tbl$fips)
coef_df <- scale(coef_df)
#coef_df

## --- Elbow Method for finding the optimal number of clusters
# set.seed(123)
# 
# # function to compute total within-cluster sum of square 
# wss <- function(k) {
#   kmeans(coef_df, k, nstart = 10 )$tot.withinss
# }
# 
# # Compute and plot wss for k = 1 to k = 15
# k.values <- 1:15
# 
# # extract wss for 2-15 clusters
# wss_values <- map_dbl(k.values, wss)
# 
# plot(k.values, wss_values,
#        type="b", pch = 19, frame = FALSE, 
#        xlab="Number of clusters K",
#        ylab="Total within-clusters sum of squares")
## ---

## get clusters
set.seed(123)
coef_clusters <- kmeans(coef_df, centers = 5)

coef_clusters <- rownames_to_column(data.frame(cluster = coef_clusters$cluster), var = "fips")
coef_clusters <- coef_clusters %>% full_join(coef_tbl) 
#coef_clusters

saveRDS(coef_clusters, "./coef_clusters.rds")

## get centroids
# centroids <- coef_clusters %>% 
#   group_by(cluster) %>% 
#   summarize(intercept = mean(intercept), 
#             linear = mean(linear), 
#             quadratic = mean(quadratic))
# centroids

