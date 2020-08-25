library(xtable)
library(rstanarm)
library(feather)
library(cowplot)
library(tidyverse)
library(ggthemes)

# read models and dfs

model_stayhome = readRDS("./roll_deaths/model.RDS")
summary_stayhome = summary(
  model_stayhome,
  probs = c(0.05, 0.5, 0.95)
)

# learn to recover coefficients from orthogonalized model
df_stayhome = read_feather("../county_train_stayhome.feather")
tpoly_sh = poly(df_stayhome$days_since_thresh, 2)
# c1_sh = lm(df_stayhome$days_since_thresh ~ tpoly_sh)$coefficients
# c2 = lm(df_stayhome$days_since_thresh ~ tpoly_sh)$coefficients
df_decrease = read_feather("../county_train_decrease.feather")
tpoly_dec = poly(df_decrease$days_since_thresh, 2)

model_decrease = readRDS("./roll_deaths_decrease/model.RDS")
summary_decrease = summary(
  model_decrease,
  probs = c(0.05, 0.5, 0.95)
)

# learn to recover coefficients from orthogonalized model
df_decrease = read_feather("../county_train_decrease.feather")
tpoly_md = poly(df_decrease$days_since_thresh, 2)
c1 = lm(df_decrease$days_since_thresh ~ tpoly_md)$coefficients
c2 = lm(df_decrease$days_since_thresh ~ tpoly_md)$coefficients

model_double = readRDS("./roll_deaths_double_stick/model.RDS")
summary_double = summary(
  model_double,
  probs = c(0.05, 0.5, 0.95)
)

cols = c("mean", "5%", "95%")

d0_coefs = c(
  "(Intercept)"="Control",
  "nchs2"="NCHS-2",
  "nchs3"="NCHS-3",
  "nchs4"="NCHS-4",
  "nchs5"="NCHS-5",
  "nchs6"="NCHS-6",
  "college"="% in college",
  "black"="% black",
  "hispanic"="% hispanic",
  "age_65_plus"="% +65 age"
)   

d1_coefs = d0_coefs
names(d1_coefs) = paste0(
  "poly(days_since_thresh, 2)1:",
  names(d0_coefs)
)
names(d1_coefs)[1] = "poly(days_since_thresh, 2)1"

d2_coefs = d0_coefs
names(d2_coefs) = paste0(
  "poly(days_since_thresh, 2)2:",
  names(d0_coefs)
)
names(d2_coefs)[1] = "poly(days_since_thresh, 2)2"

d_coefs_list = list(
  d0_coefs,
  d1_coefs,
  d2_coefs
)

val_formatter = function(x) {
  case_when(
    abs(x) > 100 ~ round(x, 0),
    abs(x) > 10 ~ round(x, 1),
    TRUE ~ round(x, 2)
  )
}

mean_formatter = function(m, l, u) {
  m = val_formatter(m)
  fmt = case_when(
    (u < 0) ~ "%s*",
    (l > 0) ~ "%s*",
    TRUE ~ "%s"
  )
  sprintf(fmt, m)
}
interv_formatter = function(l, u) {
  l = val_formatter(l)
  u = val_formatter(u)
  fmt = "[%s, %s]"
  sprintf(fmt, l, u)
}

summary_list = list(
  "Stay-at-home"=summary_stayhome,
  "Mobility"=summary_decrease,
  "Double"=summary_double
)

get_table_by_degree = function(d) {
  tbl = map(
    1:3,
    function(i) {
      d_coefs = d_coefs_list[[d + 1]]
      mean_var = paste0(names(summary_list)[i], "_m")
      interv_var = paste0(names(summary_list)[i], "_i")
      summary_list[[i]][names(d_coefs), cols] %>% 
        as.data.frame() %>%
        rownames_to_column() %>%
        mutate(
          parameter=d_coefs[rowname],
          mean = mean_formatter(`mean`, `5%`, `95%`),
          interv = interv_formatter(`5%`, `95%`)
        ) %>% 
        mutate() %>% 
        select(
          parameter,
          !!mean_var:=`mean`,
          !!interv_var:=`interv`,
        )
    }
  ) %>% 
    reduce(left_join, by="parameter") %>% 
    as.data.frame()
  # annoying thing to remove row names
  row.names(tbl) = tbl[ ,1]
  tbl = tbl[ ,-1]
  xtable(tbl)
}

get_table_by_degree(0)
get_table_by_degree(1)
get_table_by_degree(2)



curves_by_variable = function(
  varname, model, polyfun, t=1:30
) {
  # posterior samples from model coefficients
  samples = as.matrix(model)
  
  # pre-compute time polynomials and population var
  # note must call poly on the entire dataset so that
  # t and t^2 is the same as in training (it orthogonalizes)
  # ideally it should be defined in the data prep script
  poly_t = cbind(1, predict(polyfun, t))
  
  nms = paste0(
    c("",
      "poly(days_since_thresh, 2)1:",
      "poly(days_since_thresh, 2)2:"),
    varname
  )

  covar_effect = samples[ ,nms] %*% t(poly_t)

  out = tibble(
    t=t,
    mean=apply(covar_effect, 2, mean),
    sd=apply(covar_effect, 2, sd),
    q05=apply(covar_effect, 2, quantile, 0.05),
    q25=apply(covar_effect, 2, quantile, 0.25),
    median=apply(covar_effect, 2, quantile, 0.50),
    q75=apply(covar_effect, 2, quantile, 0.75),
    q95=apply(covar_effect, 2, quantile, 0.95)
  )
  out$iqr = out$q75 - out$q25
  
  return (out)
}

vars = c("black", "hispanic", "college", "age_65_plus")
model_names = c("stayhome", "decrease", "double")
models = list(
  stayhome=model_stayhome,
  decrease=model_decrease,
  double=model_double
)
polyfuns = list(
  stayhome=tpoly_sh,
  decrease=tpoly_dec,
  double=tpoly_dec
)
t = 1:30

combs = expand.grid(
  varname=vars,
  model=model_names,
  stringsAsFactors=FALSE
)
N = nrow(combs)

plotdata = map(1:N, ~ {
  varname = combs$varname[.x]
  model_name = combs$model[.x]
  model = models[[model_name]]
  polyfun = polyfuns[[model_name]]
  curves_by_variable(
    varname, model_stayhome, polyfun, t
  ) %>% 
    mutate(var=varname, model=model_name)
}) %>% 
  bind_rows() %>% 
  mutate(model=case_when(
    model == "decrease" ~ "Mobility",
    model == "stayhome" ~ "Stay-home",
    model == "double" ~ "Double intervention"
  )) %>% 
  mutate(var=case_when(
    var == "black" ~ "Black",
    var == "college" ~ "College",
    var == "hispanic" ~ "Hispanic",
    TRUE ~ "Age 65+"
  ))

ggplot(plotdata) +
  geom_line(aes(x=t, y=median, color=model), size=1.0) +
  geom_hline(yintercept=0, linetype=1, size=2, alpha=0.2) +
  geom_ribbon(
    aes(x=t, ymin=q05, ymax=q95, fill=model),
    alpha=0.4
  ) +
  facet_grid(var ~ model, scales="free") +
  labs(fill="Model",
       x="Days since threshold",
       y="Effect") +
  guides(color=FALSE, fill=FALSE) +
  theme_few()+
  scale_color_manual(values=c("#0072B2", "#009E73", "#D55E00")) +
  scale_fill_manual(values=c("#0072B2", "#009E73", "#D55E00"))

ggsave(
  "covariate-effect-comparison.pdf",
  width=12.2,
  height=11,
  units="cm"
)
