
fit_sampling <- function(county_data, fit) {
  county_data %>% 
    mutate(
      fit_mu = apply(fit, 2, mean),
      fit_med = apply(fit, 2, quantile, probs = 0.5), # use posterior median to hand skewness
      fit_lo = apply(fit, 2, quantile, probs = 0.05),
      fit_hi = apply(fit, 2, quantile, probs = 0.95))
}

ctr_sampling <- function(county_data, ctr) {
  county_data %>% 
    mutate(
      ctr_mu = apply(ctr, 2, mean),
      ctr_med = apply(ctr, 2, quantile, probs = 0.5), # use posterior median to hand skewness
      ctr_lo = apply(ctr, 2, quantile, probs = 0.05),
      ctr_hi = apply(ctr, 2, quantile, probs = 0.95))
}

gg_intervention_sampling <- function(county_data) {
  county_ <- county_data %>% 
    filter(index == 1) %>% 
    pull(county)
  
  p <- county_data %>% 
    ggplot() + 
    geom_point(aes(x=date, y=y)) + 
    geom_line(aes(x=date, y=fit_med), 
              col = "blue") + 
    geom_ribbon(aes(x=date, ymin=fit_lo, ymax=fit_hi), 
                alpha= 0.1, fill = "blue") + 
    geom_line(aes(x=date, y=ctr_med), 
              col = "red") + 
    geom_ribbon(aes(x=date, ymin=ctr_lo, ymax=ctr_hi), 
                alpha= 0.1, fill = "red") + 
    geom_vline(aes(xintercept = stayhome), color = "blue") + 
    geom_vline(aes(xintercept = stayhome + 12), linetype="dotted", color = "blue") +
    labs(title = county_, 
         x = "", y = "")
  p
}

gg_days_btwn_sampling <- function(county_data) {
  county_ <- county_data %>% 
    filter(index == 1) %>% 
    pull(county)
  
  p <- county_data %>% 
    ggplot() + 
    geom_point(aes(x=date, y=deaths)) + 
    geom_line(aes(x=date, y=fit_med), 
              col = "blue") + 
    geom_ribbon(aes(x=date, ymin=fit_lo, ymax=fit_hi), 
                alpha= 0.1, fill = "blue") + 
      geom_line(aes(x=date, y=ctr1_med), 
              col = "green") + 
      geom_ribbon(aes(x=date, ymin=ctr1_lo, ymax=ctr1_hi), 
                alpha= 0.1, fill = "green") + 
       geom_line(aes(x=date, y=ctr3_med), 
              col = "red") + 
      geom_ribbon(aes(x=date, ymin=ctr3_lo, ymax=ctr3_hi), 
                alpha= 0.1, fill = "red") +  
    geom_vline(aes(xintercept = stayhome), color = "blue") + 
    geom_vline(aes(xintercept = stayhome + 12), linetype="dotted", color = "blue") + 
    geom_vline(aes(xintercept = stayhome + 12 - 7), linetype="dotted", color = "green") + 
    geom_vline(aes(xintercept = stayhome + 12 + 5), linetype="dotted", color = "red") + 
    labs(title = county_, 
         x = "", y = "")
  p
}
