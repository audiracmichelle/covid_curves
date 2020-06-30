
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
    geom_vline(aes(xintercept = threshold_day + 12 - 7), linetype="dotted", color = "green") + 
    geom_vline(aes(xintercept = threshold_day + 12 + 5), linetype="dotted", color = "red") + 
    labs(title = county_, 
         x = "", y = "")
  p
}
