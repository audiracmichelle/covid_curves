gg_vanilla_sampling <- function(county_data) {
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
    geom_line(aes(x=date, y=ctr_med), 
              col = "red") + 
    geom_ribbon(aes(x=date, ymin=ctr_lo, ymax=ctr_hi), 
                alpha= 0.1, fill = "red") + 
    geom_vline(aes(xintercept = stayhome), color = "blue") + 
    geom_vline(aes(xintercept = stayhome + 12), linetype="dotted", color = "blue") +
    geom_vline(aes(xintercept = threshold_day + 20), color = "red") + 
    geom_vline(aes(xintercept = threshold_day + 20 + 12), linetype="dotted", color = "red") + 
    labs(title = county_, 
         x = "", y = "")
  p
}