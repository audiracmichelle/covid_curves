
gg_intrv_sampling <- function(data, name, intrv_name, lag) {
  p <- data %>% 
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
    xlim(as.Date("2020-03-01"), as.Date("2020-04-30"))

  if(intrv_name == "decrease") {
    p <- p + 
    geom_vline(aes(xintercept = decrease_40_total_visiting), color = "blue") + 
    geom_vline(aes(xintercept = decrease_40_total_visiting + lag), linetype="dotted", color = "blue") +
    labs(title = name, 
         x = "", y = "")
  }
  p
}

gg_days_btwn_sampling <- function(county_data, up, down) {
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
    geom_vline(aes(xintercept = stayhome + 12 + down), linetype="dotted", color = "green") + 
    geom_vline(aes(xintercept = stayhome + 12 + up), linetype="dotted", color = "red") + 
    labs(title = county_, 
         x = "", y = "")
  p
}

gg_days_btwn_effect <- function(county_data, up, down) {
  county_ <- county_data %>% 
    filter(index == 1) %>% 
    pull(county)
  
  p <- county_data %>% 
    ggplot() + 
    geom_point(aes(x=date, y=cum_y)) + 
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
    geom_vline(aes(xintercept = stayhome + 12 + down), linetype="dotted", color = "green") + 
    geom_vline(aes(xintercept = stayhome + 12 + up), linetype="dotted", color = "red") + 
    labs(title = county_, 
         x = "", y = "")
  p
}

gg_nchs_sampling <- function(nchs_data, n_, up = up, down = down) {
  
  p <- nchs_data %>% 
    ggplot() + 
    geom_point(aes(x=days_since_thresh, y=log_y)) + 
    geom_line(aes(x=days_since_thresh, y=fit_med), 
              col = "blue") + 
    geom_ribbon(aes(x=days_since_thresh, ymin=fit_lo, ymax=fit_hi), 
                alpha= 0.1, fill = "blue") + 
    geom_line(aes(x=days_since_thresh, y=ctr1_med), 
              col = "green") + 
    geom_ribbon(aes(x=days_since_thresh, ymin=ctr1_lo, ymax=ctr1_hi), 
                alpha= 0.1, fill = "green") + 
    geom_line(aes(x=days_since_thresh, y=ctr3_med), 
              col = "red") + 
    geom_ribbon(aes(x=days_since_thresh, ymin=ctr3_lo, ymax=ctr3_hi), 
                alpha= 0.1, fill = "red") +  
    #geom_vline(aes(xintercept = stayhome), color = "blue") + 
    #geom_vline(aes(xintercept = stayhome + 12), linetype="dotted", color = "blue") + 
    #geom_vline(aes(xintercept = stayhome + 12 + down), linetype="dotted", color = "green") + 
    #geom_vline(aes(xintercept = stayhome + 12 + up), linetype="dotted", color = "red") + 
    labs(title = n_, 
         x = "", y = "")
  p
}
