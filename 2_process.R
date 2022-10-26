
p2_targets <- list(
  
  tar_target(conus_sc_data_plot_ready,
             conus_sc_data %>%
               pivot_longer(cols = c(-site_no, -dateTime, -state_abbr),
                            names_to = "daily_stat",
                            values_to = "specific_conductance") %>%
               mutate(daily_stat = gsub("_spec_cond", "", daily_stat)) %>%
               filter(!is.na(specific_conductance)) %>% 
               # TODO: seasons should probably be more sophisticated based on latitude
               mutate(season = ifelse(month(dateTime) %in% c(12,1,2), 'Winter',
                                      ifelse(month(dateTime) %in% c(3,4,5), "Spring",
                                             ifelse(month(dateTime) %in% c(6,7,8), "Summer", "Fall")))) %>% 
               mutate(season = factor(season, levels = c('Fall', 'Winter', 'Spring', 'Summer'), ordered = TRUE)) %>% 
               # Add count per boxplot (state & statistic), but only
               # set the label for the max value
               group_by(state_abbr, daily_stat) %>% 
               mutate(count_label = ifelse(specific_conductance == max(specific_conductance), 
                                           sprintf('n = %s', n()), NA)) %>% 
               ungroup()
  ),
  
  tar_target(conus_sc_data_plot_ready_means, conus_sc_data_plot_ready %>% filter(daily_stat == "mean")),
  
  tar_target(conus_sc_data_plot_ready_daily_means_normalized, {
    conus_sc_data_plot_ready_means %>% 
      group_by(site_no) %>% 
      # Normalize the data by finding the percentile for each SC value
      mutate(daily_mean_sc_norm = cume_dist(specific_conductance)) %>% 
      ungroup()
  }),
  
  tar_target(conus_sc_data_plot_ready_annual_means_normalized, 
             conus_sc_data_plot_ready_daily_means_normalized %>% 
               mutate(yr = lubridate::year(dateTime)) %>% 
               group_by(state_abbr, site_no, yr) %>% 
               summarize(annual_mean_sc_norm = mean(daily_mean_sc_norm, na.rm=TRUE), .keep="groups"))
  
)
