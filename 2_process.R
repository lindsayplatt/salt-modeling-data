
source('2_process/src/groundwater_helpers.R')

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
               summarize(annual_mean_sc_norm = mean(daily_mean_sc_norm, na.rm=TRUE), .keep="groups")),
  
  # Combine SC and Q for ML model
  tar_target(conus_q_sc_airtemp_csv, {
    file_out <- '2_process/out/conus_q_sc_airtemp.csv'
    conus_sc_data %>% 
      select(dateTime, site_no, spec_cond = mean_spec_cond) %>% 
      inner_join(select(conus_q_data, dateTime, site_no, discharge = mean_q)) %>% 
      inner_join(select(conus_air_temp, dateTime, site_no, airtemp = mean_airtemp)) %>% 
      write_csv(file_out)
    return(file_out)
  }, format = 'file'),
  
  ##### Process sites data for trend exploration #####
  
  # Add percentiles to be able to show relative transmissivity/dtw
  tar_target(trans_ecdf, ecdf(trans$trans_MEAN)),
  tar_target(dtw_ecdf, ecdf(dtw$dtw_MEAN)),
  
  tar_target(q_sc_data, distinct(conus_sc_data) %>% 
               # Remove states without salt application info per Dugan et al., 2017
               dplyr::filter(!state_abbr %in% states_to_exclude) %>% 
               left_join(distinct(conus_q_data), by = c("state_abbr", "site_no", "dateTime")) %>% 
               filter(!is.na(mean_spec_cond), !is.na(mean_q))),
  
  # Join the transmissivity and depth-to-watertable data by COMID
  tar_target(q_sc_sites_info, q_sc_nhd_comid %>% 
               filter(site_no %in% unique(q_sc_sites_sf$site_no)) %>% 
               left_join(trans, by="COMID") %>% 
               left_join(dtw, by="COMID") %>% 
               mutate(trans_PERCENTILE = trans_ecdf(trans_MEAN)*100,
                      dtw_PERCENTILE = 100-dtw_ecdf(dtw_MEAN)*100)),
  
  # Separate baseflow in case we want to use that
  # Also, calculate flow-normalized SC
  tar_target(q_sc_data_baseq, q_sc_data %>% 
               mutate(date = as.Date(dateTime)) %>% 
               dplyr::select(site_no, date, SpecCond = mean_spec_cond, Q = mean_q, state_abbr) %>% 
               split(.$site_no) %>% 
               purrr::map(separate_baseflow) %>%
               bind_rows() %>% 
               select(site_no, date, Q, Q_Base, Q_Storm, SpecCond, state_abbr) %>%
               mutate(baseflow_only = Q_Storm == 0, 
                      is_summer = lubridate::month(date) %in% 6:9,
                      is_spring = lubridate::month(date) %in% 3:5,
                      is_fall = lubridate::month(date) %in% 10:11) %>%
               left_join(q_sc_sites_info) %>% 
               mutate(Ts = trans_MEAN, DTW = dtw_MEAN) %>% 
               # Add this trivial amt to avoid dividing by 0
               mutate(SC_flow_normalized = SpecCond / (Q+0.000000001))),
  
  tar_target(sc_trend_data_lm, add_trends(q_sc_data_baseq, trend_method = "LM")),
  tar_target(sc_trend_data_ma, add_trends(q_sc_data_baseq, trend_method = "MA"))
  
)
