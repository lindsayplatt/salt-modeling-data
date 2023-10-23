
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
  
  ##### Process sites data for groundwater trend exploration #####
  
  # Load and reproject road salt raster data
  tar_target(road_salt_2015_raster, {
    r <- raster(road_salt_2015_tif)
    rtransf <- projectRaster(r, crs = st_crs(nhd_huc04s_sf)$proj4string)
    return(rtransf)
  }),
  
  # For each HUC, crop the raster to the polygon
  tar_target(nhd_hucs, nhd_huc04s_sf$huc4),
  tar_target(road_salt_2015_raster_huc_list, {
    
    # Mask the CONUS raster to each HUC
    huc04_mask <- filter(nhd_huc04s_sf, huc4 == nhd_hucs)
    
    # If the mask sf object is a GEOMETRYCOLLECTION, then it
    # is not able to be used by the `raster::mask()` function
    # So, we convert it to a MULTIPOLYGON first.
    if(st_geometry_type(huc04_mask) == 'GEOMETRYCOLLECTION') {
      huc04_mask <- huc04_mask %>% 
        # Only keep POLYGON features (this removes points and linestrings)
        st_collection_extract('POLYGON') %>% 
        # Cast to a MULTIPOLYGON and then as an sfc
        st_cast('MULTIPOLYGON') %>% st_as_sf()
    }
    
    # Crop to the same extent (or bbox) first
    huc04_salt_raster_extent <- crop(road_salt_2015_raster, extent(huc04_mask))
    
    # Then use mask to have it match the polygon boundaries exactly
    huc04_salt_raster <- mask(huc04_salt_raster_extent, huc04_mask)
    
    # Return the raster in a list along with the HUC04 name
    list(huc04 = nhd_hucs, road_salt_raster = huc04_salt_raster)
  }, pattern = map(nhd_hucs), iteration='list'),
  
  # Summarize each HUC's raster into a tibble
  tar_target(road_salt_2015_huc_summary, {
    huc_r <- road_salt_2015_raster_huc_list$road_salt_raster
    tibble(HUC04 = road_salt_2015_raster_huc_list$huc04,
           salt_min = cellStats(huc_r, stat='min', na.rm=TRUE),
           salt_p25 = unname(quantile(huc_r, probs=0.25, na.rm=TRUE)),
           salt_mean = cellStats(huc_r, stat='mean', na.rm=TRUE),
           salt_p75 = unname(quantile(huc_r, probs=0.75, na.rm=TRUE)),
           salt_max = cellStats(huc_r, stat='max', na.rm=TRUE),
           salt_sum = cellStats(huc_r, stat='sum', na.rm=TRUE))
  }, pattern = map(road_salt_2015_raster_huc_list)),
  
  # For each specific site, find the road salt application rate for 
  # that area (sum a certain buffer of cells around it)
  tar_target(road_salt_2015_site_summary, {
    values_around_each_site <- raster::extract(
      road_salt_2015_raster, 
      st_coordinates(q_sc_sites_sf), 
      buffer=5000, # In meters, but only seems to go vertically
      df=TRUE) %>% as_tibble()
    
    aggregate_value_per_site <- values_around_each_site %>% 
      group_by(ID) %>%
      summarize(road_salt_2015_aggr = sum(road_salt_2015))
    
    # Combine with the corresponding site numbers
    q_sc_sites_sf %>% 
      st_drop_geometry() %>% 
      bind_cols(aggregate_value_per_site) %>% 
      select(site_no, road_salt_2015_aggr)
  }),
  tar_target(sites_with_salt, 
             road_salt_2015_site_summary %>% 
               filter(road_salt_2015_aggr > 0)),
  
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
               st_drop_geometry() %>% 
               mutate(date = as.Date(dateTime)) %>% 
               dplyr::select(site_no, date, SpecCond = mean_spec_cond, Q = mean_q, state_abbr) %>% 
               split(.$site_no) %>% 
               purrr::map(separate_baseflow) %>%
               bind_rows() %>% 
               select(site_no, date, Q, Q_Base, Q_Storm, SpecCond, state_abbr) %>%
               mutate(is_summer = lubridate::month(date) %in% 6:9,
                      is_spring = lubridate::month(date) %in% 3:5,
                      is_fall = lubridate::month(date) %in% 10:11,
                      is_winter = lubridate::month(date) %in% c(1:2, 12)) %>%
               left_join(q_sc_sites_info, by = 'site_no') %>% 
               mutate(Ts = trans_MEAN, DTW = dtw_MEAN) %>% 
               # Add this trivial amt to avoid dividing by 0
               mutate(SC_flow_normalized = SpecCond / (Q+0.000000001))),
  
  tar_target(sc_trend_data_lm, add_trends(q_sc_data_baseq, trend_method = "LM")),
  tar_target(sc_trend_data_ma, add_trends(q_sc_data_baseq, trend_method = "MA")),
  tar_target(sc_trend_data_mk, add_trends(q_sc_data_baseq, trend_method = "MK")),
  # TODO: COME BACK TO THE NA ONES: sum(is.na(sc_trend_data_mk$trend)) --> 416
  
  tar_target(sc_trends_with_site_info, 
             sc_trend_data_mk %>% 
               left_join(nhd_huc04_site_xwalk, by="site_no") %>% 
               left_join(road_salt_2015_huc_summary, by="HUC04"))
)
