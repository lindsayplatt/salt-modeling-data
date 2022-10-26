
p1_targets <- list(
  
  ##### Inventory #####
  
  # Define spatial query
  tar_target(conus_state_abbr, state.abb[!state.abb %in% c('AK', 'HI')]),
  
  # Dynamically branch over states to identify available data
  tar_target(conus_sc_sites,
             whatNWISsites(stateCd = conus_state_abbr, 
                           parameterCd = '00095',
                           startDate = start_date,
                           endDate = end_date),
             pattern = map(conus_state_abbr)),
  tar_target(conus_sc_sites_surface,
             filter(conus_sc_sites, site_tp_cd %in% c('ST', 'LK'))),
  tar_target(conus_sc_availability,
             whatNWISdata(siteNumber = conus_sc_sites_surface$site_no, 
                          parameterCd = '00095',
                          startDate = start_date,
                          endDate = end_date,
                          service = c('dv'))),#skip uv for now, 'uv'))),
  
  # Use results from search for available data to create download groups of 
  # sites based on service and chunks of sites. This is borrowed from Lauren 
  # Koenig's (USGS) brilliant function at 
  # https://github.com/USGS-R/ds-pipelines-targets-example-wqp/blob/main/2_download/src/fetch_wqp_data.R
  tar_target(conus_sc_download_info, {
    max_results <- 250000
    max_sites <- 2000
    df_grouped <- conus_sc_availability %>%
      select(site_no, data_type_cd, count_nu) %>% 
      group_by(data_type_cd) %>%
      arrange(desc(count_nu), .by_group = TRUE) %>%
      mutate(task_num_by_results = MESS::cumsumbinning(x = count_nu, 
                                                       threshold = max_results, 
                                                       maxgroupsize = max_sites), 
             task_num_by_group = cur_group_id()) %>%
      ungroup() %>% 
      # Each group from before (which represents a different data type code 
      # (uv or dv)) will have task numbers that start with "1", so now we create 
      # a new column called `task_num` to create unique task numbers within
      # each group and by splitting those based on max desired download sizes
      group_by(task_num_by_group, task_num_by_results) %>% 
      mutate(task_num = cur_group_id()) %>% 
      ungroup()
  }),
  tar_target(conus_sc_download_grps,
             conus_sc_download_info %>% 
               select(site_no, data_type_cd, task_num) %>% 
               group_by(task_num) %>% 
               tar_group(),
             iteration = "group"),
  
  # Define start and end dates in chunks of 5 years, so that we can
  # expand the time range without requiring a re-download of ALL
  # the data. 
  # TODO: Not as slick as I had hoped because a new start 
  # shifts the sequence. Maybe do individual years?
  tar_target(time_chunks, {
    date_seq_start <- seq(as.Date(start_date), as.Date(end_date), by="5 years")
    date_seq_end <- tail(date_seq_start, -1) - 1
    
    # Add on the end date 
    date_seq_end <- c(date_seq_end, as.Date(end_date))
    
    tibble(startDate = date_seq_start, 
           endDate = date_seq_end)
  }),
  
  ##### Download #####
  
  # Actually download some data based on the download groups
  # across sites, services, and time
  tar_target(conus_sc_data_raw_feather,{
    out_file <- sprintf('1_fetch/tmp/daily_sc_raw_%s_start_%s.feather', 
                        unique(conus_sc_download_grps$task_num),
                        gsub("-","_",time_chunks$startDate))
    readNWISdata(siteNumber = conus_sc_download_grps$site_no,
                 service = unique(conus_sc_download_grps$data_type_cd),
                 parameterCd = '00095',
                 startDate = time_chunks$startDate,
                 endDate = time_chunks$endDate) %>% 
      arrow::write_feather(out_file)
    return(out_file)
  }, pattern = cross(conus_sc_download_grps, time_chunks), format='file'),
  
  # Download site metadata & create state-site xwalk
  tar_target(conus_sc_site_metadata,
             readNWISsite(unique(conus_sc_download_info$site_no))),
  tar_target(conus_sc_site_state_xwalk,
             conus_sc_site_metadata %>%
               mutate(state_abbr = dataRetrieval::stateCdLookup(state_cd)) %>%
               select(site_no, state_abbr)),
  
  tar_target(conus_sc_data_clean_feather, {
    out_file <- gsub('tmp', 'out', gsub('raw_', '', conus_sc_data_raw_feather))
    read_feather(conus_sc_data_raw_feather) %>% 
      select(site_no, dateTime, 
             max_spec_cond = X_00095_00001, 
             min_spec_cond = X_00095_00002, 
             mean_spec_cond = X_00095_00003) %>% 
      left_join(conus_sc_site_state_xwalk, by="site_no") %>% 
      arrow::write_feather(out_file)
    return(out_file)
  }, pattern = map(conus_sc_data_raw_feather), format = 'file'),
  
  tar_target(conus_sc_data, 
             purrr::map(conus_sc_data_clean_feather, read_feather) %>% 
               bind_rows()
  )
  
  # TODO: Go back and download 'uv' data when a site didn't have any
  # 'dv' data. Use`conus_sc_availability` to figure this out.
)
