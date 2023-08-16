
p1_targets <- list(
  
  ##### Inventory SC #####
  
  # Define spatial query
  tar_target(conus_state_abbr, state.abb[!state.abb %in% c('AK', 'HI')]),
  
  # Dynamically branch over states to identify available data
  tar_target(conus_sc_sites,
             whatNWISsites(stateCd = conus_state_abbr, 
                           parameterCd = '00095',
                           startDate = start_date,
                           endDate = end_date,
                           siteType = site_type,
                           service = data_service),
             pattern = map(conus_state_abbr)),
  tar_target(conus_sc_sites_surface,
             conus_sc_sites %>% 
               # Remove any with ST-CA, ST-DCH, ST-TS
               filter(site_tp_cd %in% c('ST')) %>% 
               # Then filter to only those that also have flow data
               # which meet the appropriate requirements (at least 10 yrs
               # with some of it happening after Jan 1, 2000)
               filter(site_no %in% conus_q_availability_all$site_no)),
  tar_target(conus_sc_availability,
             whatNWISdata(siteNumber = conus_sc_sites_surface$site_no, 
                          parameterCd = '00095',
                          startDate = start_date,
                          endDate = end_date,
                          service = data_service) %>% 
               # Filter to only those sites that have had
               # data past 2010 and at least 10 years of it
               mutate(years_running = as.numeric((end_date - begin_date)/365)) %>% 
               filter(years_running >= record_min_yrs) %>% 
               filter(end_date >= as.Date(recent_date))),
  
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
  
  ##### Download SC #####
  
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
    
    # Setup desired column structures
    sc_cols <- tibble(max_spec_cond = as.numeric(NA),
                      min_spec_cond = as.numeric(NA),
                      mean_spec_cond = as.numeric(NA))
    rename_xwalk <- c(
      max_spec_cond = "X_00095_00001",
      min_spec_cond = "X_00095_00002",
      mean_spec_cond = "X_00095_00003"
    )
    
    raw_data <- read_feather(conus_sc_data_raw_feather)
    
    if(any(names(raw_data) %in% rename_xwalk)) {
      prepped_data <- raw_data %>% 
        # Some of the raw data do not have all of the stat code columns,
        # so need to add a column of NAs when it doesn't exist. The `rename()`
        # followed by the `full_join()` will accomplish this.
        rename(any_of(rename_xwalk)) %>% 
        full_join(sc_cols)
    } else {
      # This will catch empty data frames missing all SC cols
      prepped_data <- raw_data %>% cross_join(sc_cols)
    }
    
    prepped_data %>% 
      select(site_no, dateTime, max_spec_cond, min_spec_cond, mean_spec_cond) %>% 
      left_join(conus_sc_site_state_xwalk, by="site_no") %>% 
      arrow::write_feather(out_file)
    
    return(out_file)
  }, pattern = map(conus_sc_data_raw_feather), format = 'file'),
  
  tar_target(conus_sc_data, 
             purrr::map(conus_sc_data_clean_feather, read_feather) %>% 
               bind_rows()
  ),
  tar_target(conus_sc_data_csv, {
    file_out <- '1_fetch/out/conus_sc.csv'
    write_csv(conus_sc_data, file_out)
    return(file_out)
  }, format="file"),
  
  # TODO: Go back and download 'uv' data when a site didn't have any
  # 'dv' data. Use`conus_sc_availability` to figure this out.
  
  ##### Inventory Discharge #####
  
  # Dynamically branch over states to identify available data
  tar_target(conus_q_sites,
             whatNWISsites(stateCd = conus_state_abbr, 
                           parameterCd = '00060',
                           startDate = start_date,
                           endDate = end_date,
                           siteType = site_type,
                           service = data_service),
             pattern = map(conus_state_abbr)),
  tar_target(conus_q_sites_surface,
             # Remove any with ST-CA, ST-DCH, ST-TS
             filter(conus_q_sites, site_tp_cd %in% c('ST'))),
  tar_target(conus_q_availability_all,
             whatNWISdata(siteNumber = conus_q_sites_surface$site_no, 
                          parameterCd = '00060',
                          startDate = start_date,
                          endDate = end_date,
                          service = data_service) %>% 
               # Filter to only those sites that have had data past 2010
               # and at least 10 years
               mutate(years_running = as.numeric((end_date - begin_date)/365)) %>% 
               filter(years_running >= record_min_yrs) %>% 
               filter(end_date >= as.Date(recent_date))),
  # Match Q sites back to those that have SC so that we only
  # download what we need
  tar_target(conus_q_availability, 
             conus_q_availability_all %>% 
               filter(site_no %in% conus_sc_availability$site_no)),
  
  # Use results from search for available data to create download groups of 
  # sites based on service and chunks of sites. This is borrowed from Lauren 
  # Koenig's (USGS) brilliant function at 
  # https://github.com/USGS-R/ds-pipelines-targets-example-wqp/blob/main/2_download/src/fetch_wqp_data.R
  tar_target(conus_q_download_info, {
    max_results <- 250000
    max_sites <- 2000
    df_grouped <- conus_q_availability %>%
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
  tar_target(conus_q_download_grps,
             conus_q_download_info %>% 
               select(site_no, data_type_cd, task_num) %>% 
               group_by(task_num) %>% 
               tar_group(),
             iteration = "group"),
  
  ##### Download discharge #####
  
  # Actually download some data based on the download groups
  # across sites, services, and time
  tar_target(conus_q_data_raw_feather,{
    out_file <- sprintf('1_fetch/tmp/daily_q_raw_%s_start_%s.feather', 
                        unique(conus_q_download_grps$task_num),
                        gsub("-","_",time_chunks$startDate))
    readNWISdata(siteNumber = conus_q_download_grps$site_no,
                 service = unique(conus_q_download_grps$data_type_cd),
                 parameterCd = '00060',
                 startDate = time_chunks$startDate,
                 endDate = time_chunks$endDate) %>% 
      arrow::write_feather(out_file)
    return(out_file)
  }, pattern = cross(conus_q_download_grps, time_chunks), format='file'),
  
  # Download site metadata & create state-site xwalk
  tar_target(conus_q_site_metadata,
             readNWISsite(unique(conus_q_download_info$site_no))),
  tar_target(conus_q_site_state_xwalk,
             conus_q_site_metadata %>%
               mutate(state_abbr = dataRetrieval::stateCdLookup(state_cd)) %>%
               select(site_no, state_abbr)),
  
  tar_target(conus_q_data_clean_feather, {
    out_file <- gsub('tmp', 'out', gsub('raw_', '', conus_q_data_raw_feather))
    raw_data <- read_feather(conus_q_data_raw_feather) 
    
    # Setup desired column structures
    q_cols <- tibble(max_q = as.numeric(NA),
                      min_q = as.numeric(NA),
                      mean_q = as.numeric(NA))
    rename_xwalk <- c(
      max_q = "X_00060_00001",
      min_q = "X_00060_00002",
      mean_q = "X_00060_00003"
    )
    
    if(any(names(raw_data) %in% rename_xwalk)) {
      # Keep mean, min, and max daily stat codes but insert NAs when they don't exist
      prepped_data <- raw_data %>% 
        # Some of the raw data do not have all of the stat code columns,
        # so need to add a column of NAs when it doesn't exist. The `rename()`
        # followed by the `full_join()` will accomplish this.
        rename(any_of(rename_xwalk)) %>% 
        full_join(q_cols)
    } else {
      # Sometimes there is no data, which can cause issues with selecting columns
      # and joining other data, and so needs to be handled separately
      prepped_data <- raw_data %>% cross_join(q_cols)
    }
    prepped_data %>% 
      select(site_no, dateTime, max_q, min_q, mean_q) %>% 
      left_join(conus_q_site_state_xwalk, by="site_no") %>% 
      arrow::write_feather(out_file)
    return(out_file)
  }, pattern = map(conus_q_data_raw_feather), format = 'file'),
  
  tar_target(conus_q_data, 
             purrr::map(conus_q_data_clean_feather, read_feather) %>% 
               bind_rows()
  ),
  tar_target(conus_q_data_csv, {
    file_out <- '1_fetch/out/conus_q.csv'
    write_csv(conus_q_data, file_out)
    return(file_out)
  }, format="file"),
  
  ##### Gather Air Temperature #####
  
  # Rudimentary air temp so that we can try an initial LSTM model
  tar_target(conus_air_temp_sites, 
             whatNWISdata(siteNumbers = conus_sc_download_grps$site_no) %>% 
               filter(parm_cd == "00020", data_type_cd == "dv") %>% 
               pull(site_no),
             pattern = map(conus_sc_download_grps)),
  tar_target(conus_air_temp, 
             readNWISdata(siteNumbers = conus_air_temp_sites, 
                          parameterCd = "00020", service = "dv",
                          startDate = start_date,
                          endDate = end_date) %>% 
               select(site_no, dateTime, mean_airtemp = X_00020_00003)),
  
  ##### Download transmissivity and depth-to-water table #####
  
  # Download then load the transmissivity and depth-to-water table data from
  # ScienceBase: https://www.sciencebase.gov/catalog/item/60be54f6d34e86b9389117f9
  tar_target(trans_csv, format="file", 
             item_file_download(sb_id = '60be54f6d34e86b9389117f9',
                                names = 'trans.csv',
                                destinations = '1_fetch/out/trans.csv')),
  tar_target(trans, read_csv(trans_csv, col_types = cols()) %>% 
               select(COMID = comid, trans_MEAN = MEAN, trans250, tottrans250)),
  tar_target(dtw_csv, format="file", 
             item_file_download(sb_id = '60be54f6d34e86b9389117f9',
                                names = 'dtw.csv',
                                destinations = '1_fetch/out/dtw.csv')),
  tar_target(dtw, read_csv(dtw_csv, col_types = cols()) %>% 
               select(COMID = comid, dtw_MEAN = MEAN, dtw250, totdtw250)),
  
  ##### Download road salt data #####
  
  tar_target(road_salt_all_zip, format="file", 
             item_file_download(sb_id = '5b15a50ce4b092d9651e22b9',
                                names = '1992_2015.zip',
                                destinations = '1_fetch/out/road_salt_all.zip')),
  # Extract only the 2015 data
  tar_target(road_salt_2015_tif, {
    file_out <- '1_fetch/out/road_salt_2015.tif'
    file_extracted <- '1_fetch/out/2015.tif'
    zip::unzip(zipfile = road_salt_all_zip,
               files = basename(file_extracted),
               exdir = dirname(file_extracted))
    file.rename(from = file_extracted, to = file_out)
    return(file_out)
  }, format = 'file'),
  
  ##### Download NHD Data #####
  
  # Identify the sites & find their matching COMIDs
  tar_target(q_sc_sites_sf, readNWISsite(unique(q_sc_data$site_no)) %>% 
               select(site_no, station_nm, state_cd, huc_cd, dec_long_va, dec_lat_va, drain_area_va) %>% 
               st_as_sf(coords = c('dec_long_va', 'dec_lat_va'), crs = 4326) %>% 
               # These sites caused an error in `get_flowline_index()` that I could not figure out
               filter(!site_no %in% c("05075720", "01104420", "02290947", "02326993", "02326995",
                                      "03353420", "07381324", "10172630", "295124089542100",
                                      "410401112134801"))),
  
  tar_target(q_sc_nhd_comid, {
    sf::sf_use_s2(FALSE)
    # Fetch NHD COMIDs for each site
    nhd_info <- q_sc_sites_sf %>%
      mutate(row_num = row_number()) %>%
      split(.$site_no) %>%
      purrr::map(~{
        if(.x$row_num%%50 == 0) {
          # Print a message for every 50th site so I can see some progress
          message('Starting flow index for ', .x$row_num, ' out of ', nrow(q_sc_sites_sf))
        }
        suppressMessages(get_flowline_index(.x, flines = "download_nhdplusv2"))
      }) %>% bind_rows() %>%
      select(COMID, REACHCODE, REACH_meas)
    
    # Immediately bind site numbers to their corresponding COMIDs
    q_sc_site_df <- q_sc_sites_sf %>% 
      st_drop_geometry() %>% 
      select(site_no)
    nhd_comid_xwalk <- bind_cols(q_sc_site_df, nhd_info)
    
    return(nhd_comid_xwalk)
  }),
  
  tar_target(nhd_huc04_site_xwalk, 
             q_sc_nhd_comid %>%
               mutate(HUC04 = substr(REACHCODE, 1, 4)) %>% 
               select(site_no, HUC04)),
  tar_target(nhd_huc04s, unique(nhd_huc04_site_xwalk$HUC04)),
  tar_target(nhd_huc04s_sf, get_huc(id = nhd_huc04s, type = "huc04")),
  
  ##### Get US sf objects for plotting #####
  
  # States with salt application rates data
  tar_target(conus_salt_sf, 
             usmap::us_map(exclude = c(states_to_exclude, "AK", "HI")) %>% 
               st_as_sf(coords = c('x', 'y'), crs = usmap::usmap_crs()) %>% 
               group_by(group, abbr) %>% 
               summarise(geometry = st_combine(geometry), .groups="keep") %>%
               st_cast("POLYGON")),
  
  # States without salt application rates data 
  tar_target(conus_nosalt_sf, 
             usmap::us_map(include = states_to_exclude) %>% 
               st_as_sf(coords = c('x', 'y'), crs = usmap::usmap_crs()) %>% 
               group_by(group, abbr) %>% 
               summarise(geometry = st_combine(geometry), .groups="keep") %>%
               st_cast("POLYGON"))
  
)
