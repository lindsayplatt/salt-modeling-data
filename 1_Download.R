# Targets for downloading data used in this analysis

source('1_Download/src/nwis_fxns.R')
source('1_Download/src/retry_fxns.R')

p1_targets <- list(
  
  # Define target listing the state abbreviations within the Contiguous United States (CONUS)
  tar_target(p1_conus_state_cds, state.abb[!state.abb %in% c('AK', 'HI')]), 
  
  ##### NWIS DATA: Download SC and Q {75 MIN} #####
  
  ###### NWIS DATA 0: Set download configs for streamflow (Q) & specific conductivity (SC) ######
  
  tar_target(p1_nwis_start_date, as.Date('1950-01-01')), 
  tar_target(p1_nwis_end_date, as.Date('2022-12-31')),
  tar_target(p1_nwis_pcode_sc, '00095'), # NWIS specific conductance code
  tar_target(p1_nwis_pcode_q, '00060'), # NWIS streamflow code
  tar_target(p1_nwis_min_years, 10), # Minimum number of years required
  tar_target(p1_nwis_min_end_date, as.Date('2000-01-01')), # Sites must have at least 1 record more recent than this
  
  ###### NWIS DATA 1: Identify sites with continuous SC (by state) ######
  tar_target(p1_nwis_sc_sites_all,
             inventory_nwis_sites_byState(state_cd = p1_conus_state_cds, 
                                          param_cd = p1_nwis_pcode_sc, 
                                          start_date = p1_nwis_start_date,
                                          end_date = p1_nwis_end_date),
             pattern = map(p1_conus_state_cds),
             iteration = 'list'),
  
  ###### NWIS DATA 2: Inventory NWIS & only keep sites with mean daily or instantaneous SC (by state) ######
  tar_target(p1_nwis_sc_data_info, inventory_nwis_data(site_numbers = p1_nwis_sc_sites_all, 
                                                       param_cd = p1_nwis_pcode_sc,
                                                       start_date = p1_nwis_start_date,
                                                       end_date = p1_nwis_end_date),
             pattern = map(p1_nwis_sc_sites_all),
             iteration = 'list'),
  
  ###### NWIS DATA 3: Filter SC sites for `min_yrs` and `min_end_date` (by state) ######
  tar_target(p1_nwis_sc_sites_qualified, 
             filter_to_min_data_standards(p1_nwis_sc_data_info, 
                                          min_yrs = p1_nwis_min_years, 
                                          min_end_date = p1_nwis_min_end_date),
             pattern = map(p1_nwis_sc_data_info),
             iteration = 'list'),
  
  ###### NWIS DATA 4: Identify SC sites that also have mean daily or instantaneous Q (by state) ######
  # The Q data should meet the same minimum data standards as SC. That filtering is combined into
  # a single step below. Notice there is no `iteration = 'list'` because we want the output to be
  # a single table, not a list of tables by state as we have had above.
  tar_target(p1_nwis_q_sites_query,
             inventory_nwis_data(site_numbers = p1_nwis_sc_sites_qualified$site_no, 
                                 param_cd = p1_nwis_pcode_q,
                                 start_date = p1_nwis_start_date,
                                 end_date = p1_nwis_end_date) %>% 
               filter_to_min_data_standards(min_yrs = p1_nwis_min_years, 
                                            min_end_date = p1_nwis_min_end_date),
             pattern = map(p1_nwis_sc_sites_qualified)),
  
  # Using the sites that also have qualified Q, filter the SC site list before querying
  tar_target(p1_nwis_sc_sites_query, p1_nwis_sc_sites_qualified %>% 
               filter(site_no %in% p1_nwis_q_sites_query$site_no),
             pattern = map(p1_nwis_sc_sites_qualified)),
  
  ###### NWIS DATA 5: Prepare to download all the SC data ###### 
  # Separate by the service and download groups. 
  # DV = daily data
  # UV = instantaneous data (downloading only if mean daily values aren't available)
  tar_target(p1_nwis_sc_sites_query_uv, filter(p1_nwis_sc_sites_query, query_service == 'uv')),
  tar_target(p1_nwis_sc_sites_query_uv_download_grps,
             p1_nwis_sc_sites_query_uv %>% 
               # Set `max_results` much lower than default for `uv` because 1 day = 1440 records
               add_download_grp(max_results = 15000) %>% 
               group_by(task_num) %>% 
               tar_group(),
             iteration = 'group'),
  tar_target(p1_nwis_sc_sites_query_dv, filter(p1_nwis_sc_sites_query, query_service == 'dv')),
  tar_target(p1_nwis_sc_sites_query_dv_download_grps,
             p1_nwis_sc_sites_query_dv %>% 
               add_download_grp() %>% 
               group_by(task_num) %>% 
               tar_group(),
             iteration = 'group'),
  
  ###### NWIS DATA 6: Download the SC data and save as files in `1_Download/out_nwis` ######
  
  tar_target(p1_nwis_sc_data_uv_feather,
             download_nwis_data(
               out_file = sprintf('1_Download/out_nwis/sc_uv_%03d.feather',
                                  unique(p1_nwis_sc_sites_query_uv_download_grps$task_num)),
               site_numbers = p1_nwis_sc_sites_query_uv_download_grps$site_no,
               param_cd = p1_nwis_pcode_sc,
               start_date = p1_nwis_start_date,
               end_date = p1_nwis_end_date,
               service_cd = 'uv'
             ),
             pattern = map(p1_nwis_sc_sites_query_uv_download_grps),
             format = 'file'),
  tar_target(p1_nwis_sc_data_dv_feather, 
             download_nwis_data(
               out_file = sprintf('1_Download/out_nwis/sc_dv_%03d.feather', 
                                  unique(p1_nwis_sc_sites_query_dv_download_grps$task_num)),
               site_numbers = p1_nwis_sc_sites_query_dv_download_grps$site_no,
               param_cd = p1_nwis_pcode_sc, 
               start_date = p1_nwis_start_date,
               end_date = p1_nwis_end_date,
               service_cd = 'dv'
             ),
             pattern = map(p1_nwis_sc_sites_query_dv_download_grps),
             format = 'file'),
  
  ###### NWIS DATA 7: Prepare to download all the Q data ###### 
  # Separate by the service and download groups.
  # DV = daily data
  # UV = instantaneous data (downloading only if mean daily values aren't available
  tar_target(p1_nwis_q_sites_query_uv, filter(p1_nwis_q_sites_query, query_service == 'uv')),
  tar_target(p1_nwis_q_sites_query_uv_download_grps,
             p1_nwis_q_sites_query_uv %>% 
               # Set `max_results` much lower than default for `uv` because 1 day = 1440 records
               add_download_grp(max_results = 15000) %>% 
               group_by(task_num) %>% 
               tar_group(),
             iteration = 'group'),
  tar_target(p1_nwis_q_sites_query_dv, filter(p1_nwis_q_sites_query, query_service == 'dv')),
  tar_target(p1_nwis_q_sites_query_dv_download_grps,
             p1_nwis_q_sites_query_dv %>% 
               add_download_grp() %>% 
               group_by(task_num) %>% 
               tar_group(),
             iteration = 'group'),
  
  ###### NWIS DATA 8: Download the Q data and save as files in `1_Download/out_nwis` ######
  tar_target(p1_nwis_q_data_uv_feather,
             download_nwis_data(
               out_file = sprintf('1_Download/out_nwis/q_uv_%03d.feather',
                                  unique(p1_nwis_q_sites_query_uv_download_grps$task_num)),
               site_numbers = p1_nwis_q_sites_query_uv_download_grps$site_no,
               param_cd = p1_nwis_pcode_q,
               start_date = p1_nwis_start_date,
               end_date = p1_nwis_end_date,
               service_cd = 'uv'
             ),
             pattern = map(p1_nwis_q_sites_query_uv_download_grps),
             format = 'file'),
  tar_target(p1_nwis_q_data_dv_feather, 
             download_nwis_data(
               out_file = sprintf('1_Download/out_nwis/q_dv_%03d.feather', 
                                  unique(p1_nwis_q_sites_query_dv_download_grps$task_num)),
               site_numbers = p1_nwis_q_sites_query_dv_download_grps$site_no,
               param_cd = p1_nwis_pcode_q, 
               start_date = p1_nwis_start_date,
               end_date = p1_nwis_end_date,
               service_cd = 'dv'
             ),
             pattern = map(p1_nwis_q_sites_query_dv_download_grps),
             format = 'file')
  
)
