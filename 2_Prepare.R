# Targets for transforming data to be used in modeling or 
# site selection in this analysis

source('2_Prepare/src/ts_nwis_fxns.R')
source('2_Prepare/src/attr_nwis_fxns.R')

p2_targets <- list(
  
  ##### TIMESERIES DATA PREP #####
  
  # All are prefixed with `p2_ts_`
  
  ###### TS DATA 1: Calc daily mean SC from instantaneous data ######
  
  tar_target(p2_ts_sc_uv_to_dv_feather, 
             calculate_dv_from_uv(out_file = file.path('2_Prepare/tmp/', 
                                                       gsub('uv', 'uv_to_dv', 
                                                            basename(p1_nwis_sc_data_uv_feather))),
                                  in_file = p1_nwis_sc_data_uv_feather,
                                  site_tz_xwalk = NULL,
                                  param_colname = 'SpecCond'),
             pattern = map(p1_nwis_sc_data_uv_feather),
             format = 'file'),
  
  ###### TS DATA 2: Combine all daily mean SC data ######
  
  tar_target(p2_ts_sc_dv_feather, 
             combine_all_dv_data(out_file = '2_Prepare/tmp/ts_sc_dv.feather',
                                 in_files = c(p2_ts_sc_uv_to_dv_feather,
                                              p1_nwis_sc_data_dv_feather),
                                 param_colname = 'SpecCond'),
             format = 'file'),
  
  ###### TS DATA 3: Fill in missing SC values ######
  
  # TODO: Fill in data gaps. Figure out the appropriate gap size and method.
  
  ##### STATIC ATTRIBTUES PREP #####
  
  # All are prefixed with `p2_attr_`
  
  ###### ATTR DATA 1: Collapse Q timeseries to mean Q per site ######
  
  # First, convert instantaneous Q to daily Q
  tar_target(p2_attr_q_uv_to_dv_feather, 
             calculate_dv_from_uv(out_file = file.path('2_Prepare/tmp/', 
                                                       gsub('uv', 'uv_to_dv', 
                                                            basename(p1_nwis_q_data_uv_feather))),
                                  in_file = p1_nwis_q_data_uv_feather,
                                  site_tz_xwalk = NULL,
                                  param_colname = 'Flow'),
             pattern = map(p1_nwis_q_data_uv_feather),
             format = 'file'),
  
  # Then, combine all daily Q
  tar_target(p2_attr_q_dv_feather, 
             combine_all_dv_data(out_file = '2_Prepare/tmp/attr_q_dv.feather',
                                 in_files = c(p2_attr_q_uv_to_dv_feather,
                                              p1_nwis_q_data_dv_feather),
                                 param_colname = 'Flow'),
             format = 'file'),
  
  # Then, find a single mean daily Q value per site
  tar_target(p2_attr_mean_q, calculate_mean_q_per_site(p2_attr_q_dv_feather))
  
  ###### ATTR DATA 2: Extract road salt application per site ######
  
  ###### ATTR DATA 3: Calculate SC trend per site ######
  
  ###### ATTR DATA 4: Combine all static attributes into one table ######
  
)
