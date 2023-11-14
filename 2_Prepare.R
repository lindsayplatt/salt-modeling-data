# Targets for transforming data to be used in modeling or 
# site selection in this analysis

source('2_Prepare/src/ts_nwis_fxns.R')
source('2_Prepare/src/ts_gap_fxns.R')
source('2_Prepare/src/attr_prep_fxns.R')
source('2_Prepare/src/attr_combine_all.R')

p2_targets <- list(
  
  ##### TIMESERIES DATA PREP #####
  
  # All are prefixed with `p2_ts_`
  
  ###### TS DATA 1: Calc daily mean SC from instantaneous data ######
  
  tar_target(p2_ts_sc_uv_to_dv_feather, 
             calculate_dv_from_uv(out_file = file.path('2_Prepare/tmp/', 
                                                       gsub('uv', 'uv_to_dv', 
                                                            basename(p1_nwis_sc_data_uv_feather))),
                                  in_file = p1_nwis_sc_data_uv_feather,
                                  site_tz_xwalk = p1_nwis_sc_sites_metadata,
                                  param_colname = 'SpecCond'),
             pattern = map(p1_nwis_sc_data_uv_feather),
             format = 'file'),
  
  ###### TS DATA 2: Combine all daily mean SC data ######
  
  # Also, it replaces values of -999999. 
  tar_target(p2_ts_sc_dv_feather, 
             combine_all_dv_data(out_file = '2_Prepare/tmp/ts_sc_dv.feather',
                                 in_files = c(p2_ts_sc_uv_to_dv_feather,
                                              p1_nwis_sc_data_dv_feather),
                                 param_colname = 'SpecCond'),
             format = 'file'),
  
  ###### TS DATA 3: Fill in missing SC values ######
  
  # TODO: Keep investigating WRTDS for gap-filling. For now, this is simple 
  # linear interpolation for any gap of 5 or fewer days. Do this per site!
  tar_target(p2_ts_sc_dv_bySite, 
             read_feather(p2_ts_sc_dv_feather) %>% 
               group_by(site_no) %>% 
               tar_group(), 
             iteration = 'group'),
  tar_target(p2_ts_sc_dv_gapFilled, fill_ts_gaps(p2_ts_sc_dv_bySite, 
                                                 param_colname = 'SpecCond', 
                                                 max_gap_days = 5),
             pattern = p2_ts_sc_dv_bySite),
  
  # Now summarize the timeseries that were saved by the gap-filling
  tar_target(p2_ts_sc_dv_gapSummary, 
             summarize_gap_fixes(p2_ts_sc_dv_gapFilled, param_colname = 'SpecCond')),
  
  ###### TS DATA 4: Detrend the SC timeseries ######
  
  # TODO: Detrend the SC timeseries since we are using trend as a static attr
  
  ###### TS DATA 5: Split SC timeseries into individual site-years ######
  
  # TODO: Split SC timeseries into site-years
  
  ##### STATIC ATTRIBTUES PREP #####
  
  # All are prefixed with `p2_attr_`
  
  ###### ATTR DATA 1: Collapse Q timeseries to mean Q per site ######
  
  # First, convert instantaneous Q to daily Q
  tar_target(p2_attr_q_uv_to_dv_feather, 
             calculate_dv_from_uv(out_file = file.path('2_Prepare/tmp/', 
                                                       gsub('uv', 'uv_to_dv', 
                                                            basename(p1_nwis_q_data_uv_feather))),
                                  in_file = p1_nwis_q_data_uv_feather,
                                  site_tz_xwalk = p1_nwis_sc_sites_metadata,
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
  # TODO: do we need to do anything about negative streamflows?
  tar_target(p2_attr_meanFlow, calculate_mean_q_per_site(p2_attr_q_dv_feather)),
  
  ###### ATTR DATA 2: Extract road salt application per site ######
  
  tar_target(p2_attr_roadSalt, aggregate_road_salt_per_site(road_salt_tif = p1_sb_road_salt_2015_tif, 
                                                            sites_sf = p1_nwis_sc_sites_sf)),
  
  ###### ATTR DATA 3: Calculate SC trend per site ######
  
  # Calculate SC trends to add as a static attribute
  tar_target(p2_attr_sc_trends, calculate_sc_trend(p2_ts_sc_dv_feather, max_pval = 0.05)),
  
  ###### ATTR DATA 4: Pivot and link NHD+ attributes to sites ######
  
  tar_target(p2_attr_nhd, prepare_nhd_attributes(p1_nhdplus_attr_vals_tbl,
                                                 p1_nwis_site_nhd_comid_xwalk)),
  
  # TODO: add GW signature? transmissivity? depth2wt?
  
  ###### ATTR DATA 5: Combine all static attributes into one table ######
  
  tar_target(p2_attr_all, combine_static_attributes(p2_attr_meanFlow,
                                                    p2_attr_roadSalt,
                                                    p2_attr_sc_trends,
                                                    p2_attr_nhd))
  
)
