# Targets for transforming data to be used in modeling or 
# site selection in this analysis

source('2_Prepare/src/ts_nwis_fxns.R')
source('2_Prepare/src/ts_gap_fxns.R')
source('2_Prepare/src/ts_detrend_fxns.R')
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
  
  # First, we need to identify sites that should be gap-filled. Since WRTDS takes
  # a lot of time to run per site, I don't want to waste resources running it
  # for sites that we won't use anyways. So, this means that before we gap-fill,
  # we have ...
  #   1. Filtered to sites that meet our date range criteria. See the targets
  #      that create `p3_attr_q_qualified` and `p3_ts_sc_qualified` in `3_Filter`.
  #   2. Remove sites that have negative flows (WRTDS does NOT play nice with 
  #      negatives). TODO: Later look into what to do with negative flows.
  #   3. Remove sites that don't actually have any missing SC values on dates where
  #      there is a flow value (we can't gap-fill with WRTDS where Q is missing).
  
  tar_target(p2_ts_sc_to_gapfill, 
             prep_data_for_wrtds(
               p3_attr_q_qualified, p3_ts_sc_qualified,
               param_colname = 'SpecCond',
               # Skipping a couple of sites that I can't figure out
               # TODO: Maybe return to these in the future with an EGRET dev help
               #  1. 05055400: finishes survival regression in `modelEstimation()` but 
               #      then errors with "replacement has 3428 rows, data has 4486"
               #  2. 11336600: fails with some weird `Backtrace` error
               sites_that_crash = c('05055400', '11336600')
             ) %>% 
               group_by(site_no) %>% 
               tar_group(),
             iteration = 'group'),
  
  
  # Run WRTDS to produce complete timeseries of SC data per site
  # Note that this step will be lengthy. 
  # Nov 24-25, 2023: This took 21 hours for 331 sites (avg of 4 min per site)!!!
  tar_target(p2_ts_sc_WRTDS, 
             apply_wrtds(data_q_param = p2_ts_sc_to_gapfill,
                         param_colname = 'SpecCond',
                         param_nwis_cd = p1_nwis_pcode_sc),
             pattern = map(p2_ts_sc_to_gapfill)),
  # TODO: boxplot of WRTDS SE
  tar_target(p2_ts_sc_gapFilled, fill_ts_gaps_wrtds(p3_ts_sc_qualified, 
                                                       p2_ts_sc_WRTDS,
                                                       param_colname = 'SpecCond')),
  
  # Now summarize the timeseries that were saved by the gap-filling
  # TODO: scrutinize this output. Was expecting higher numbers, I think?
  tar_target(p2_ts_sc_gapSummary, 
             summarize_gap_fixes(p2_ts_sc_gapFilled, param_colname = 'SpecCond')),
  
  ###### TS DATA 4: Detrend the SC timeseries ######
  
  # Detrend the SC timeseries since we are using trend as a static attr
  # Detrend only works for sites without any NAs in their time series
  tar_target(p2_ts_sc_detrend_sites, identify_sites_to_detrend(p2_ts_sc_dv_gapFilled, 'SpecCond')),
  tar_target(p2_ts_sc_dv_detrend, detrend_ts_data(p2_ts_sc_dv_gapFilled, p2_ts_sc_detrend_sites, 'SpecCond')),
  
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
  tar_target(p2_attr_meanFlow, calculate_mean_q_per_site(p3_attr_q_qualified)),
  
  ###### ATTR DATA 2: Extract road salt application per site ######
  
  tar_target(p2_attr_roadSalt, aggregate_road_salt_per_site(road_salt_tif = p1_sb_road_salt_2015_tif, 
                                                            sites_sf = p1_nwis_sc_sites_sf)),
  
  ###### ATTR DATA 3: Calculate SC trend per site ######
  
  # Calculate SC trends to add as a static attribute
  # This calculates the trend only for data that meets our criteria from 3_Filter
  tar_target(p2_attr_sc_trends, calculate_sc_trend(p3_ts_sc_qualified, max_pval = 0.05)),
  
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
