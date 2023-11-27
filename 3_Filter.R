# Targets for filtering sites and data based on specific citeria

source('3_Filter/src/ts_gap_filtering.R')
source('3_Filter/src/ts_qualification.R')

p3_targets <- list(
  
  ##### TS FILTERING: Filter sites and data as part of processing in `2_Prepare` #####
  
  # Some of these steps are happening *BEFORE* the gap-filling steps in 
  # 2_Prepare because WRTDS is an expensive computational step and we 
  # only want to fill gaps for those sites that pass our other criteria.
  
  ##### Step 1: remove very large gaps in timeseries data #####
  
  # Before running WRTDS, remove any data that is too large of a gap to attempt
  # to fill. Only apply gap-filling to data more recent than the final large gap
  # (currently defined as 3 years of sequential NAs)
  tar_target(p3_ts_sc_rmLargeGaps,
             filter_beyond_large_ts_gaps(p2_ts_sc_dv_feather,
                                         param_colname = 'SpecCond',
                                         large_gap_days = 365*2)),
  
  ##### Step 2: identify sites that meet (or don't) certain criteria #####
  
  # Identify the sites with at least 5 years and some of those years occurring 
  # in the last 15 years (will keep them). Note that these need to be identified
  # *after* the removal of the large gaps above.
  tar_target(p3_ts_sc_temporal_qualified_sites, 
             identify_temporal_qualifying_sites(p3_ts_sc_rmLargeGaps)),
  
  # Identify sites that may be influenced by agriculture (will remove them)
  tar_target(p3_ts_sc_ag_sites, identify_ag_sites(p2_ag_attr_nhd)),
  
  # Identify sites that have suspiciously high SC (will remove them)
  tar_target(p3_ts_sc_highSC_sites, identify_highSC_sites(p3_ts_sc_rmLargeGaps)),
  
  ##### Step 3: filter data to just those sites that match our requirements #####
  
  # Filter the data to just those sites that match our requirements
  tar_target(p3_ts_sc_qualified, 
             filter_data_to_qualifying_sites(p3_ts_sc_rmLargeGaps, 
                                             keep_sites = p3_ts_sc_temporal_qualified_sites,
                                             remove_sites = c(p1_nwis_sc_sites_tidal,
                                                              p3_ts_sc_ag_sites,
                                                              p3_ts_sc_highSC_sites))),
  
  # Do the same for Flow data.
  tar_target(p3_attr_q_qualified, 
             read_feather(p2_attr_q_dv_feather) %>% 
               filter_data_to_qualifying_sites(keep_sites = p3_ts_sc_temporal_qualified_sites,
                                               remove_sites = c(p1_nwis_sc_sites_tidal,
                                                                p3_ts_sc_ag_sites,
                                                                p3_ts_sc_highSC_sites)))
  
)
