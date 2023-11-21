# Targets for filtering sites and data based on specific citeria

source('3_Filter/src/ts_gap_filtering.R')
source('3_Filter/src/ts_qualification.R')

p3_targets <- list(
  
  # Some of these steps are happening *BEFORE* the gap-filling steps in 
  # 2_Prepare because WRTDS is an expensive computational step and we 
  # only want to fill gaps for those sites that pass our other criteria.
  
  # Before running WRTDS, remove any data that is too large of a gap to attempt
  # to fill. Only apply gap-filling to data more recent than the final large gap
  # (currently defined as 3 years of sequential NAs)
  tar_target(p3_ts_sc_rmLargeGaps,
             filter_beyond_large_ts_gaps(p2_ts_sc_dv_feather,
                                         param_colname = 'SpecCond')),
  
  # Filter the prepared data to sites with at least 5 years and some 
  # of those years occurring in the last 15 years.
  tar_target(p3_ts_sc_qualified_sites, 
             identify_qualifying_sites(p3_ts_sc_rmLargeGaps)),
  tar_target(p3_ts_sc_qualified, 
             p3_ts_sc_rmLargeGaps %>% 
               filter(site_no %in% p3_ts_sc_qualified_sites))
  
)
