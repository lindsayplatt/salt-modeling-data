# Targets for applying a clustering algorithm (e.g. dynamic 
# time-warping) to the salinity timeseries data

source('4_ClusterTS/src/ts_cluster_prep.R')

p4_targets <- list(
  
  
  ##### PREP DATA: specific changes needed for the clustering algorithm #####
  
  ###### PREP DATA 1: Z-score SC data per site ######
  
  tar_target(p4_ts_sc_norm, normalize_data_bysite(p2_ts_sc, 'SpecCond')),
  
  ###### PREP DATA 2: Split SC time series into list of individual site-years ######
  
  tar_target(p4_ts_sc_list, convert_ts_data_into_list(p4_ts_sc_norm, 'SpecCond'))
  
)
