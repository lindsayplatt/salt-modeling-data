# Targets for applying a clustering algorithm (e.g. dynamic 
# time-warping) to the salinity timeseries data

source('4_ClusterTS/src/ts_cluster_prep.R')
source('4_ClusterTS/src/apply_dtw.R')

p4_targets <- list(
  
  
  ##### PREP DATA: specific changes needed for the clustering algorithm #####
  
  ###### PREP DATA 1: Z-score SC data per site ######
  
  tar_target(p4_ts_sc_norm, normalize_data_bysite(p2_ts_sc, 'SpecCond')),
  
  ###### PREP DATA 2: Split SC time series into list of individual site-years ######
  
  tar_target(p4_ts_sc_list, convert_ts_data_into_list(p4_ts_sc_norm, 'SpecCond')),
  
  ##### DETERMINE OPTIMAL CLUSTERING MODEL #####
  
  # 2 clusters with a 5-day window was running for over 2.5 hrs
  # and hadn't converged.
  # 3 clusters with 30 day window was running for 9 hours ... no progress.
  # Switched distance method to `dtw_basic` and in ~5.5 hours I was able to
  # run models for clusters 7-10 with windows 10, 30, and 60
  tar_target(p4_cluster_opts, 2:10),
  tar_target(p4_window_opts, c(5, 10, 20, 30, 45, 60)),
  tar_target(p4_distance_methods, c('dtw_basic')), # Using dtw_basic because it is faster
  tar_target(p4_centroid_methods, c('pam')),
  
  # Apply DTW Map over the combinations of clusters and windows to try
  tar_target(p4_dtw_tests_out_qs, 
             apply_dynamic_timewarping(out_file = sprintf('4_ClusterTS/tmp/output_dist_%s_centroid_%s_cluster_%02d_window_%02d.qs',
                                                          p4_distance_methods, p4_centroid_methods, p4_cluster_opts, p4_window_opts),
                                       p4_ts_sc_list, 
                                       p4_cluster_opts, 
                                       p4_window_opts,
                                       p4_distance_methods,
                                       p4_centroid_methods),
             pattern = cross(p4_cluster_opts, p4_window_opts, p4_distance_methods, p4_centroid_methods),
             format = 'file'),
  
  # TODO: work on how best to evaluate the clustering models
  # Probably won't be able to load all at once, as I have here.
  tar_target(p4_dtw_tests_evaluate, 
             evaluate_dtw_clusters(p4_dtw_tests_out_qs),
             pattern = map(p4_dtw_tests_out_qs)),
  
  ##### USE OPTIMAL CLUSTERING MODEL ##### 
  
  # TODO: ADD FRACTION OF YEARS PER CLUSTER
  
)
