# Targets for applying a clustering algorithm (e.g. dynamic 
# time-warping) to the salinity timeseries data

source('4_ClusterTS/src/ts_cluster_prep.R')
source('4_ClusterTS/src/apply_dtw.R')
source('4_ClusterTS/src/evaluate_dtw.R')


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
  
  # While the function can handle multiple qs files at once, added mapping
  # so that we weren't trying to work with too much data in memory.
  tar_target(p4_dtw_tests_evaluate, 
             evaluate_dtw(p4_dtw_tests_out_qs),
             pattern = map(p4_dtw_tests_out_qs)),
  
  ##### USE OPTIMAL CLUSTERING MODEL ##### 
  
  # Using COP index & "elbow" method with the cluster distance values,
  # I determined that 7 clusters with a window size of 20 was optimal.
  
  tar_target(p4_distance_optimal, 'dtw_basic'), # TODO: switch to `dtw`
  tar_target(p4_centroid_optimal, 'pam'), # DBA had issues with turning into a flat line.
  tar_target(p4_cluster_optimal, 7),
  tar_target(p4_window_optimal, 20),
  tar_target(p4_dtw_optimal_qs, 
             apply_dynamic_timewarping(out_file = sprintf('4_ClusterTS/out/output_dist_%s_centroid_%s_cluster_%02d_window_%02d.qs',
                                                          p4_distance_optimal, p4_centroid_optimal, p4_cluster_optimal, p4_window_optimal),
                                       p4_ts_sc_list, 
                                       p4_cluster_optimal, 
                                       p4_window_optimal,
                                       p4_distance_optimal,
                                       p4_centroid_optimal),
             format = 'file'),
  
  ##### Process clustering output into usable tibbles #####
  
  tar_target(p4_dtw_clusters_bySiteYear, 
             process_dtw_clusters_bySiteYear(p4_dtw_optimal_qs, p4_ts_sc_list)),
  tar_target(p4_dtw_clusters_bySite, 
             choose_single_cluster_per_site(p4_dtw_clusters_bySiteYear))
  
)
