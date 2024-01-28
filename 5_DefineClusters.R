# Targets for applying a random forest to link attributes
# to each timeseries cluster from `4_ClusterTS`

# source('5_DefineClusters/src/prep_attr_randomforest.R')
# source('5_DefineClusters/src/visualize_attributes_bycluster.R')
# source('5_DefineClusters/src/apply_randomforest.R')
# source('5_DefineClusters/src/evaluate_randomforest.R')

p5_targets <- list(
  
  # TODO: SHOULD I ONLY BE RUNNING RANDOM FOREST FOR THOSE SITES WHICH
  # APPEARED IN A PARTICULAR CLUSTER AT LEAST 50% OF THE TIME??
  # tar_read(p4_dtw_clusters_bySite) %>% group_by(cluster) %>% 
  #   summarize(min_frac = min(frac_years),
  #             max_frac = max(frac_years),
  #             n_sites = n(),
  #             n_over50 = sum(frac_years > 0.5),
  #             n_over50_pct = n_over50/n_sites*100)
  
  ##### Prep data for RF #####
  tar_target(p5_site_attr_rf, 
             prep_attr_randomforest(p4_dtw_clusters_bySite, p3_static_attributes)),
  
  # Visualize attributes by cluster
  tar_target(p5_cluster_attrs_num_viz, 
             visualize_numeric_attrs_by_cluster(p5_site_attr_rf)),
  tar_target(p5_cluster_attrs_trendcount_viz, 
             visualize_categorical_attrs_by_cluster(p5_site_attr_rf)),
  tar_target(p5_cluster_attrs_trendpct_viz, 
             visualize_categorical_attrs_by_cluster(p5_site_attr_rf, plot_percent = TRUE)),
  
  ##### Determine optimal RF configs #####
  tar_target(p5_rf_model_initial, apply_randomforest(p5_site_attr_rf)),
  tar_target(p5_mtry_optimal, optimize_mtry(p5_site_attr_rf)),
  
  ##### Run optimal RF #####
  
  tar_target(p5_rf_model_optimal, apply_randomforest(p5_site_attr_rf, p5_mtry_optimal)),
  
  ##### Evaluate RF output #####
  
  tar_target(p5_rf_attr_importance, calculate_attr_importance(p5_rf_model_optimal)),
  tar_target(p5_rf_attr_importance_viz, visualize_attr_importance(p5_rf_attr_importance)),
  tar_target(p5_rf_attr_importance_simple_viz, visualize_attr_importance(p5_rf_attr_importance,
                                                                         simple = TRUE)),
  tar_target(p5_rf_attr_partdep, calculate_partial_dependence(p5_rf_model_optimal, 
                                                              p5_site_attr_rf)),
  tar_target(p5_rf_attr_partdep_viz, visualize_partial_dependence(p5_rf_attr_partdep))
  
  # TODO: casewise importance vs overall importance?
  # `localImp = TRUE` vs `importance = TRUE`
  
  # https://pubs.acs.org/doi/full/10.1021/acs.est.9b07718
  # https://github.com/imbs-hl/ranger
  # https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=452
  
)
