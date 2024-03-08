# Targets for applying a random forest to link attributes
# to each site that is experiencing "episodic salinization"
# or "baseflow salinization" or both.

source('6_DefineCharacteristics/src/prep_attr_randomforest.R')
source('6_DefineCharacteristics/src/apply_randomforest.R')
source('6_DefineCharacteristics/src/evaluate_randomforest.R')
source('6_DefineCharacteristics/src/visualize_attribute_distributions.R')
source('6_DefineCharacteristics/src/visualize_results.R')

p6_targets <- list(
  
  ##### Run random forest for both episodic & baseflow salinization #####
  
  ###### Prep data for RF ######
  
  tar_target(p6_site_attr, prep_attr_randomforest(p3_static_attributes, p4_episodic_sites, p5_sc_baseflow_trend)),
  tar_target(p6_site_attr_rf, dplyr::select(p6_site_attr, -site_no)),
  
  ###### Determine optimal RF configs ######
  
  tar_target(p6_rf_model_initial, apply_randomforest(p6_site_attr_rf)),
  
  # Find and use optimal `mtry` to minimize OOB error
  tar_target(p6_mtry_optimal_all_attrs, optimize_mtry(p6_site_attr_rf)),
  tar_target(p6_rf_model_minOOB, apply_randomforest(p6_site_attr_rf, p6_mtry_optimal_all_attrs)),
  
  # Trim the number of attributes used by only choosing those that appeared as
  # one of the top 10 most important in one of the categories.
  tar_target(p6_site_attr_rf_optimal, optimize_attrs(p6_site_attr_rf, p6_rf_model_minOOB)),
  
  # Now with the new site attributes re-calculate the optimal `mtry`
  tar_target(p6_mtry_optimal, optimize_mtry(p6_site_attr_rf_optimal)),
  
  ###### Run optimal RF ######
  
  tar_target(p6_rf_model_optimal, apply_randomforest(p6_site_attr_rf_optimal, p6_mtry_optimal)),
  
  ###### Evaluate RF output ######
  
  tar_target(p6_rf_attr_importance, calculate_attr_importance(p6_rf_model_optimal)),
  tar_target(p6_rf_attr_importance_viz, visualize_attr_importance(p6_rf_attr_importance)),
  tar_target(p6_rf_attr_partdep, calculate_partial_dependence(p6_rf_model_optimal, 
                                                              p6_site_attr_rf)),
  tar_target(p6_rf_attr_partdep_viz, visualize_partial_dependence(p6_rf_attr_partdep, p6_site_attr_rf)),
  
  ###### Visualize site category attribute distributions ######
  
  tar_target(p6_attrs_num_viz, visualize_numeric_attrs(p6_site_attr)),
  tar_target(p6_attrs_num_optimal_viz, visualize_numeric_attrs(p6_site_attr_rf_optimal)),
  tar_target(p6_category_map, visualize_catgory_sites_map(p6_site_attr, p1_nwis_sc_sites_sf, p1_conus_state_cds)),
  
  ##### Run random forest for just episodic #####
  
  ###### Prep data for RF ######
  
  tar_target(p6b_site_attr, prep_attr_randomforest(p3_static_attributes, sites_episodic = p4_episodic_sites)),
  tar_target(p6b_site_attr_rf, dplyr::select(p6b_site_attr, -site_no)),
  
  ###### Determine optimal RF configs ######
  
  tar_target(p6b_rf_model_initial, apply_randomforest(p6b_site_attr_rf)),
  
  # Find and use optimal `mtry` to minimize OOB error
  tar_target(p6b_mtry_optimal_all_attrs, optimize_mtry(p6b_site_attr_rf)),
  tar_target(p6b_rf_model_minOOB, apply_randomforest(p6b_site_attr_rf, p6b_mtry_optimal_all_attrs)),
  
  # Trim the number of attributes used by only choosing those that appeared as
  # one of the top 10 most important in one of the categories.
  tar_target(p6b_site_attr_rf_optimal, optimize_attrs(p6b_site_attr_rf, p6b_rf_model_minOOB)),
  
  # Now with the new site attributes re-calculate the optimal `mtry`
  tar_target(p6b_mtry_optimal, optimize_mtry(p6b_site_attr_rf_optimal)),
  
  ###### Run optimal RF ######
  
  tar_target(p6b_rf_model_optimal, apply_randomforest(p6b_site_attr_rf_optimal, p6b_mtry_optimal)),
  
  ###### Evaluate RF output ######
  
  tar_target(p6b_rf_attr_importance, calculate_attr_importance(p6b_rf_model_optimal)),
  tar_target(p6b_rf_attr_importance_viz, visualize_attr_importance(p6b_rf_attr_importance)),
  tar_target(p6b_rf_attr_partdep, calculate_partial_dependence(p6b_rf_model_optimal, 
                                                               p6b_site_attr_rf)),
  tar_target(p6b_rf_attr_partdep_viz, visualize_partial_dependence(p6b_rf_attr_partdep, p6b_site_attr_rf)),
  
  ###### Visualize site category attribute distributions ######
  
  tar_target(p6b_attrs_num_viz, visualize_numeric_attrs(p6b_site_attr_rf_optimal)),
  tar_target(p6b_category_map, visualize_catgory_sites_map(p6b_site_attr, p1_nwis_sc_sites_sf, p1_conus_state_cds)),
  
  ##### Run random forest for just baseflow salinization #####
  
  ###### Prep data for RF ######
  
  tar_target(p6c_site_attr, prep_attr_randomforest(p3_static_attributes, site_baseflow_trend_info = p5_sc_baseflow_trend)),
  tar_target(p6c_site_attr_rf, dplyr::select(p6c_site_attr, -site_no)),
  
  ###### Determine optimal RF configs ######
  
  tar_target(p6c_rf_model_initial, apply_randomforest(p6c_site_attr_rf)),
  
  # Find and use optimal `mtry` to minimize OOB error
  tar_target(p6c_mtry_optimal_all_attrs, optimize_mtry(p6c_site_attr_rf)),
  tar_target(p6c_rf_model_minOOB, apply_randomforest(p6c_site_attr_rf, p6c_mtry_optimal_all_attrs)),
  
  # Trim the number of attributes used by only choosing those that appeared as
  # one of the top 10 most important in one of the categories.
  tar_target(p6c_site_attr_rf_optimal, optimize_attrs(p6c_site_attr_rf, p6c_rf_model_minOOB)),
  
  # Now with the new site attributes re-calculate the optimal `mtry`
  tar_target(p6c_mtry_optimal, optimize_mtry(p6c_site_attr_rf_optimal)),
  
  ###### Run optimal RF ######
  
  tar_target(p6c_rf_model_optimal, apply_randomforest(p6c_site_attr_rf_optimal, p6c_mtry_optimal)),
  
  ###### Evaluate RF output ######
  
  tar_target(p6c_rf_attr_importance, calculate_attr_importance(p6c_rf_model_optimal)),
  tar_target(p6c_rf_attr_importance_viz, visualize_attr_importance(p6c_rf_attr_importance)),
  tar_target(p6c_rf_attr_partdep, calculate_partial_dependence(p6c_rf_model_optimal, 
                                                               p6c_site_attr_rf)),
  tar_target(p6c_rf_attr_partdep_viz, visualize_partial_dependence(p6c_rf_attr_partdep, p6c_site_attr_rf)),
  
  ###### Visualize site category attribute distributions ######
  
  tar_target(p6c_attrs_num_viz, visualize_numeric_attrs(p6c_site_attr_rf_optimal)),
  tar_target(p6c_category_map, visualize_catgory_sites_map(p6c_site_attr, p1_nwis_sc_sites_sf, p1_conus_state_cds)),
  
  ##### Save resulting RF outputs as PNGs #####
  
  # Saving in the `log/` folder and committing so that we can git compare future versions
  
  tar_target(p6_rf_results_png, create_summary_view(
    '6_DefineCharacteristics/log/random_forest_results_combined.png',
    p6_rf_attr_partdep_viz,
    p6_attrs_num_viz,
    p6_rf_attr_importance_viz,
    p6_category_map
  ), format='file'), 
  
  tar_target(p6b_rf_results_png, create_summary_view(
    '6_DefineCharacteristics/log/random_forest_results_episodic.png',
    p6b_rf_attr_partdep_viz,
    p6b_attrs_num_viz, 
    p6b_rf_attr_importance_viz,
    p6b_category_map
  ), format='file'), 
  
  tar_target(p6c_rf_results_png, create_summary_view(
    '6_DefineCharacteristics/log/random_forest_results_baseflow.png',
    p6c_rf_attr_partdep_viz,
    p6c_attrs_num_viz, 
    p6c_rf_attr_importance_viz,
    p6c_category_map
  ), format='file'),
  
  # Saving attribute correlations as a file
  tar_target(p6_attr_correlations_png, 
             visualize_attr_correlation('6_DefineCharacteristics/log/attr_correlations.png',
                                        p6_site_attr_rf), 
             format='file')
  
)
