# Targets for applying a random forest to link attributes
# to each site that is experiencing "episodic salinization"
# or "baseflow salinization" or both.

source('6_DefineCharacteristics/src/prep_attr_randomforest.R')
source('6_DefineCharacteristics/src/apply_randomforest.R')
source('6_DefineCharacteristics/src/evaluate_randomforest.R')
source('6_DefineCharacteristics/src/visualize_attribute_distributions.R')

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
  
  ##### Save PDF of all resulting RF #####
  
  tar_target(p6_rf_results_pdf, {
    out_file <- '6_DefineCharacteristics/out/random_forest_results.pdf'
    
    # Prep plot views for each type
    both_rf_plots <- list(
      p6_attrs_num_viz,
      p6_rf_attr_partdep_viz,
      p6_rf_attr_importance_viz,
      p6_category_map
    )
    
    episodic_rf_plots <- list(
      p6b_attrs_num_viz, 
      p6b_rf_attr_partdep_viz,
      p6b_rf_attr_importance_viz,
      p6b_category_map
    )
    
    baseflow_rf_plots <- list(
      p6c_attrs_num_viz, 
      p6c_rf_attr_partdep_viz,
      p6c_rf_attr_importance_viz,
      p6c_category_map
    )
    
    pdf(out_file, width = 30, height = 15) # Good monitor size
    # pdf(out_file, width = 24, height = 17) # Printer aspect ratio
    print(cowplot::plot_grid(plotlist = both_rf_plots, nrow=2), rel_heights = c(0.60, 0.40))
    print(cowplot::plot_grid(plotlist = episodic_rf_plots, nrow=2), rel_heights = c(0.60, 0.40))
    print(cowplot::plot_grid(plotlist = baseflow_rf_plots, nrow=2), rel_heights = c(0.60, 0.40))
    dev.off()
    return(out_file)
  }, format = 'file')
  
)
