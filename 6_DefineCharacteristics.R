# Targets for applying a random forest to link attributes
# to each site that is experiencing "episodic salinization"
# or "baseflow salinization" or both.

source('6_DefineCharacteristics/src/prep_attr_randomforest.R')
source('6_DefineCharacteristics/src/apply_randomforest.R')
source('6_DefineCharacteristics/src/evaluate_randomforest.R')
source('6_DefineCharacteristics/src/visualize_attribute_distributions.R')

p6_targets <- list(
  
  ##### Prep data for RF #####
  
  tar_target(p6_site_attr_rf, prep_attr_randomforest(p3_static_attributes, p4b_episodic_sites, p5b_baseflow_sites)),
  
  ##### Determine optimal RF configs #####
  
  tar_target(p6_rf_model_initial, apply_randomforest(p6_site_attr_rf)),
  
  # Find and use optimal `mtry` to minimize OOB error
  tar_target(p6_mtry_optimal_all_attrs, optimize_mtry(p6_site_attr_rf)),
  tar_target(p6_rf_model_minOOB, apply_randomforest(p6_site_attr_rf, p6_mtry_optimal_all_attrs)),
  
  # Trim the number of attributes used by only choosing those that appeared as
  # one of the top 10 most important in one of the categories.
  tar_target(p6_site_attr_rf_optimal, optimize_attrs(p6_site_attr_rf, p6_rf_model_minOOB)),
  
  # Now with the new site attributes re-calculate the optimal `mtry`
  tar_target(p6_mtry_optimal, optimize_mtry(p6_site_attr_rf_optimal)),
  
  ##### Run optimal RF #####
  
  tar_target(p6_rf_model_optimal, apply_randomforest(p6_site_attr_rf_optimal, p6_mtry_optimal)),
  
  ##### Evaluate RF output #####
  
  tar_target(p6_rf_attr_importance, calculate_attr_importance(p6_rf_model_optimal)),
  tar_target(p6_rf_attr_importance_viz, visualize_attr_importance(p6_rf_attr_importance)),
  tar_target(p6_rf_attr_importance_simple_viz, visualize_attr_importance(p6_rf_attr_importance,
                                                                         simple = TRUE)),
  tar_target(p6_rf_attr_partdep, calculate_partial_dependence(p6_rf_model_optimal, 
                                                              p6_site_attr_rf)),
  tar_target(p6_rf_attr_partdep_viz, visualize_partial_dependence(p6_rf_attr_partdep)),
  
  ##### Visualize site category attribute distributions #####
  
  tar_target(p6_attrs_num_viz, visualize_numeric_attrs(p6_site_attr_rf_optimal))
  
)
