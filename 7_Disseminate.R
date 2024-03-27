# Targets for creating paper and slidedeck visualizations

source('7_Disseminate/src/roadSalt_figs.R')
source('7_Disseminate/src/partialDependence_figs.R')

p7_targets <- list(
  
  # Create a manual crosswalk between attribute names and display names
  tar_target(p7_attr_name_xwalk, tibble(
    attribute = c("medianFlow", "roadSaltPerSqKm", "annualPrecip", "baseFlowInd", 
                  "subsurfaceContact", "gwRecharge", "pctOpenWater", "basinSlope", 
                  "pctForested", "pctWetland", "pctAgriculture", "pctDeveloped", 
                  "annualSnow", "winterAirTemp", "depthToWT", "transmissivity"),
    display_name = c("Median Flow (m3/s)", "Road Salt / SqKm (kg)", "Precip (mm/yr)", "Baseflow Index",
                     "Subsurface Contact (days)", "GW Recharge (mm/yr)", "Open Water (%)", "Basin Slope (%)",
                     "Forested (% area)", "Wetland (% area)", "Agriculture (% area)", "Developed (% area)", 
                     "Snow (mm/yr)", "Winter Air Temp (°C)", "Depth to WT (m)", "Transmissivity (m2/day)"))),
  
  # Isolate overall importance and out-of-bag errors
  tar_target(p7_overall_attr_importance_episodic, p6b_rf_attr_importance %>% 
               filter(site_category == 'Overall mean') %>% 
               arrange(desc(importance))),
  tar_target(p7_oob_error_episodic, p6b_rf_oob),
  tar_target(p7_overall_attr_importance_baseflow, p6c_rf_attr_importance %>% 
               filter(site_category == 'Overall mean') %>% 
               arrange(desc(importance))),
  tar_target(p7_oob_error_baseflow, p6c_rf_oob),
  
  # Create a single dataset that collapses all categorization data of baseflow
  # and episodic into a single data.frame (so each site should appear twice)
  tar_target(p7_site_categories_episodic, p6b_site_attr %>% 
               mutate(model = 'episodic') %>% 
               select(site_no, model, site_category = site_category_fact)),
  tar_target(p7_site_categories_baseflow, p6c_site_attr %>% 
               mutate(model = 'baseflow') %>% 
               select(site_no, model, site_category = site_category_fact)),
  tar_target(p7_site_categories, bind_rows(p7_site_categories_episodic, p7_site_categories_baseflow)),
  
  # Boxplot of roadsalt showing that sites across all the categories had similar amounts
  tar_target(p7_roadsalt_boxes_png, 
             create_roadSalt_boxplot('7_Disseminate/out/roadSalt_boxes.png', p3_static_attributes, p7_site_categories),
             format = 'file'),
  
  # Map of roadsalt per site showing that sites don't completely follow a gradient from south --> north
  tar_target(p7_roadsalt_sitemap_png, 
             create_roadSalt_site_map('7_Disseminate/out/roadSalt_sitemap.png', p3_static_attributes, p1_nwis_sc_sites_sf, p1_conus_state_cds),
             format = 'file'),
  
  # Map of gridded roadsalt 
  tar_target(p7_roadsalt_gridmap_png, 
             create_roadSalt_map('7_Disseminate/out/roadSalt_gridmap.png', p1_sb_road_salt_2015_tif, p1_conus_state_cds),
             format = 'file'),
  
  # Partial dependence plots showing how probability varies by attribute value
  tar_target(p7_partDep_episodic_png, 
             create_partialDependence_miniPlots_figure('7_Disseminate/out/partDep_episodic.png', 
                                                       p6b_rf_attr_partdep, p6b_site_attr_rf_optimal,
                                                       p7_overall_attr_importance_episodic$attribute,
                                                       p7_attr_name_xwalk, '#0b5394'),
             format = 'file'),
  tar_target(p7_partDep_baseflow_png, 
             create_partialDependence_miniPlots_figure('7_Disseminate/out/partDep_baseflow.png', 
                                                       p6c_rf_attr_partdep, p6c_site_attr_rf_optimal,
                                                       p7_overall_attr_importance_baseflow$attribute,
                                                       p7_attr_name_xwalk, '#b45f06'),
             format = 'file')
  
)
