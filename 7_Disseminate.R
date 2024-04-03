# Targets for creating paper and slidedeck visualizations

source('7_Disseminate/src/roadSalt_figs.R')
source('7_Disseminate/src/partialDependence_figs.R')
source('7_Disseminate/src/importance_figs.R')
source('7_Disseminate/src/attribute_boxplot_figs.R')
source('7_Disseminate/src/category_map_figs.R')
source('7_Disseminate/src/baseflowTrend_figs.R')
source('7_Disseminate/src/episodic_detection_figs.R')

p7_targets <- list(
  
  # Create a manual crosswalk between attribute names and display names
  tar_target(p7_attr_name_xwalk, tibble(
    attribute = c("medianFlow", "roadSaltPerSqKm", "annualPrecip", "baseFlowInd", 
                  "subsurfaceContact", "gwRecharge", "pctOpenWater", "basinSlope", 
                  "pctForested", "pctWetland", "pctAgriculture", "pctDeveloped", 
                  "annualSnow", "winterAirTemp", "depthToWT", "transmissivity"),
    display_name = c("Median Flow (m3/s)", "Road Salt / SqKm (kg)", "Precip (mm/yr)", "Baseflow Index",
                     "Subsurface Contact (days)", "GW Recharge (mm/yr)", "Open Water (% area)", "Basin Slope (%)",
                     "Forested (% area)", "Wetland (% area)", "Agriculture (% area)", "Developed (% area)", 
                     "Snow (mm/yr)", "Winter Air Temp (°C)", "Depth to WT (m)", "Transmissivity (m2/day)"))),
  
  # Isolate overall importance and out-of-bag errors
  tar_target(p7_overall_attr_importance_episodic, p6b_rf_attr_importance %>% 
               filter(site_category == 'Overall mean') %>% 
               arrange(desc(importance))),
  tar_target(p7_oob_error_episodic, round(p6b_rf_oob*100, digits=1)),
  tar_target(p7_overall_attr_importance_baseflow, p6c_rf_attr_importance %>% 
               filter(site_category == 'Overall mean') %>% 
               arrange(desc(importance))),
  tar_target(p7_oob_error_baseflow, round(p6c_rf_oob*100, digits=1)),
  
  # Create a single dataset that collapses all categorization data of baseflow
  # and episodic into a single data.frame (so each site should appear twice)
  tar_target(p7_site_categories_episodic, p6b_site_attr %>% 
               mutate(model = 'episodic') %>% 
               select(site_no, model, site_category = site_category_fact)),
  tar_target(p7_site_categories_baseflow, p6c_site_attr %>% 
               mutate(model = 'baseflow') %>% 
               select(site_no, model, site_category = site_category_fact)),
  tar_target(p7_site_categories, bind_rows(p7_site_categories_episodic, p7_site_categories_baseflow)),
  
  ##### Road salt figures #####
  
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
  
  ##### Partial dependence plots #####
  
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
             format = 'file'),
  
  ##### Attribute importance figures #####
  
  tar_target(p7_importance_episodic_png, 
             visualize_attr_importance('7_Disseminate/out/importance_episodic.png', 
                                       p7_overall_attr_importance_episodic,
                                       p7_attr_name_xwalk, scico_palette = 'lapaz')),
  
  tar_target(p7_importance_baseflow_png, 
             visualize_attr_importance('7_Disseminate/out/importance_baseflow.png', 
                                       p7_overall_attr_importance_baseflow,
                                       p7_attr_name_xwalk, scico_palette = 'lajolla')),
  
  ##### Boxplots of attributes by model/category #####
  
  tar_target(p7_attr_episodic_boxplots_png, 
             create_attribute_boxplots('7_Disseminate/out/attributes_boxes_episodic.png',
                                       p6b_site_attr_rf_optimal,
                                       p7_overall_attr_importance_episodic$attribute,
                                       p7_attr_name_xwalk, 
                                       c(Episodic='#0b5394', `Not episodic` = 'grey40'))),
  tar_target(p7_attr_baseflow_boxplots_png, 
             create_attribute_boxplots('7_Disseminate/out/attributes_boxes_baseflow.png',
                                       p6c_site_attr_rf_optimal,
                                       p7_overall_attr_importance_baseflow$attribute,
                                       p7_attr_name_xwalk, 
                                       c(positive='#b45f06', none='grey40', negative='#663329'))),
  
  ##### Map of sites by category #####
  
  tar_target(p7_episodic_sitemap_png, 
             create_episodic_site_map('7_Disseminate/out/episodic_sitemap.png', p1_nwis_sc_sites_sf, 
                                      p7_site_categories, p1_conus_state_cds)),
  tar_target(p7_baseflow_sitemap_png, 
             create_baseflow_site_map('7_Disseminate/out/baseflow_sitemap.png', p1_nwis_sc_sites_sf, 
                                      p7_site_categories, p1_conus_state_cds)),
  
  tar_target(p7_overlap_sitemap_png, 
             create_overlap_site_map('7_Disseminate/out/overlap_sitemap.png', p1_nwis_sc_sites_sf, 
                                      p7_site_categories, p1_conus_state_cds)),
  
  ##### Save plots of baseflow trends for each site #####
  
  tar_target(p7_baseflow_trends_plotlist, create_baseflow_trend_plotlist(p5_sc_baseflow_qualified, p5_sc_baseflow_trend)),
  tar_target(p7_baseflow_trends_png, 
             ggsave(filename = sprintf('7_Disseminate/out/baseflow_trends_grp%s.png', names(p7_baseflow_trends_plotlist)), 
                    plot = p7_baseflow_trends_plotlist[[1]], height = 8, width = 10, dpi = 500), 
             format = 'file', pattern = map(p7_baseflow_trends_plotlist)),
  
  ##### Save plots of episodic behavior for each site #####
  
  tar_target(p7_episodic_plotlist, create_episodic_plotlist(p3_ts_sc_qualified, p4_episodic_sites)),
  tar_target(p7_episodic_png, 
             ggsave(filename = sprintf('7_Disseminate/out/episodic_grp%s.png', names(p7_episodic_plotlist)), 
                    plot = p7_episodic_plotlist[[1]], height = 8, width = 10, dpi = 500), 
             format = 'file', pattern = map(p7_episodic_plotlist))
  
)
