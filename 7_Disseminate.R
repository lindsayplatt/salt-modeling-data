# Targets for creating paper and slidedeck visualizations

source('7_Disseminate/src/attribute_correlations.R')
source('7_Disseminate/src/roadSalt_figs.R')
source('7_Disseminate/src/partialDependence_figs.R')
source('7_Disseminate/src/importance_figs.R')
source('7_Disseminate/src/attribute_boxplot_figs.R')
source('7_Disseminate/src/category_map_figs.R')
source('7_Disseminate/src/baseflowTrend_figs.R')
source('7_Disseminate/src/episodic_detection_figs.R')

p7_targets <- list(
  
  # Create a manual crosswalk between attribute names and display names
  tar_target(p7_attribute_details, tibble(
    attribute = c("medianFlow", "basinSlope", "pctAgriculture", "pctDeveloped",
                  "pctForested", "pctOpenWater", "pctWetland", "annualPrecip",
                  "annualSnow", "winterAirTemp", "baseFlowInd", "gwRecharge",
                  "subsurfaceContact", "depthToWT", "transmissivity", "roadSaltPerSqKm"),
    display_name = c("Median Flow (m3/s)", "Basin Slope (%)", "Agriculture (% area)", "Developed (% area)",
                     "Forested (% area)", "Open Water (% area)", "Wetland (% area)", "Precip (mm/year)",
                     "Snow (mm/year)", "Winter Air Temp (°C)", "Baseflow Index",
                     "GW Recharge (mm/year)", "Subsurface Contact (days)", "Depth to WT (m)",
                     "Transmissivity (m2/day)", "Road Salt (kg/km^2)"),
    definition = c(
      "Median streamflow derived from daily streamflow time series",
      "Average topographic slope to indicate basin steepness; uses the NHDPlus attribute CAT_BASIN_SLOPE",
      "Percent of basin area that is agriculture; sum of the NHDPlus attribute categories CAT_NLCD19_81 (pasture/hay) and CAT_NLCD19_82 (cropland)",
      "Percent of basin area that is developed; sum of the NHDPlus attribute categories CAT_NLCD19_22 (open development, < 20% impervious), CAT_NLCD19_23 (low development, 20-49% impervious), CAT_NLCD19_24 (medium development, 50-79% impervious), and CAT_NLCD19_25 (high development, 80-100% impervious)",
      "Percent of basin area that is forested; sum of the NHDPlus attribute categories CAT_NLCD19_41 (deciduous), CAT_NLCD19_42 (evergreen), and CAT_NLCD19_43 (mixed)",
      "Percent of basin area that is open water (lakes, rivers, etc); uses the NHDPlus category CAT_NLCD19_11",
      "Percent of basin area that is wetland; sum of the NHDPlus attribute categories CAT_NLCD19_90 (woody wetlands) and CAT_NLCD19_95 (herbaceous wetlands)",
      "Total annual precipitation; uses the NHDPlus attribute CAT_PPT7100_ANN",
      "Total annual snowfall; calculated by multiplying total annual precipitation (CAT_PPT7100_ANN) by the NHDPlus attribute CAT_PRSNOW (percent of precipitation that is snow)",
      "Average winter air temperature; calculated by averaging the NHDPlus attributes CAT_TAV7100_DEC, CAT_TAV7100_JAN, CAT_TAV7100_FEB, and CAT_TAV7100_MAR",
      "Average percent of the total streamflow that is baseflow; uses the NHDPlus attribute CAT_BFI",
      "Amount of water added to the water table per year; uses the NHDPlus attribute CAT_RECHG",
      "Number of days that water resides in the saturated subsurface before entering a stream; uses the NHDPlus attribute CAT_CONTACT",
      "Modeled parameter estimating the depth below the surface where the subsurface is saturated",
      "Modeled parameter estimating how easily water is transmitted through shallow subsurface",
      "Estimated weight of road deicing salt applied by area"
    ),
    source = c(
      "USGS NWIS",
      rep("NHDPlus", 12),
      rep("Zell and Sanford, 2020", 2),
      "Falcone et al., 2023"
    ))),
  tar_target(p7_attr_name_xwalk, select(p7_attribute_details, attribute, display_name)),
  
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
  
  ##### Attribute correlation figure #####
  
  # Saving attribute correlations as a file
  tar_target(p7_attribute_correlations_png, 
             visualize_attr_correlation('7_Disseminate/out/3_attribute_correlations.png',
                                        p6a_site_attr_rf,
                                        p7_attr_name_xwalk), 
             format='file'),
  
  ##### Additional attribute explanations #####
  
  # Figure showing baseflow days are higher for smaller rivers
  tar_target(p7_bfdays_vs_flow_png, 
             create_bf_vs_q_scatterplot('7_Disseminate/out/5_baseflowdays_vs_medianflow.png', 
                                        p3_ts_sc_qualified, p5_sc_baseflow_qualified,
                                        p3_static_attributes),
             format = 'file'),
  
  # Relationship between developed and forested percents
  tar_target(p7_dev_vs_forest_png, 
             create_dev_vs_forest_scatters('7_Disseminate/out/4_developed_vs_pctforested.png', p6c_site_attr),
             format = 'file'),
  
  # Figure showing the feature space for the episodic classification criteria
  tar_target(p7_episodic_feature_space_png,
             generate_episodic_feature_space_figure('7_Disseminate/out/2.3_episodic_feature_space.png', p4_ts_sc_peak_summary),
             format = 'file'),
  
  ##### Road salt figures #####
  
  # Boxplot of roadsalt showing that sites across all the categories had similar amounts
  tar_target(p7_roadsalt_boxes_png, 
             create_roadSalt_boxplot('7_Disseminate/out/4_roadSalt_boxes.png', p3_static_attributes, p7_site_categories),
             format = 'file'),
  
  # Map of roadsalt per site showing that sites don't completely follow a gradient from south --> north
  tar_target(p7_roadsalt_sitemap_png, 
             create_roadSalt_site_map('7_Disseminate/out/roadSalt_sitemap.png', p3_static_attributes, p1_nwis_sc_sites_sf, p1_conus_state_cds),
             format = 'file'),
  
  # Map of gridded roadsalt 
  tar_target(p7_roadsalt_gridmap_png, 
             create_roadSalt_map('7_Disseminate/out/2.2_roadSalt_gridmap.png', p1_sb_road_salt_2015_tif, p1_conus_state_cds),
             format = 'file'),
  
  # Figure showing road salt vs pct forested
  tar_target(p7_roadsalt_forest_png, 
             create_roadSalt_vs_forested('7_Disseminate/out/4_roadsalt_vs_pctforested.png', p3_static_attributes),
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
                                       p7_attr_name_xwalk, geom_color = 'grey15'), 
             format='file'),
  
  tar_target(p7_importance_baseflow_png, 
             visualize_attr_importance('7_Disseminate/out/importance_baseflow.png', 
                                       p7_overall_attr_importance_baseflow,
                                       p7_attr_name_xwalk, geom_color = 'grey15'), 
             format='file'),
  
  ##### Boxplots of attributes by model/category #####
  
  tar_target(p7_attr_all_boxplots_png, 
             create_attribute_boxplots('7_Disseminate/out/3_attributes_boxes_all.png',
                                       mutate(p6a_site_attr_rf, site_category_fact = 'ALL'),
                                       # Same order as the table
                                       c('medianFlow', 'basinSlope', 'pctAgriculture',
                                         'pctDeveloped', 'pctForested', 'pctOpenWater',
                                         'pctWetland', 'annualPrecip', 'annualSnow',
                                         'winterAirTemp', 'baseFlowInd', 'gwRecharge',
                                         'subsurfaceContact', 'depthToWT', 
                                         'transmissivity', 'roadSaltPerSqKm'),
                                       p7_attr_name_xwalk, 
                                       c(ALL='#868b8e'),
                                       legend_position = "none",
                                       attribute_text_size = 9), 
             format='file'),
  
  tar_target(p7_attr_episodic_boxplots_png, 
             create_attribute_boxplots('7_Disseminate/out/attributes_boxes_episodic.png',
                                       p6b_site_attr_rf_optimal,
                                       p7_overall_attr_importance_episodic$attribute,
                                       p7_attr_name_xwalk, 
                                       c(Episodic='#0b5394', `Not episodic` = 'grey40')), 
             format='file'),
  tar_target(p7_attr_baseflow_boxplots_png, 
             create_attribute_boxplots('7_Disseminate/out/attributes_boxes_baseflow.png',
                                       p6c_site_attr_rf_optimal,
                                       p7_overall_attr_importance_baseflow$attribute,
                                       p7_attr_name_xwalk, 
                                       c(positive='#b45f06', none='grey40', negative='#663329')), 
             format='file'),
  
  tar_target(p7_attr_overlap_boxplots_png, {
    
    # Identify which sites are episodic only, positive baseflow only, or both
    category_info <- p7_site_categories %>%
      pivot_wider(names_from = 'model', values_from = 'site_category')
    sites_both <- category_info %>%
      filter(episodic == 'Episodic', baseflow == 'positive') %>%
      pull(site_no)
    sites_only_episodic <- category_info %>%
      filter(episodic == 'Episodic', baseflow != 'positive') %>%
      pull(site_no)
    sites_only_posbaseflow <- category_info %>%
      filter(episodic != 'Episodic', baseflow == 'positive') %>%
      pull(site_no)
    
    # Prep some data for visualizing episodic only, positive baseflow only, and 
    # sites that are both as separate categories
    site_attr_data_both <-  p3_static_attributes %>% 
      mutate(site_category = case_when(
        site_no %in% sites_both ~ "Both signatures",
        site_no %in% sites_only_episodic ~ "Only episodic",
        site_no %in% sites_only_posbaseflow ~ "Only positive baseflow",
        TRUE ~ 'NONE')) %>% 
      filter(site_category != 'NONE') %>% 
      mutate(site_category_fact = factor(site_category, 
                                         levels = c('Only episodic', 'Both signatures', 
                                                    'Only positive baseflow'))) %>% 
      # Drop the `attr_` prefix so that the results are cleaner
      rename_with(~gsub("attr_", "", .x)) %>% 
      # Only use the 7 attributes discussed across the two models
      select(site_category_fact, depthToWT, medianFlow, pctDeveloped, winterAirTemp, 
             annualSnow, annualPrecip, pctForested)
    
    # In order of their Gini index of importance from the two models.
    importance_order <- c('depthToWT', 'medianFlow', 'pctDeveloped', 'winterAirTemp', 
                          'annualSnow', 'annualPrecip', 'pctForested')
    
    # Now make a boxplots of the attribute distributions
    create_attribute_boxplots('7_Disseminate/out/3.3_attribute_boxplots_overlap.png',
                              site_attr_data_both,
                              importance_order,
                              p7_attr_name_xwalk, 
                              c(`Both signatures` = '#5e5a4d', 
                                `Only episodic` = '#0b5394', 
                                `Only positive baseflow` = '#b45f06'))
  }, format='file'),
  
  ##### Combine random forest output into a single figure #####
  
  # Note that `cowplot::draw_image()` requires that you have `magick`
  tar_target(p7_rf_results_episodic_png, {
    out_file <- '7_Disseminate/out/3.1_episodic_rf_results.png'
    fig_importance <- cowplot::ggdraw() + cowplot::draw_image(p7_importance_episodic_png)
    fig_partDep <- cowplot::ggdraw() + cowplot::draw_image(p7_partDep_episodic_png)
    fig_boxes <- cowplot::ggdraw() + cowplot::draw_image(p7_attr_episodic_boxplots_png)
    png(out_file, width = 6.5, height = 3.25, units = 'in', res = 500)
    print(cowplot::plot_grid(fig_importance, fig_partDep, fig_boxes, 
                             nrow=1, label_size=10,
                             labels=sprintf('(%s)', letters[1:3])))
    dev.off()
    return(out_file)
  }, format='file'),
  
  tar_target(p7_rf_results_baseflow_png, {
    out_file <- '7_Disseminate/out/3.2_baseflow_rf_results.png'
    fig_importance <- cowplot::ggdraw() + cowplot::draw_image(p7_importance_baseflow_png)
    fig_partDep <- cowplot::ggdraw() + cowplot::draw_image(p7_partDep_baseflow_png)
    fig_boxes <- cowplot::ggdraw() + cowplot::draw_image(p7_attr_baseflow_boxplots_png)
    png(out_file, width = 6.5, height = 3.25, units = 'in', res = 500)
    print(cowplot::plot_grid(fig_importance, fig_partDep, fig_boxes, 
                             nrow=1, label_size=10,
                             labels=sprintf('(%s)', letters[1:3])))
    dev.off()
    return(out_file)
  }, format='file'),
  
  ##### Map of sites by category #####
  
  tar_target(p7_all_sitemap_png, {
    out_file <- '7_Disseminate/out/2.2_sitemap_all_qualified.png'
    p_map <- map_category_sites(p1_nwis_sc_sites_sf, p3_static_attributes$site_no, p1_conus_state_cds, 
                                site_color = 'grey30', map_title = 'Qualified sites')
    ggsave(out_file, p_map, width = 3.25, height = 3.25, dpi = 500, bg='white')
    return(out_file)
  }, format='file'),
  
  tar_target(p7_episodic_sitemap_png, 
             create_episodic_site_map('7_Disseminate/out/3.1_episodic_sitemap.png', p1_nwis_sc_sites_sf, 
                                      p7_site_categories, p1_conus_state_cds), 
             format='file'),
  tar_target(p7_baseflow_sitemap_png, 
             create_baseflow_site_map('7_Disseminate/out/3.2_baseflow_sitemap.png', p1_nwis_sc_sites_sf, 
                                      p7_site_categories, p1_conus_state_cds), 
             format='file'),
  
  tar_target(p7_overlap_sitemap_png, 
             create_overlap_site_map('7_Disseminate/out/3.3_sitemap_overlap.png', p1_nwis_sc_sites_sf, 
                                      p7_site_categories, p1_conus_state_cds), 
             format='file'),
  
  ##### Save plots of baseflow trends for each site #####
  
  tar_target(p7_baseflow_trends_plotlist, create_baseflow_trend_plotlist(p5_sc_baseflow_qualified, p5_sc_baseflow_trend)),
  tar_target(p7_baseflow_trends_png, 
             ggsave(filename = sprintf('7_Disseminate/out/SI_baseflow_trends_grp%s.png', names(p7_baseflow_trends_plotlist)), 
                    plot = p7_baseflow_trends_plotlist[[1]], height = 8, width = 10, dpi = 500), 
             format = 'file', pattern = map(p7_baseflow_trends_plotlist)),
  
  tar_target(p7_baseflow_trends_examples_plot, {
    example_posbaseflow_sites <- c('040871488', '01645762') # Milwaukee, DC (Vienna)
    example_nobaseflow_sites <- c('03067510', '04166500') # West Virginia, Detroit
    example_negbaseflow_sites <- c('01104415', '03072655') # Massachusetts, SW Pennsylvania
    p5_sc_baseflow_qualified %>% 
      filter(site_no %in% c(example_posbaseflow_sites, example_nobaseflow_sites,
                            example_negbaseflow_sites)) %>%
      create_baseflow_trend_plotlist(p5_sc_baseflow_trend, nrow=2, dir="v") 
  }),
  tar_target(p7_baseflow_trends_examples_png, {
    out_file <- '7_Disseminate/out/3.2_baseflow_ts.png'
    png(out_file, width = 13, height = 6.5, units='in', res=500)
    print(p7_baseflow_trends_examples_plot)
    dev.off()
    return(out_file)
  }, format='file'),
  
  ##### Save plots of episodic behavior for each site #####
  
  tar_target(p7_episodic_plotlist, create_episodic_plotlist(p3_ts_sc_qualified, p4_episodic_sites)),
  tar_target(p7_episodic_png, 
             ggsave(filename = sprintf('7_Disseminate/out/SI_episodic_grp%s.png', names(p7_episodic_plotlist)), 
                    plot = p7_episodic_plotlist[[1]], height = 8, width = 10, dpi = 500), 
             format = 'file', pattern = map(p7_episodic_plotlist)),
  
  tar_target(p7_episodic_examples_plot, {
    example_episodic_sites <- c('02042500', '01481500', '04166500') # Richmond, Wilmington, Detroit
    example_not_episodic_sites <- c('01481000', '03183500', '04176500') # NW of Wilmington, SE of Philly, West Virginia, S of Detroit
    p3_ts_sc_qualified %>% 
      filter(site_no %in% c(example_episodic_sites, example_not_episodic_sites)) %>%
      create_episodic_plotlist(example_episodic_sites, nrow=2) 
  }),
  tar_target(p7_episodic_examples_png, {
    out_file <- '7_Disseminate/out/3.1_episodic_ts.png'
    png(out_file, width = 13, height = 6.5, units='in', res=500)
    print(p7_episodic_examples_plot)
    dev.off()
    return(out_file)
  }, format='file'),
  
  ##### Save and export output data for publication #####
  
  # Export final sites and their attributes
  tar_target(p7_site_attributes_csv, {
    out_file <- '7_Disseminate/out/site_attributes.csv'
    p6a_site_attr %>% 
      select(-site_category_fact) %>% 
      write_csv(file = out_file)
    return(out_file)
  }, format = 'file'),
  
  # Export full names and units for each of the attributes
  tar_target(p7_attr_name_details_csv, {
    out_file <- '7_Disseminate/out/site_attribute_details.csv'
    p7_attribute_details %>% 
      rename(name_units = display_name) %>% 
      write_csv(file = out_file, quote='all')
    return(out_file)
  }, format = 'file'),
  
  # Export final classification results
  tar_target(p7_site_classifications_csv, {
    out_file <- '7_Disseminate/out/site_classifications.csv'
    episodic_site_classes <- p7_site_categories_episodic %>% 
      rename(Classification_Episodic = site_category) %>% select(-model)
    baseflow_site_classes <- p7_site_categories_baseflow %>% 
      rename(Classification_Baseflow = site_category) %>% select(-model)
    episodic_site_classes %>% 
      left_join(baseflow_site_classes, by = 'site_no') %>% 
      write_csv(file = out_file)
    return(out_file)
  }, format = 'file')
)
