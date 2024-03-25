# Targets for creating paper and slidedeck visualizations

source('7_Disseminate/src/roadSalt_figs.R')

p7_targets <- list(
  
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
             format = 'file')
  
)
