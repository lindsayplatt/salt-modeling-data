
# TODO: documentation
prep_attr_randomforest <- function(site_attr_data, sites_episodic, sites_baseflow) {
  
  site_attr_data %>% 
    mutate(site_category = case_when(
      site_no %in% sites_episodic & site_no %in% sites_baseflow ~ 'Both',
      site_no %in% sites_episodic & !site_no %in% sites_baseflow ~ 'Episodic',
      !site_no %in% sites_episodic & site_no %in% sites_baseflow ~ 'Baseflow',
      TRUE ~ 'Neither'
    )) %>% 
    # Make the site category a factor so that random forest
    # is doing classification, not regression
    mutate(site_category_fact = factor(site_category)) %>% 
    
    # TODO: Figure out better way to handle missing data below.
    
    # Note that `attr_streamDensity` is missing for 3 sites
    # and either those sites will need to be removed OR 
    # the streamDensity attribute cannot be used. 
    # TODO: Removing that attribute for now.
    select(-attr_streamDensity) %>% 
    # One site does not have a match in NHD+, so is missing all attributes - removing
    filter(site_no != '295501090190400') %>% 
    # One site is missing road salt (did not have a catchment in NHD+) - removing
    filter(site_no != '01542500') %>% 
    
    # Remove the columns that are not needed for the RF
    select(site_category_fact, starts_with('attr')) %>% 
    # Don't need trend attributes
    select(-starts_with("attr_Trend")) %>% 
    # Drop the `attr_` prefix so that the results are cleaner
    rename_with(~gsub("attr_", "", .x))
  
}
