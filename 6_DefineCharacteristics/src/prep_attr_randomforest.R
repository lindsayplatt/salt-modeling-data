
# TODO: documentation
prep_attr_randomforest <- function(site_attr_data, sites_episodic, site_baseflow_trend_info) {
  
  # Filter to just those sites with positive baseflow trends
  sites_baseflow <- site_baseflow_trend_info %>% filter(baseflowTrend == 'positive') %>% pull(site_no)
  
  # TODO: expand categories later by considering a combination of episodic + trend type (rather than just positive)
  #   Both episodic and positive baseflow trend
  #   Episodic with negative baseflow trend
  #   Episodic without a baseflow trend
  #   Not episodic but with positive baseflow trend
  #   Not episodic and with negative baseflow trend
  #   Neither episodic nor baseflow trend
  
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
    # TODO: Removing upstream salt for now because too many sites are missing it
    select(-attr_roadSaltCumulative) %>% 
    # One site does not have a match in NHD+, so is missing all attributes - removing
    filter(site_no != '295501090190400') %>% 
    # One site is missing road salt (did not have a catchment in NHD+) - removing
    filter(site_no != '01542500') %>% 
    
    # Remove the columns that are not needed for the RF
    select(site_category_fact, starts_with('attr')) %>% 
    # Drop the `attr_` prefix so that the results are cleaner
    rename_with(~gsub("attr_", "", .x))
  
}
