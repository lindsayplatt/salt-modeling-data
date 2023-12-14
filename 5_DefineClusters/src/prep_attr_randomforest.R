
# TODO: DOCUMENTATION
prep_attr_randomforest <- function(site_clusters, site_attr_data) {
  
  # Combine clusters & attributes
  site_clusters %>% 
    # Make the cluster a factor so that random forest
    # is doing classification, not regression
    mutate(clusterf = factor(cluster)) %>% 
    # Add in the attribute data per site
    left_join(site_attr_data, by = "site_no") %>% 
    # Note that `attr_streamDensity` is missing for 3 sites
    # and either those sites will need to be removed OR 
    # the streamDensity attribute cannot be used. 
    # TODO: Removing that attribute for now.
    select(-attr_streamDensity) %>% 
    # Remove the columns that are not needed for the RF
    select(clusterf, starts_with('attr')) %>% 
    # Drop the `attr_` prefix so that the results are cleaner
    rename_with(~gsub("attr_", "", .x))
  
}
