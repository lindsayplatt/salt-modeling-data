
# TODO: DOCUMENTATION
apply_randomforest <- function(site_attr_data, mtry = NULL) {
  
  # Use the default for mtry in `randomForest()` if passed in as NULL here
  if(is.null(mtry)) mtry <- floor(sqrt(ncol(site_attr_data) - 1))
  
  randomForest(
    site_category_fact ~ .,
    data = site_attr_data,
    mtry = mtry, 
    importance = TRUE)
  
}

# TODO: DOCUMENTATION
optimize_mtry <- function(site_attr_data) {
  # Need to select the value of mtry that reduces OOB
  # mtry = number of variables for each tree
  # OOB = out of bag error
  mtry <- tuneRF(
    select(site_attr_data, -site_category_fact),
    site_attr_data$site_category_fact, 
    ntreeTry=500,
    stepFactor=1.5,
    improve=0.01, 
    trace=TRUE, 
    plot=FALSE)
  best.mtry <- mtry[which.min(mtry[,'OOBError']), 'mtry']
  return(best.mtry)
}

# TODO: documentation
optimize_attrs <- function(site_attr_data, rf_model, n_important = 10) {
  
  # Prepare importance values in long table format
  rf_importance <- rf_model$importance %>% 
    as_tibble(rownames = 'attribute') %>% 
    dplyr::select(-MeanDecreaseGini, -MeanDecreaseAccuracy) %>% 
    pivot_longer(-attribute, 
                 names_to = 'site_category',
                 values_to = 'importance') 
  
  # Identify top 10 most important attributes for each site category.
  top10_per_category <- rf_importance %>% 
    split(.$site_category) %>% 
    map(~{
      .x %>% arrange(desc(importance)) %>% head(n_important)
    }) %>%
    bind_rows()
  
  # Find the unique attributes among all category top 10s.
  most_important_attrs <- unique(top10_per_category$attribute)
  
  # Trim the attribute data down to only that set of unique attributes
  site_attr_data_trim <- site_attr_data %>% 
    dplyr::select(site_category_fact, all_of(most_important_attrs))
  
}
