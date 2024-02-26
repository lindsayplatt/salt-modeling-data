
#' @title Run a random forest model
#' @description Using the `randomForest::randomForest()` function, run a 
#' random forest model for the given set of site attributes and site categories.
#' 
#' @param site_attr_data a tibble with the columns `site_category_fact` and any
#' number of columns that give static attributes (not prefixed with `attr_`)
#' @param mtry the number of variables randomly sampled as candidates at each 
#' split; by default it will use the square root of the number of attributes.
#' 
#' @returns an list object of class randomForest, see `?randomForest::randomForest`
#' 
apply_randomforest <- function(site_attr_data, mtry = NULL) {
  
  # Use the default for mtry in `randomForest()` if passed in as NULL here
  if(is.null(mtry)) mtry <- floor(sqrt(ncol(site_attr_data) - 1))
  
  randomForest(
    site_category_fact ~ .,
    data = site_attr_data,
    mtry = mtry, 
    importance = TRUE)
  
}

#' @title Attempt to optimize the `mtry` random forest parameter
#' @description This function uses `randomForest::tuneRF()` to attempt to identify
#' the best value for `mtry` (the number of variables randomly sampled as candidates
#' at each split). It will select the value of `mtry` that minimizes the out-of-bag
#' error.
#' 
#' @param site_attr_data a tibble with the columns `site_category_fact` and any
#' number of columns that give static attributes (not prefixed with `attr_`)
#' 
#' @returns a single numeric vector giving the best `mtry` value to use in a future
#' random forest model run
#' 
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

#' @title Identify the most important attributes to use in a future random forest model
#' @description This function identifies attributes that are highest among the 
#' importance metrics ranking from a previous random forest model run in order to
#' better optimize the next random forest model run with fewer, more targeted attributes.
#' 
#' @param site_attr_data a tibble with the columns `site_category_fact` and any
#' number of columns that give static attributes (not prefixed with `attr_`)
#' @param rf_model a model object of a random forest that was already run; likely
#' returned from `apply_randomforest()`
#' @param n_important the maximum number of attributes to keep and be used in the 
#' next, more optimized random forest run. Uses the "importance" metric to rank
#' the attributes and only keeps the highest. Defaults to top 10 most important.
#' 
#' @returns a tibble with the columns `site_category_fact` and columns that 
#' correspond to only the top `n_important` attributes identified
#' 
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
  
  return(site_attr_data_trim)
}
