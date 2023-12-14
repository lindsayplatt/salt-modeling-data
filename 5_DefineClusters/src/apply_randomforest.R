
# TODO: DOCUMENTATION
apply_randomforest <- function(site_attr_data, mtry = NULL) {
  
  # Use the default for mtry in `randomForest()` if passed in as NULL here
  if(is.null(mtry)) mtry <- floor(sqrt(ncol(site_attr_data) - 1))
  
  randomForest(
    clusterf ~ .,
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
    select(site_attr_data, -clusterf),
    site_attr_data$clusterf, 
    ntreeTry=500,
    stepFactor=1.5,
    improve=0.01, 
    trace=TRUE, 
    plot=FALSE)
  best.mtry <- mtry[which.min(mtry[,'OOBError']), 'mtry']
  return(best.mtry)
}
