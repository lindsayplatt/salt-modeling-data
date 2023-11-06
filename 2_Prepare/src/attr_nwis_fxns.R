
#' @title Calculate static mean Q for each site
#' @description Using the daily timeseries data for streamflow, calculate a single
#' mean value per site to serve as a static attribute.
#' 
#' @param in_file a character string indicating the file path containing all
#' daily Q records with at least the columns `site_no`, `dateTime`, and `Flow`.
#' 
#' @return a feather file containing only one value for each site; it should have 
#' the columns `site_no` and `attr_meanFlow`.
#'
calculate_mean_q_per_site <- function(in_file) {
  read_feather(in_file) %>% 
    group_by(site_no) %>% 
    summarize(attr_meanFlow = mean(Flow, na.rm = TRUE))
}
