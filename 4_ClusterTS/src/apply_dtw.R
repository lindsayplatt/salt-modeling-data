
#' @title Apply the dynamic time-warping algorithm to generate clusters
#' @description Apply the `dtwclust::tsclust()` time series clustering algorithm
#' to generate clusters using dynamic time-warping.
#' 
#' @param ts_list a tibble with at least the columns `site_no`, `year` and `[PARAM]_norm`
#' @param n_clusters a single numeric value indicating the number of clusters 
#' to use when grouping time series
#' @param n_day_window a single numeric value giving the number of days TODO ?????
#' @param distance_method see `?dtwclust::tsclust()`, defaults to "dtw"
#' @param centroid_method see `?dtwclust::tsclust()`, defaults to "pam"
#' 
#' @return TODO: ???
#' 
apply_dynamic_timewarping <- function(out_file, ts_list, n_clusters, n_day_window = 20,
                                      distance_method = "dtw", centroid_method = "pam") {
  message(sprintf('Starting DTW for %s clusters and window size of %s days', 
                  n_clusters, n_day_window))
  dtw_out <- tsclust(ts_list, 
                     type = "partitional", 
                     k = n_clusters, 
                     distance = distance_method, 
                     centroid = centroid_method, # TODO: or `sdtw_cent` or others (see `?tsclust`)
                     tsclust_args(dist = list(window.size = n_day_window)))
  qsave(dtw_out, out_file)
  return(out_file)
}
