
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

# TODO: DOCUMENT FUNCTION
evaluate_dtw_clusters <- function(in_files) {
  clust.out <- map(in_files, qread)
  
  cluster_windows <- str_split(in_files, 'cluster_|_window|.qs') 
  clusters <- map(cluster_windows, ~pluck(.x, 2)) %>% unlist() 
  windows <- map(cluster_windows, ~pluck(.x, 3)) %>% unlist() %>% gsub('_', '', .)
  dtw_names <- sprintf('%s_%s', clusters, windows)
  names(clust.out) <- dtw_names
  
  # Attempt at sum of square residuals within each cluster
  # to evaluate with the elbow method. Instead of residuals,
  # I am summing the cluster differences (and not squaring 
  # since the "distance" measure might not be a vertical line)
  # sum_of_cldist <- map(clust.out, ~sum(.x@cldist[,1]))
  cvi_plus_elbow <- function(x) {
    c(cvi(x), ELBOW = sum(x@cldist[,1]))
  }
  
  # Now evaluate to find which number of clusters works best
  clust.out %>% 
    purrr::map(cvi_plus_elbow) %>% 
    bind_rows(.id = 'clust_window') %>% 
    rename(
      # Consulted section 5.1 of https://rpubs.com/esobolewska/dtw-time-series
      # and the docs for `?cvi` to get these definitions and interpretations
      # Docs list the origin publications for each of these indices.
      `Silhouette index` = Sil, # MAXimize
      `Dunn index` = D, # MAXimize
      `COP index` = COP, # MINimize
      `Davies-Bouldin index` = DB, # MINimize
      `Modified Davies-Bouldin index` = DBstar, # MINimize
      `Calinkski-Harabasz index` = CH, # MAXimize
      `Score function` = SF, # MAXimize
      `Elbow method` = ELBOW # MINimize
    ) %>% 
    pivot_longer(cols = -clust_window, names_to = 'eval_metric') %>% 
    # This one has been zero for all tests so far, 2023-11-23
    filter(eval_metric != 'Score function') %>% 
    # This one has been all over the place so far
    filter(eval_metric != 'Modified Davies-Bouldin index') %>% 
    separate(clust_window, into = c('n_clusters', 'n_windows'), sep = '_') %>% 
    mutate(
      n_clusters = as.numeric(n_clusters), # It was a list name, so needed to make numeric for plotting
      n_windows = as.numeric(n_windows), # It was a list name, so needed to make numeric for plotting
      index_goal = ifelse(
        eval_metric %in% c('COP index', 'Davies-Bouldin index', 'Modified Davies-Bouldin index'), 
        yes = 'minimize', no = 'maximize')
    )
  
  
  ggplot(clust.out.eval, aes(x = n_clusters, y = value, color = n_windows)) +
    geom_line(aes(group=n_windows), size=2) +
    facet_wrap(vars(eval_metric), scales='free_y') +
    theme_bw()
  
}

