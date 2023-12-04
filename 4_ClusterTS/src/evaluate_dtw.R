#' @title Evaluate the clustering output
#' @description Extract various cluster validity indices (CVIs) using the function
#' `dtwclust::cvi()` & add an additional metric to look at the cluster distances 
#' as an attempt at using the "elbow method" for evaluation. Note that this currently
#' only works when comparing metrics with different clusters or windows, not different
#' distance or centroid methods (though the file-naming allows for that).
#' 
#' TODO: handle different distance measures or centroid methods.
#' 
#' @param in_files a vector of qs file paths with the `dtwclust` output for  
#' a number of clustering algorithm attempts. Expects the names to be formatted
#' as `output_dist_[DISTANCE METHOD]_centroid_[CENTROID METHOD]_cluster_[N CLUSTERS]_window_[WINDOW SIZE].qs`
#' 
#' @return a tibble with the columns `n_clusters`, `n_windows`, `eval_metric`,
#' `value`, and `index_goal` with rows for every combination of clusters & windows
#' plus evaluation metrics available.
#' 
evaluate_dtw <- function(in_files) {
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
  
  # Return a table with various evaluation metrics to be used to determine 
  # which clustering configuration (n clusters, window size, etc) works best
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
        eval_metric %in% c('COP index', 'Davies-Bouldin index', 'Modified Davies-Bouldin index', 'Elbow method'), 
        yes = 'minimize', no = 'maximize')
    )
  
}

#' @title Visualize different clustering config results
#' @description Use output from `evaluate_dtw()` to visualize the results of 
#' different clustering configurations and find the best combination of clusters
#' and windows. Also, visualizes results for a single cluster config so that 
#' you can visualize how the window size differs.
#' 
#' @param dtw_eval a tibble with at least the columns `n_clusters`, `n_windows`,
#' `value`, and `eval_metric` to be used for plotting (e.g. output of `evaluate_dtw()`)
#' 
#' @return a ggplot object that contains line plots of the evaluation metrics
#' 
compare_dtw_cluster_configs <- function(dtw_eval) {
  uniq_clusters <- unique(dtw_eval$n_clusters)
  # Add different lines per window, but skip if there is only one 
  # window option present in the eval data
  if(length(uniq_clusters) > 1) {
    p_eval <- ggplot(dtw_eval, aes(x = n_clusters, y = value, color = n_windows)) +
      geom_line(aes(group=n_windows), linewidth=1) +
      facet_wrap(vars(eval_metric), scales='free_y') +
      theme_bw() +
      ggtitle('Comparing DTW results for various cluster & window sizes')
  } else {
    ggplot(dtw_eval, aes(x = n_windows, y = value)) +
      geom_line(linewidth=1) +
      facet_wrap(vars(eval_metric), scales='free_y') +
      theme_bw() +
      ggtitle(sprintf('Comparing DTW results for %s clusters & various window sizes', uniq_clusters))
  }
}

#' @title Process cluster output by site and year
#' @description Extract and format a table that includes every site-year
#' combination and the cluster in which it appeared for the optimal
#' clustering algorithm.
#' 
#' @param in_file qs file with the `dtwclust` output for the clustering
#' algorithm that was found to be the most optimal.
#' @param ts_list a tibble with at least the columns `site_no`, `year` and `[PARAM]_norm`
#' 
#' @return a tibble with the columns `site`, `year`, and `cluster`
#' 
process_dtw_clusters_bySiteYear <- function(in_file, ts_list) {
  cluster_optimal <- qread(in_file)
  
  site_cluster_output <- tibble(
    site_year = names(ts_list),
    cluster = cluster_optimal@cluster
  ) %>% 
    separate(site_year, into = c('site', 'year'), sep='-') %>% 
    mutate(year = as.numeric(year))
}

#' @title Process cluster output by site
#' @description Use output from `process_dtw_clusters_bySiteYear()` to get
#' a single cluster per site.
#' 
#' @param cluster_info_bySiteYear a tibble with at least the columns `site`, `year`, 
#' and `cluster`. Should be the output of `process_dtw_clusters_bySiteYear()`
#' 
#' @return a tibble with the columns `site`, `cluster`, and `frac_years` which is
#' the fraction of the years that appeared in the selected cluster per site.
#' 
choose_single_cluster_per_site <- function(cluster_info_bySiteYear) {
  cluster_info_bySiteYear <- tar_read(p4_dtw_clusters_bySiteYear)
  
  cluster_info_bySiteYear %>% 
    # Tally total number of years per site
    group_by(site) %>% 
    mutate(n_yrs_site = n()) %>%
    # Tally total number of years per cluster per site
    group_by(site, cluster) %>% 
    mutate(n_yrs_cluster = n()) %>% 
    ungroup() %>% 
    # Add fraction of years per cluster
    mutate(frac_years = n_yrs_cluster / n_yrs_site) %>% 
    # Choose one cluster per site based on the most frequent one
    group_by(site) %>%
    summarize(cluster = cluster[which.max(n_yrs_cluster)],
              frac_years = frac_years[which.max(n_yrs_cluster)])
  
}
