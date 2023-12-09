
#' @title Remove site-years that are short.
#' @description Some years start late or end early (at the ends of a site's time
#' range). These years should not be included in the clustering model because we
#' are only wanting to use complete years.
#' 
#' @param ts_data a tibble with at least the columns `site_no` and `date`
#' 
#' @return tibble with the same columns as `ts_data` but fewer rows
#' 
remove_incomplete_years <- function(ts_data) {
  
  # Count how many site-year time series we started with
  n_siteyrs_all <- ts_data %>% 
    select(site_no, year) %>% 
    distinct() %>% nrow()
  
  ts_data_siteyr_complete <- ts_data %>% 
    # Identify the start/end day for each annual ts
    mutate(doy = yday(date)) %>% 
    group_by(site_no, year) %>% 
    mutate(first_day = min(doy),
           last_day = max(doy)) %>% 
    ungroup() %>% 
    # Filter to site-years that are complete
    filter(first_day == 1, last_day >= 365) %>% 
    # Remove columns added in this function
    select(-doy, -first_day, -last_day)
  
  # Count how many site-year time series remain
  n_siteyrs_complete <- ts_data_siteyr_complete %>% 
    select(site_no, year) %>% 
    distinct() %>% nrow()
  
  message(sprintf('%s out of %s site-years remain (%s removed)',  
                  n_siteyrs_complete, n_siteyrs_all, 
                  n_siteyrs_all - n_siteyrs_complete))
  
  return(ts_data_siteyr_complete)
}

# Load the znorm function from an earlier phase script
znorm <- function() {}
insertSource("2_Prepare/src/attr_prep_fxns.R", functions="znorm") 

#' @title Apply normalization to each site's time series data
#' @description Normalize the time series data per site (not per site-year) 
#' before passing into the clustering algorithm.
#' 
#' @param ts_data a tibble with at least the columns `site_no` and `[PARAM]`
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values. In this workflow, this is likely `SpecCond`.
#' 
#' @return tibble with all the columns from `ts_data` plus `[PARAM]_norm`
#' 
normalize_data_bysite <- function(ts_data, param_colname) {
  ts_data %>% 
    # Temporarily rename the data column so that this can handle any param
    rename_with(~gsub(param_colname, 'PARAM', .x)) %>% 
    # By site, normalize the data by z-scoring
    group_by(site_no) %>% 
    mutate(PARAM_norm = znorm(PARAM)) %>% 
    ungroup() %>% 
    # Reverse the `PARAM` placeholder column names 
    rename_with(~gsub('PARAM', param_colname, .x))
}

#' @title Convert data into list of vectors for clustering
#' @description For clustering, each site-year combination will be treated as its
#' own separate time series will be clustered individually. This function takes
#' the full dataset and converts into the format that `dtwclust::tsclust()` needs,
#' where each list item is a separate time series (vectors of values).
#' 
#' @param ts_data a tibble with at least the columns `site_no`, `year` and `[PARAM]_norm`
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values. In this workflow, this is likely `SpecCond`.
#' 
#' @return a list with an entry for each site-year combination. Each list item 
#' is just a vector of values (no tables), named with the '[SITE]-[YEAR]'.
#' 
convert_ts_data_into_list <- function(ts_data, param_colname) {
  ts_data %>% 
    mutate(site_yr = sprintf('%s-%s', site_no, year)) %>% 
    split(.$site_yr) %>%  
    map(pull, var = sprintf('%s_norm', param_colname))
}
