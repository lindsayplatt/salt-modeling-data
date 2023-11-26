
#' @title Filter only to sites that have all available data
#' @description The detrending algorithm will not work if there are any
#' missing values in the timeseries. This identifies only those sites that
#' have complete records to use as an argument in `detrend_sc_data()`. It will
#' check against the column that contains the gap-filled data, `[PARAM]_fill`
#' 
#' @param ts_data a tibble with at least the columns `site_no` and `[PARAM]`
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values. In this workflow, this is likely `SpecCond`.
#'  
#' @return a vector of site numbers
#' 
identify_sites_to_detrend <- function(ts_data, param_colname) {
  n_sites_before <- length(unique(ts_data$site_no))
  
  sites_to_detrend <- ts_data %>% 
    # Temporarily rename the data column so that this can handle any param
    rename_with(~gsub(param_colname, 'PARAM', .x)) %>% 
    # By site, identify if the column has any NA values
    group_by(site_no) %>% 
    summarize(hasNAs = any(is.na(PARAM_fill))) %>% 
    # If there were any NA values (even just one!), filter out that site
    filter(!hasNAs) %>% 
    # Return only the site numbers as a character vector
    pull(site_no) 
  
  message(sprintf('%s out of %s sites can be detrended', 
                  length(sites_to_detrend), n_sites_before))
  
  return(sites_to_detrend)
}

#' @title Detrend a timeseries
#' @description Detrend a timeseries by site using functions and packages from
#' the `tidyverts` collection of "tidy tools for time series analysis", including
#' `tsibble` and `feasts`. This can handle data from one or more sites at a time
#' but the detrending algorithm, `feasts::STL()`, only works for complete timeseries
#' record and will error if there are NAs (hence the `sites_to_detrend` argument).
#' 
#' @param ts_data a tibble with at least the columns `site_no`, `dateTime`, 
#' `[PARAM]`, and `[PARAM]_fill` (gap-filled column)
#' @param sites_to_detrend character vector of site numbers that don't have any
#' missing data as identified by `identify_sites_to_detrend()`.
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values. In this workflow, this is likely `SpecCond`.
#' 
#' @return tibble with the columns `site_no`, `dateTime`, `SpecCond` (original 
#' values), `SpecCond_fill` (gap-filled values), and `SpecCond_detrend` (gap-filled
#' values that no longer have a trend).
#' 
detrend_ts_data <- function(ts_data, sites_to_detrend, param_colname) {
  
  ts_data_to_detrend <- ts_data %>% 
    # Only keep sites that did not have any missing data
    filter(site_no %in% sites_to_detrend) %>%  
    # Temporarily rename the data column so that this can handle any param
    rename_with(~gsub(param_colname, 'PARAM', .x)) 
  
  ts_data_to_detrend %>% 
    # Convert to a timeseries tibble, tsibble
    # using the dateTime column as the time component
    # and specifying the site_no as the `key` (meaning
    # treating as separate timeseries)
    as_tsibble(index = dateTime, key = site_no) %>% 
    # Apply the STL model to the gap-filled 
    # values and use an annual period
    model(STL(PARAM_fill ~ season(period = '1 year'))) %>% 
    # Extract values from the model as columns
    components() %>% 
    # Compute the "detrended" 
    mutate(PARAM_detrend = PARAM_fill - trend) %>%
    # Convert back to a regular tibble (not ts tibble)
    as_tibble() %>% 
    # Add the original PARAM column back in (removed because 
    # it was not a column used during the model step)
    mutate(PARAM = ts_data_to_detrend$PARAM) %>% 
    # Return only the necessary columns
    dplyr::select(site_no, 
                  dateTime, 
                  PARAM,
                  PARAM_fill,
                  PARAM_detrend) %>% 
    # Reverse the temporary column names back to the original 
    rename_with(~gsub('PARAM', param_colname, .x))
  
}
