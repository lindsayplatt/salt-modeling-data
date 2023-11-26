
#' @title Filter sites to those that meet minimum data quantity criteria
#' @description Apply a minimum number of years + a minimum date requirement to
#' filter to lengthier time series that have had data recently.
#' 
#' @param ts_data a tibble of daily water quality time series. It needs at least the 
#' columns `site_no` and `dateTime`
#' @param min_years numeric value indicating the minimum number of years that the
#' minimum and maximum parameter values must span. This assumes that the data has 
#' already been filtered past large gaps (see `filter_beyond_large_ts_gaps()`) 
#' and that any remaining gaps can be filled with the WRTDS method). Defaults to 5.
#' @param min_recent_date Date value indicating the most recent date that the 
#' `ts_data` *MUST* have at least one record beyond. Defaults to `2007-01-01`, 
#' which (at the time of this writing) meant a site must have had at least one 
#' record in the last 15 years.
#' 
#' @return a vector of NWIS site character strings whose `ts_data` met requirements
#' 
identify_qualifying_sites <- function(ts_data, min_years = 5, min_recent_date = as.Date('2007-01-01')) {
  
  qualifying_sites <- ts_data %>% 
    group_by(site_no) %>% 
    summarize(min_date = min(dateTime),
              max_date = max(dateTime)) %>% 
    mutate(year_span = as.numeric(max_date - min_date)/365) %>% 
    ungroup() %>% 
    # Filter to sites that have the minimum number of years of data (even 
    # if there are some gaps, which will be filled by WRTDS)
    filter(year_span >= min_years) %>% 
    # Filter to sites that have some data more recently than `min_recent_date`
    filter(max_date >= min_recent_date) %>% 
    pull(site_no)
  
}
