
#' @title Combine all static attributes data
#' @description Using `site_no` as the identifier, this function combines 
#' all of the desired static attributes into a single table. If the 
#' attribute was not known for a particular site, there will be an NA in 
#' that column. No filtering is done to remove sites without certain 
#' attributes in this function.
#' 
#' @param ts_data a tibble of daily water quality time series. It needs at least the 
#' columns `site_no`, `dateTime`, and `[PARAM]`
#' @param ts_bf_days a tibble specifying whether a date is a baseflow day for 
#' each site; has the columns `site_no`, `dateTime`, and `is_baseFlow_day`
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values. In this workflow, this is likely `SpecCond`.
#' 
#' @return a tibble with only the rows that matched site-days from `ts_bf_days`
#' for days with only flow made of baseflow data. Has the columns `site_no`, 
#' `dateTime`, and `[PARAM]`
#' 
filter_ts_to_baseflow_days <- function(ts_data, ts_bf_days, param_colname) {
  ts_data %>% 
    left_join(ts_bf_days, by = c('site_no', 'dateTime')) %>% 
    filter(is_baseFlow_day) %>% 
    select(site_no, dateTime, !!as.name(param_colname))
}

#' @title Determine if a site can be used to calculate baseflow trends
#' @description A site needs a minimum amount of data across different seasons (aka
#' month in this case) in order for a trend to be calculated. This function applies 
#' criteria to the baseflow-only data to remove sites that don't qualify to have a 
#' trend calculated. First, a site's months need to qualify: each month needs a minimum 
#' of 2 samples on average months at least 5 years of data. Then, a site needs
#' to have at least 3 different seasons qualify to be kept.
#' 
#' @param ts_data_bf a tibble of daily water quality time series with only those
#' dates considered a "baseflow day". It needs at least the columns `site_no`, 
#' `dateTime`, and `[PARAM]`
#' @param min_avg_samples_per_season minimum number of values that a given site
#' and season (aka month here) needs to have on average each year. Defaults to 2.
#' @param min_years_per_season minimum number of years per season (aka month in
#' this case) that a given site needs to have data. Defaults to 5.
#' @param min_seasons minimum number of seasons (aka months) that need to pass the 
#' qualifying criteria (`min_avg_samples_per_season` and `min_years_per_season`)
#' per site in order for that site to be used in the baseflow trend analysis.
#' 
#' @return a tibble with only the rows that matched site-seasons (aka site-months) 
#' from `ts_bf_days`. Has the columns `site_no`, `month`, and `avg_samples_per_season`,
#' `n_years_season`, `site_season_bf_qualified`, and `site_bf_qualified`. Contains
#' all site-months, regardless of whether they passed qualification or not.
#' 
apply_baseflow_trend_criteria <- function(ts_data_bf, min_avg_samples_per_season = 2, 
                                          min_years_per_season = 5, min_seasons = 3) {
  
  # As noted in USGS methods for seasonal kendall, it is OK to use the median to
  # calculate trend if there are "generally two or more samples taken in each season"
  ts_data_bf %>% 
    mutate(year = year(dateTime), 
           month = month(dateTime)) %>% 
    group_by(site_no, year, month) %>% 
    summarize(n_samples_per_yr_season = n(), .groups='keep') %>% 
    group_by(site_no, month) %>% 
    summarize(avg_samples_per_season = mean(n_samples_per_yr_season),
              n_years_season = n(), .groups='keep') %>% 
    # Only keep seasons that have have more than an average of two samples
    # and at least 5 different years of data
    mutate(site_season_bf_qualified =
             avg_samples_per_season >= min_avg_samples_per_season & 
             n_years_season >= min_years_per_season) %>% 
    group_by(site_no) %>% 
    # Then, only allow a site if it had at least 3 seasons that qualified
    mutate(site_bf_qualified = sum(site_season_bf_qualified) >= min_seasons)
}

#' @title Filter to sites who met our qualification criteria
#' @description A site needs a minimum amount of data across different seasons (aka
#' month in this case) in order for a trend to be calculated. The function 
#' `apply_baseflow_trend_criteria()` applied such criteria and saved a table that
#' said whether a site qualified or not. This function filters the actual baseflow
#' time series data to only those sites and seasons (aka months) which qualified.
#' 
#' @param ts_data_bf a tibble of daily water quality time series with only those
#' dates considered a "baseflow day". It needs at least the columns `site_no`, 
#' `dateTime`, and `[PARAM]`
#' @param site_season_bf_qualified a tibble with only the rows that matched 
#' site-seasons (aka site-months) from `ts_bf_days`. Contains output from the 
#' function `apply_baseflow_trend_criteria()`
#' 
#' @return a tibble of time series data for only sites in seasons where enough 
#' of the days qualified as baseflow days so that a trend can be calculated. The
#' tibble should have the columns `site_no`, `dateTime`, `SpecCond`, and `month`.
#' 
filter_ts_to_qualified_site_seasons <- function(ts_data_bf, site_season_bf_qualified) {
  site_season_qualified <- site_season_bf_qualified %>% 
    filter(site_bf_qualified & site_season_bf_qualified) %>% 
    select(site_no, month) %>% 
    distinct()
  
  ts_data_bf %>% 
    mutate(month = month(dateTime)) %>%
    right_join(site_season_qualified, by = c('site_no', 'month'))
}
