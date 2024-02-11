# TODO: DOCUMENTATION!
filter_ts_to_baseflow_days <- function(ts_data, ts_bf_days, param_colname) {
  ts_data %>% 
    left_join(ts_bf_days, by = c('site_no', 'dateTime')) %>% 
    filter(is_baseFlow_day) %>% 
    select(site_no, dateTime, !!as.name(param_colname))
}

# TODO: DOCUMENTATION!
# A site needs a minimum amount of data across different seasons in order for a trend to be calculated.
apply_baseflow_trend_criteria <- function(ts_data_bf, min_avg_samples_per_season = 2, 
                                          min_years_per_season = 5, min_seasons = 3) {
  # TODO: consider moving these into 3_Filter.
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

# Filter to sites who met our qualification criteria
# TODO: docs!
identify_trend_sites <- function(site_season_bf_qualified) {
  site_season_bf_qualified %>% 
    filter(site_bf_qualified) %>% 
    pull(site_no) %>% 
    unique()
}
