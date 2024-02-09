# TODO: DOCUMENTATION!
filter_ts_to_baseflow_days <- function(ts_data, ts_bf_days, param_colname) {
  ts_data %>% 
    left_join(ts_bf_days, by = c('site_no', 'dateTime')) %>% 
    filter(is_baseFlow_day) %>% 
    select(site_no, dateTime, !!as.name(param_colname))
}
