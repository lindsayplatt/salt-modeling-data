
#' @title Calculate a trend in SC per site
#' @description For each site in the data, apply different trend algorithms to 
#' categorize that site's SC data as trending 'positive', 'negative', or 'none'.
#' 
#' @param ts_data a tibble containing all daily SC records with at least the columns
#  `site_no`, `dateTime`, and `SpecCond`. Should use data after `3_Filter` phase
#' @param max_pval numeric value indicating the maximum p-value that is allowed
#' to declare a trend significant. Passed on to `extract_mk_trend()`. 
#' 
calculate_sc_trend <- function(ts_data, max_pval = 0.05) {
  ts_data %>% 
    split(.$site_no) %>% 
    map(~{tibble(
      baseflowTrend = apply_SeasonalKendall(.x, max_pval = 0.05)
    )}) %>%
    bind_rows(.id = 'site_no')
}

#' @title Find a trend in SC using a Seasonal Kendall test
#' @description Calculate a trend for SC data using `EnvStats::kendallSeasonalTrendTest()`
#' 
#' @param ts_data a tibble of SC timeseries data for a single site with at least
#' the columns `dateTime` and `SpecCond`.
#' @param max_pval numeric value indicating the maximum p-value that is
#' allowed to declare a trend significant. Passed on to `extract_sk_trend()`
#' 
#' @return a character string indicating the trend as "none", "positive", or "negative"
#' 
apply_SeasonalKendall <- function(ts_data, max_pval) {
  
  # According to PNNL: https://vsp.pnnl.gov/help/Vsample/Design_Trend_Seasonal_Kendall.htm
  # Use seasonal MK when the "presence of seasonality implies that the data have different distributions for 
  #   different seasons (e.g., months) of the year. For example, a monotonic upward trend may exist over years 
  #   for January but not for June." AND a "monotonic upward (downward) trend means that the variable consistently 
  #   increases (decreases) over time, but the trend may or may not be linear."
  # Use MK when "seasonality is not expected to be present or when trends occur in different directions 
  #   (up or down) in different seasons." If there is seasonality but trends might be in different directions,
  #   looks like you might be able to "de-seasonalize" first?? See examples in `?Kendall::MannKendall`
  # While I think that the trends may be different by season, I'm not sure we can apply the blanket statement 
  # that they will all either be going up or down. I think there may be instances where winter seasons are going
  # up but summer are going down?
  
  # To use a Seasonal Kendall, first transform the data into monthly medians
  ts_data_monthly <- ts_data %>% 
    mutate(year = lubridate::year(dateTime),
           month = as.numeric(lubridate::month(dateTime))) %>% 
    group_by(year, month) %>% 
    summarize(SpecCond_med = median(SpecCond), .groups="keep") %>% 
    ungroup()
  
  # Run the Seasonal Kendall test and extract the trend result
  EnvStats::kendallSeasonalTrendTest(SpecCond_med ~ month + year, 
                                       data = ts_data_monthly) %>% 
    extract_sk_trend(max_pval = max_pval)
  
}

#' @title Extract the trend from Seasonal Kendall (SK output)
#' @description Use the output from a `EnvStats::kendallSeasonalTrendTest()` 
#' to categorize the trend as either `none`, `positive`, or `negative`. 
#' 
#' @param sk_output a model object of the class `htestEnvStats` from running 
#' `EnvStats::kendallSeasonalTrendTest()`.
#' @param max_pval numeric value indicating the maximum p-value that is
#' allowed to declare a trend significant. Any model output with a p-value
#' above this value will have return "none" for the trend. Defaults to 0.05.
#' 
#' @return a character string indicating the trend as "none", "positive", or "negative"
#' 
extract_sk_trend <- function(sk_output, max_pval = 0.05) {
  
  # Extract the pvalue
  pval <- sk_output$p.value[['z (Trend)']]
  
  # Extract the slope 
  slope <- sk_output$estimate[['slope']]
  
  # Return the appropriate trend name based on the
  # Seasonal Kendall slope but only if the
  # p-value was below `max_pval`.
  if(pval >= max_pval | slope == 0) {
    trend <- "none"
  } else if(slope < 0) {
    trend <- "negative"
  } else if(slope > 0) {
    trend <- "positive"
  }
  
  return(trend)
}
