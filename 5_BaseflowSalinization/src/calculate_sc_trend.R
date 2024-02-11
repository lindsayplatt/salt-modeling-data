
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
  # TODO: which of these trend tests should we use?
  ts_data %>% 
    split(.$site_no) %>% 
    map(~{tibble(
      attr_Trend = apply_SeasonalKendall(.x, max_pval = 0.05)
    )}) %>%
    bind_rows(.id = 'site_no')
}

#' @title Find a trend in SC using a Seasonal Mann-Kendall test
#' @description Calculate a trend for SC data using `Kendall::SeasonalMannKendall()`
#' 
#' @param ts_data a tibble of SC timeseries data for a single site with at least
#' the columns `dateTime` and `SpecCond`.
#' @param max_pval numeric value indicating the maximum p-value that is
#' allowed to declare a trend significant. Passed on to `extract_mk_trend()`
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
  
  # To use a Seasonal Mann-Kendall, first transform the data into monthly medians
  # TODO: SWITCH TO EnvStats::kendallSeasonalTrendTest()
  ts_data_monthly <- ts_data %>% 
    mutate(year = lubridate::year(dateTime),
           month_num = sprintf('%02d', lubridate::month(dateTime))) %>% 
    group_by(year, month_num) %>% 
    summarize(SpecCond_med = median(SpecCond), .groups="keep") %>% 
    ungroup() %>% 
    pivot_wider(names_from = month_num, values_from = SpecCond_med) %>% 
    # Arrange columns so January is first, then replace with abbreviations instead of numbers
    relocate(year, order(names(.))) %>% 
    rename_with(~month.abb[as.numeric(.x)], -year)
  
  # Convert the data into a `ts` object to pass to `SeasonalMannKendall()`
  smk_data <- ts_data_monthly %>% 
    # Remove the `year` column so that just the data is converted to a `ts` obj
    dplyr::select(-year) %>% 
    ts(start = c(min(ts_data_monthly$year), 1), frequency = 1)
  
  # Run the Seasonal Mann-Kendall model and extract the trend result
  smk_data %>% 
    Kendall::SeasonalMannKendall() %>% 
    extract_mk_trend(max_pval = max_pval)
  
}

#' @title Extract the trend from MannKendall (MK output)
#' @description Use the output from a `Kendall::MannKendall()` or 
#' `Kendall::SeasonalMannKendall()` model run to categorize the trend
#' as either `none`, `positive`, or `negative`. 
#' 
#' @param mk_output a model object of the class `Kendall` from running either
#' `Kendall::MannKendall()` or `Kendall::SeasonalMannKendall()`.
#' @param max_pval numeric value indicating the maximum p-value that is
#' allowed to declare a trend significant. Any model output with a p-value
#' above this value will have return "none" for the trend. Defaults to 0.05.
#' 
#' @return a character string indicating the trend as "none", "positive", or "negative"
#' 
extract_mk_trend <- function(mk_output, max_pval = 0.05) {
  
  # Extract the pvalue programmatically
  pval <- capture.output(summary(mk_output))[3] %>% 
    str_split_1("pvalue =") %>% tail(1)
  
  # Drop the '<' that can sometimes be present for
  # super small numbers.
  pval <- as.numeric(gsub('< ', '', pval))
  
  # Return the appropriate trend name based on the
  # MannKendall `Score` (aka S) but only if the
  # p-value was below `max_pval`.
  if(pval >= max_pval | mk_output$S == 0) {
    trend <- "none"
  } else if(mk_output$S < 0) {
    trend <- "negative"
  } else if(mk_output$S > 0) {
    trend <- "positive"
  }
  
  return(trend)
}
