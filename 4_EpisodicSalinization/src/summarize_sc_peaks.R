
#' @title Identify sites that qualify as episodic winter sites
#' @description Using peaks identified with `find_event_peaks()`, this function
#' will summarize peaks per site based on whether or not they occurred during
#' winter. Then, it will add a new field indicating whether the site qualifies
#' as an "episodic winter salting site", meaning that on average specific
#' conductance peaks occur during the winter a minimum percent of the time.
#' 
#' @param ts_peak_data a tibble output from `find_event_peaks()` with at least the 
#' columns `site_no`, `dateTime`, and `peak_flag`.
#' @param winter_months a numeric vector indicating which months should be 
#' considered "winter"; defaults to `c(12,1,2,3)` or Dec, Jan, Feb, and Mar.
#' @param min_winter_perc a single numeric value indicating the percentage of 
#' time the peak values should occur in winter to qualify the site as an episodic
#' winter salting site. Given as a fraction; defaults to `0.50` (or 50%).
#' 
summarize_salt_peaks <- function(ts_peak_data, winter_months = c(12,1,2,3), 
                                 min_winter_perc = 0.50) {
  # Calculate total peaks by site, year, and season
  peak_summary <- ts_peak_data %>% 
    # Add some additional date-time columns
    mutate(year = year(dateTime), 
           month = month(dateTime)) %>% 
    # Create a winter flag - if month in (12,1,2,3)
    mutate(is_winter = month %in% winter_months) %>% 
    mutate(is_winter = factor(is_winter, levels = c(TRUE, FALSE))) %>% 
    # Count the total number of peaks per site, year, and winter/non-winter
    group_by(site_no, year, is_winter) %>% 
    summarize(n_peaks_season = sum(peak_flag, na.rm = T), .groups='keep')
  
  # TODO: verify the criteria. See https://github.com/lindsayplatt/salt-modeling-data/issues/44
  # To qualify as winter salting sites for this analysis, more than 
  # 50% of the SpC peaks need to happen during winter on average.
  salt_sites_info <- peak_summary %>% 
    # Calculate percent of peaks in winter/non-winter for each year
    group_by(site_no, year) %>% 
    mutate(n_peaks_total = sum(n_peaks_season, na.rm=TRUE)) %>% 
    mutate(peaks_perc = n_peaks_season/n_peaks_total) %>% 
    # Find the average percentage of peaks per season
    group_by(site_no, is_winter) %>%  
    summarize(peaks_perc_avg = mean(peaks_perc, na.rm=TRUE), .groups='keep') %>% 
    pivot_wider(names_from = is_winter, values_from = peaks_perc_avg, names_prefix = 'winter') |> 
    mutate(is_salt_site = winterTRUE > min_winter_perc)
  
  return(salt_sites_info)
}
