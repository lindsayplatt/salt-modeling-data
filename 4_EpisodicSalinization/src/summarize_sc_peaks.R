
#' @title Identify sites that qualify as episodic winter sites
#' @description Using peaks identified with `find_event_peaks()`, this function
#' will summarize peaks per site based on whether or not they occurred during
#' winter. Then, it will add a new field indicating whether the site qualifies
#' as an "episodic winter salting site", meaning that on average specific
#' conductance peaks occur during the winter a minimum percent of the time and
#' the difference between winter and non-winter average specific conductance is 
#' greater than some minimum percent.
#' 
#' @param ts_peak_data a tibble output from `find_event_peaks()` with at least the 
#' columns `site_no`, `dateTime`, and `peak_flag`.
#' @param winter_months a numeric vector indicating which months should be 
#' considered "winter"; defaults to `c(12,1,2,3)` or Dec, Jan, Feb, and Mar.
#' @param min_winter_perc a single numeric value indicating the percentage of 
#' time the peak values should occur in winter to qualify the site as an episodic
#' winter salting site. Given as a fraction; defaults to `0.50` (or 50%).
#' @param min_perc_diff a single numeric value indicating the minimum percent  
#' difference between average winter and non-winter specific conductance to 
#' qualify the site as an episodic winter salting site. Given as a fraction; 
#' defaults to `0.10` (or 10%).
#' 
summarize_salt_peaks <- function(ts_peak_data, winter_months = c(12,1,2,3), 
                                 min_winter_perc = 0.50, min_perc_diff = 0.10) {
  
  # Calculate total peaks by site, year, and season
  peak_summary <- ts_peak_data %>% 
    # Add some additional date-time columns
    mutate(year = year(dateTime), 
           month = month(dateTime)) %>% 
    # Create a winter flag - if month in (12,1,2,3)
    mutate(is_winter = month %in% winter_months) %>% 
    # mutate(is_winter = factor(is_winter, levels = c(TRUE, FALSE))) %>% 
    # Count the total number of peaks per site, year, and winter/non-winter
    # and add the average annual winter/non-winter specific conductance
    group_by(site_no, year, is_winter) %>% 
    summarize(n_peaks_season_annual = sum(peak_flag, na.rm = T), 
              avg_sc_season_annual = mean(SpecCond, na.rm = T), .groups='keep')
  
  # To qualify as winter salting sites for this analysis, more than  
  # `min_winter_perc` of the SpC peaks need to happen during winter 
  # on average and the percent difference between winter and non-winter 
  # SpC averages should be greater than `min_perc_diff`
  salt_sites_info <- peak_summary %>% 
    # Calculate percent of peaks in winter/non-winter for each year
    group_by(site_no, year) %>% 
    mutate(n_peaks_annual = sum(n_peaks_season_annual, na.rm=TRUE)) %>% 
    mutate(peaks_perc_annual = n_peaks_season_annual/n_peaks_annual) %>% 
    # Find the average percentage of peaks per season & calculate
    # the average specific conductance for each season
    group_by(site_no, is_winter) %>%  
    summarize(peaks_perc = mean(peaks_perc_annual, na.rm=TRUE), 
              sc_season = mean(avg_sc_season_annual, na.rm=TRUE), .groups='keep') %>% 
    # Munge the data so that there is a column per metric per season
    pivot_longer(cols = -c(site_no, is_winter), names_to = 'metric') %>% 
    mutate(metric_season = sprintf('%s_%s', metric, ifelse(is_winter, 'winterYes', 'winterNo'))) %>% 
    pivot_wider(id_cols = site_no, names_from = metric_season, values_from = value) %>%
    mutate(sc_perc_diff = ((sc_season_winterYes - sc_season_winterNo) / sc_season_winterYes)) %>% 
    select(site_no, peaks_perc_winterYes, sc_perc_diff) %>% 
    # Now determine if it is a salt site based on the percent of peaks occurring 
    # in winter and the difference between the average winter/non-winter SC
    mutate(is_salt_site = peaks_perc_winterYes > min_winter_perc & sc_perc_diff > min_perc_diff)
  
  return(salt_sites_info)
}
