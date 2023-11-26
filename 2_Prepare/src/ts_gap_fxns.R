
#' @title Prepare data to be gap-filled using WRTDS
#' @description Not all sites have data that can be gap-filled using WRTDS (e.g.
#' sites with negative flow values, sites without any missing data on days with
#' a flow value). This can accept input for more than one site at once.
#' 
#' @param data_q a tibble with flow data, needs at least the columns `dateTime`, 
#' and `Flow` columns. 
#' @param data_param a tibble with a water quality parameter of interest to be
#' regressed with WRTDS. It needs at least the columns `site_no`, `dateTime` and 
#' `[PARAM]`. In this workflow, the data is likely specific conductance.
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values. In this workflow, this is likely `SpecCond`.
#' @param sites_that_crash vector of site numbers that caused an issue when they
#' were used in `apply_wrtds()`. They can be investigated further later.
#' 
#' @return a tibble of param data with flows combined that is ready to be passed
#' to `apply_wrtds()` to fill in missing data. Will have the columns `site_no`,
#' `dateTime`, `Flow`, `Flow_cd`, and `[PARAM]`
#' 
prep_data_for_wrtds <- function(data_q, data_param, param_colname, sites_that_crash) {
  
  param_date_info <- data_param %>% 
    # Per site, identify the time range of the parameter data
    group_by(site_no) %>% 
    summarize(param_start = min(dateTime),
              param_end = max(dateTime)) %>% 
    ungroup()
  
  data_q %>% 
    # First, match up parameter data to a flow record (we don't want any
    # param values without a corresponding flow record)
    left_join(data_param, by = c('site_no', 'dateTime')) %>% 
    # Now filter the flow data to only be for dates within the time range of the
    # parameter data per site so that we are not using WRTDS to extend the param
    # beyond its time range (which EGRET functions warn us against, so removing 
    # that data up front to save on processing time)
    left_join(param_date_info, by = c('site_no')) %>% 
    filter(dateTime >= param_start,
           dateTime <= param_end) %>% 
    # Prep some filtering information by site
    group_by(site_no) %>% 
    mutate(has_missing_param = any(is.na(!!as.name(param_colname))),
           has_negative_flows = any(Flow < 0)) %>% 
    ungroup() %>% 
    # Now only keep those sites that actually have a missing param value where
    # there is a flow value. If not, this will error when we get to WRTDS
    filter(has_missing_param) %>% 
    # Remove any sites that have negative flow values
    filter(!has_negative_flows) %>% 
    # Select only the columns we need to run WRTDS
    dplyr::select(site_no, dateTime, Flow, Flow_cd, !!as.name(param_colname)) %>% 
    # Filter out the sites that crashed in `apply_wrtds()` before
    filter(!site_no %in% sites_that_crash)
  
}

#' @title Run WRTDS to generate a complete timeseries
#' @description Use the EGRET R package to run a Weighted Regression in Time,
#' Discharge, and Season (WRTDS) to generate a timeseries without any NA values.
#' The output of this will be used to fill gaps.
#' 
#' @param data_q_param a single site's combined flow and water quality parameter
#' data, needs at least the columns `dateTime`, `Flow`, `Flow_cd`, and `[PARAM]`.
#' The function assumes the values in `Flow` are in cubic feet per second (cfs)
#' and that the values in `[PARAM]` represent daily means from a continuous 
#' dataset (not usually how WRTDS works). Due to the data we have to pass in, 
#' we fill in low and high daily concentrations with the same value, equal to the 
#' daily mean (whatever is in `[PARAM]` column. In addition, we assume there are
#' no censored values. In this workflow, the data is likely specific conductance.
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values. In this workflow, this is likely `SpecCond`.
#' @param param_nwis_cd the 5-digit character string representing the NWIS code
#' for this particular parameter so that information in EGRET's WRTDS functions
#' can be gathered. In this workflow, it is likely '00095' for specific conductance.
#' 
#' @return a tibble with the columns `site_no`, `dateTime`, `Flow`, `[PARAM]_wrtds`
#' (WRTDS daily output), and `[PARAM]_wrtds_stderr` (the standard error associated  
#' with the WRTDS daily output).
#' 
apply_wrtds <- function(data_q_param, param_colname, param_nwis_cd) {
  
  site_number <- unique(data_q_param$site_no)
  
  # Mostly suppress the EGRET function messages with `verbose=FALSE` because 
  # they can be A LOT to print out as the pipeline is running. Instead, we 
  # should see this message and one when the EGRET fxns are complete.
  message(sprintf("Starting EGRET's WRTDS for %s (%s param pts for %s days)", 
                  site_number, sum(!is.na(data_q_param[[param_colname]])), 
                  max(data_q_param$dateTime) - min(data_q_param$dateTime) + 1))
  
  # Prepare data for using WRTDS functions. Get the site information (yes, this
  # queries NWIS and should technically go in `1_Download` but it is super fast
  # so it was easier to just put it in here and duplicate some of that info).
  # Convert flow and water quality data into the correctly structured data sets
  wrtds_info <- readNWISInfo(site_number, param_nwis_cd, interactive = FALSE)
  wrtds_daily <- data_q_param %>% 
    rename(value = Flow, code = Flow_cd) %>% 
    populateDaily(qConvert = 35.314667, # Convert ft3/s to m3/s
                  verbose = FALSE)
  wrtds_sample <- data_q_param %>%  
    # First, remove the dates that had flow but didn't have SC before  
    # trying to run WRTDS (which will fill in those gaps)
    filter(!is.na(SpecCond)) %>%
    # Set up parameter data in prep for EGRET WRTDS methods
    mutate(ConcLow = !!as.name(param_colname), 
           ConcHigh = !!as.name(param_colname),
           Uncen = 1) %>% 
    populateSampleColumns()
  
  # Step 1: Use `mergeReport()` to prepare `eList`
  eList <- mergeReport(INFO = wrtds_info, Daily = wrtds_daily, 
                       Sample = wrtds_sample, verbose = FALSE)
  
  # Step 2: add surfaces with `modelEstimation()` so that `yHat` and `SE` are
  # added directly to the eList Sample and Daily data frames:
  eList <- modelEstimation(eList, verbose = FALSE)
  
  # Step 3: Run WRTDS-Kalman to generate a complete time series
  eList <- WRTDSKalman(eList, verbose = FALSE)
    
  message("Completed EGRET's WRTDS for ", site_number)
  
  # Format and return the data from the WRTDS output 
  wrtds_out <- eList$Daily %>% 
    # Add the site_no as a column (this fxn only does one site at a time)
    mutate(site_no = site_number) %>% 
    # Arrange and rename some of the columns before returning
    dplyr::select(site_no, 
                  dateTime = Date, 
                  Flow = Q,
                  !!as.name(sprintf('%s_wrtds', param_colname)) := ConcDay, 
                  !!as.name(sprintf('%s_wrtds_stderr', param_colname)) := SE)
  
  return(wrtds_out)
}

#' @title Fill gaps in a timeseries using WRTDS data
#' @description Fill gaps in a timeseries using the output from `apply_wrtds()`. 
#' This currently only uses WRTDS values when there is an NA in the timeseries 
#' data. 
#' 
#' @param ts_data a tibble with at least the columns `site_no`, `dateTime`, and `[PARAM]`
#' @param wrtds_data a tibble with at least the columns `site_no`, `dateTime`, and 
#' `[PARAM]_wrtds`. Should be the output of `apply_wrtds()`.
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values. In this workflow, this is likely `SpecCond`.
#' 
#' @return tibble with all columns related to the parameter and gap-filling: 
#' `site_no`, `dateTime`, `[PARAM]`, `[PARAM]_wrtds`, `[PARAM]_fill`. Plus, additional 
#' rows compared to `ts_data` because there is now data for each day between the 
#' min and max dates. Note that this drops `[PARAM]_cd` from the input `ts_data`
#' and `[PARAM]_wrtds_stderr` from the input `wrtds_data`.
#' 
fill_ts_gaps_wrtds <- function(ts_data, wrtds_data, param_colname) {
  
  # Create a tibble that includes all possible combinations of dates per site
  ts_data_all_days <- ts_data %>% 
    split(.$site_no) %>% 
    map(~{
      tibble(site_no = unique(.x$site_no),
             dateTime = seq(min(.x$dateTime), 
                            max(.x$dateTime),
                            by = 'days')) %>% 
        # Join in the real data
        left_join(.x, by = c('site_no', 'dateTime'))
    }) %>% bind_rows()
  
  wrtds_data %>% 
    # Use `full_join` to keep all the values in `wrtds_data` and `ts_data` because
    # some sites did not get passed through WRTDS but we want to include them in
    # the parameter data set after filling gaps where it's possible
    full_join(ts_data_all_days, by = c('site_no', "dateTime")) %>% 
    # Temporarily rename the data column so that this can handle any param
    rename_with(~gsub(param_colname, 'PARAM', .x)) %>% 
    # Create a new column that combines the parameter timeseries values to create
    # a complete (no NA) timeseries. It will only use the WRTDS values when there
    # is an NA for `ts_data`.
    mutate(PARAM_fill = ifelse(is.na(PARAM), 
                               yes = PARAM_wrtds, 
                               no = PARAM)) %>% 
    # Select the columns we need
    dplyr::select(site_no, dateTime, PARAM, PARAM_wrtds, PARAM_fill) %>% 
    # Reverse the `PARAM` placeholder column names 
    rename_with(~gsub('PARAM', param_colname, .x))
  
}

#' @title Summarize counts of pre- and post-gap filling.
#' @description Tally counts site-years that now qualify for dynamic time-warping
#' algorithms because there are no NAs. Includes counts of how many site-year
#' timeseries qualified before and after the gap-filling. This should have all
#' sites' data passed in at once.
#' 
#' @param ts_filled_data a tibble with at least the columns `site_no`, `dateTime`,
#' `[PARAM]`, and `[PARAM]_fill`; expects the output of fill_ts_gaps
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values. In this workflow, this is likely `SpecCond`.
#' 
#' @return a tibble with the columns XXX

summarize_gap_fixes <- function(ts_filled_data, param_colname) {
  
  # How many site-years are now available?
  ts_filled_info <- ts_filled_data %>%
    mutate(year = year(dateTime)) %>%
    # Temporarily rename the data column so that this can handle any param
    rename_with(~gsub(param_colname, 'PARAM', .x)) %>% 
    group_by(site_no, year) %>%
    # Add columns that say whether or not NAs appear in the data for this
    # specific site-year. If there are NAs, that timeseries would be 
    # removed before getting used in the clustering algorithms.
    summarize(use_before = !any(is.na(PARAM)),
              use_after = !any(is.na(PARAM_fill)),
              .groups = 'keep') %>%
    ungroup()
  
  # How many site-year combos did we have before gap-filling?
  ts_usable_before <- ts_filled_info %>% filter(use_before) %>% nrow()
  # n=1600
  
  # How many were saved? Meaning the gap-filling removes
  # all NAs in that site-year timeseries so it can be used.
  ts_saved <- ts_filled_info %>% filter(!use_before & use_after) %>% nrow()
  # n=2137
  
  # How many couldn't be saved? There were still NAs after
  # gap-filling attempts (too many large gaps?)
  ts_still_unusable <- ts_filled_info %>% filter(!use_before & !use_after) %>% nrow()
  # n=5299
  
  # So how many site-years do we have to work with after gap-filling?
  ts_usable_now <- ts_filled_info %>% filter(use_after) %>% nrow()
  # n=3737
  
  tibble(status = c('All site-year ts in data',
                    'TS usable before gap-filling',
                    'TS usable thanks to gap-filling',
                    'TS still unusable',
                    'All site-year ts now usable'),
         num_ts = c(nrow(ts_filled_info),
                    ts_usable_before, 
                    ts_saved, 
                    ts_still_unusable, 
                    ts_usable_now))
}
