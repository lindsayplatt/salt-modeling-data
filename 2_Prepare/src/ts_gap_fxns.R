
#' @title Identify gaps that can be filled
#' @description Count the length of gaps by identifying how many NAs appear
#' multiple days in a row. Filter to only gaps that are below a max number of 
#' days and return a vector of indices that refer to the input vector. Note
#' that this assumes you've only passed in one site's data at a time.
#' 
#' @param data_vector vector of values where some might be NA
#' @param max_gap_days single numeric value indicating the maximum 
#' number of sequential values that can be NA
#' 
#' @return numeric vector of the indices that have passed the criteria for being 
#' gap-filled; indices correspond to the input vector, `data_vector`
#' 
identify_acceptable_gaps <- function(data_vector, max_gap_days) {
  
  # Count how many NA values appear sequentially
  acceptableNA_sequences <- accelerometry::rle2(
    x = is.na(data_vector), 
    indices = TRUE) %>% as_tibble() %>% 
    # value == 0 means "FALSE" to is.na(), keep those
    # value == 1 means "TRUE" to is.na(), only keep if they are < max gap allowed
    filter(value == 0 | value == 1 & length <= max_gap_days)
  
  # Use the `start` and `stop` fields returned by `rle2` to create
  # a vector of the indices.
  acceptable_ids <- apply(acceptableNA_sequences, 1, 
                          function(x) seq(x[['start']], x[['stop']])) %>% 
    reduce(c)
  
  return(acceptable_ids)
}

#' @title Linearly interpolate to fill data gaps
#' @description Apply linear interpolation to fill data gaps. Only fills gaps
#' in rows that correspond to the input parameter `ids_to_interp`. All other
#' gaps will remain NAs.
#' 
#' @param ts_data a tibble with at least the `[PARAM]` column and a row for each 
#' day; designed for use within `fill_ts_gaps()` 
#' @param ids_to_interp vector of row indices that can be filled, output 
#' of `identify_acceptable_gaps()`
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values. In this workflow, this is likely `SpecCond`.
#' 
#' @return tibble with an additional column called `[PARAM]_fill`, containing the 
#' gap-filled values (may still have some NAs)
#' 
interpolate_gaps <- function(ts_data, ids_to_interp, param_colname) {
  ts_data %>% 
    # Temporarily rename the data column so that this can handle any param
    rename_with(~gsub(param_colname, 'PARAM', .x)) %>% 
    # Linear interpolate with ALL dates and values available (doing ALL and 
    # THEN replacing with NA afterwards so that the interpolation
    # uses appropriate data points to fill).
    mutate(PARAM_interp = zoo::na.approx(PARAM, na.rm = FALSE)) %>% 
    # Now replace interpolated values with `NA` for any of those
    # rows where we think the gap is too big to accept the interpolation
    mutate(PARAM_fill = ifelse(row_number() %in% ids_to_interp,
                              PARAM_interp, NA)) %>% 
    dplyr::select(-PARAM_interp) %>% 
    ungroup() %>% 
    # Reverse the `PARAM` placeholder column names 
    rename_with(~gsub('PARAM', param_colname, .x))
}

#' @title Fill gaps in a timeseries
#' @description Identify, fill, and announce gap-filling. This is 
#' currently setup to use linear interpolation via `interpolate_gaps()` 
#' to fill gaps that meet gap-filling maximums, determined by 
#' `identify_acceptable_gaps()`. This assumes that the data passed in is for 
#' only one site.
#' 
#' @param ts_data a tibble with at least the columns `site_no`, `dateTime`, and `[PARAM]`
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values. In this workflow, this is likely `SpecCond`.
#' @param max_gap_days single numeric value indicating the maximum number of 
#' sequential values that can be NA; passed on to `identify_acceptable_gaps()`
#' 
#' @return tibble with an additional column called `[PARAM]_fill`, containing the 
#' gap-filled values (may still have some NAs) and additional rows so that the 
#' data has one for each day between the min and max dates).
#' 
fill_ts_gaps_linear <- function(ts_data, param_colname, max_gap_days) {
  # Start by creating a data frame with all possible days for each site
  ts_data_all_days <- tibble(site_no = unique(ts_data$site_no),
                             dateTime = seq(min(ts_data$dateTime), 
                                            max(ts_data$dateTime),
                                            by = 'days')) %>% 
    # Join in the real data
    left_join(ts_data, by = c('site_no', 'dateTime')) %>% 
    # Temporarily rename the data column so that this can handle any param
    rename_with(~gsub(param_colname, 'PARAM', .x)) 
  
  # Create a vector of the row indices that qualify to be filled
  row_ids_to_fill <- identify_acceptable_gaps(ts_data_all_days[['PARAM']], 
                                              max_gap_days = 5)
  
  ts_data_all_days %>% 
    # Reverse the temporary column name before passing to `interpolate_gaps()`
    # because it will also rename them temporarily.
    rename_with(~gsub('PARAM', param_colname, .x)) %>% 
    # Fill the gaps!
    interpolate_gaps(row_ids_to_fill, param_colname) 
}

#' @title Run WRTDS to generate a complete timeseries
#' @description Use the EGRET R package to run a Weighted Regression in Time,
#' Discharge, and Season (WRTDS) to generate a timeseries without any NA values.
#' The output of this will be used to fill gaps.
#' 
#' @param data_q a single site's flow data, needs at least the columns `dateTime`, 
#' `Flow`, and `Flow_cd` columns. The values in `Flow` are assumed to be in cubic
#' feet per second (cfs).
#' @param data_param a single site's water quality parameter of interest to be
#' regressed with WRTDS. It needs at least the columns `dateTime` and `[PARAM]`.
#' To use with EGRET's WRTDS functions, we assume that the values in `[PARAM]` 
#' represent daily means from a continuous dataset (not usually how WRTDS 
#' works). Due to the data we have to pass in, we fill in low and high daily
#' concentrations with the same value, equal to the daily mean (whatever is in
#' `[PARAM]` column. In addition, we assume there are no censored values. In 
#' this workflow, the data is likely specific conductance.
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values. In this workflow, this is likely `SpecCond`.
#' @param param_nwis_cd the 5-digit character string representing the NWIS code
#' for this particular parameter so that information in EGRET's WRTDS functions
#' can be gathered. In this workflow, it is likely '00095' for specific conductance.
#' 
#' @return a tibble with the columns `site_no`, `dateTime`, `Flow`, `[PARAM]_wrtds`
#' (WRTDS daily output), and `[PARAM]_wrtds_stderr` (the standard error associated  
#' with the WRTDS daily output). Note that this data will have daily values for the  
#' full flow record, which may be outside of the original`[PARAM]` time range.
#' 
apply_wrtds <- function(data_q, data_param, param_colname, param_nwis_cd) {
  
  site_number <- unique(data_q$site_no)
  
  # Mostly suppress the EGRET function messages with `verbose=FALSE` because 
  # they can be A LOT to print out as the pipeline is running. Instead, we 
  # should see this message and one when the EGRET fxns are complete.
  message(sprintf("Starting EGRET's WRTDS for %s (%s param pts for %s days)", 
                  site_number, nrow(data_param), 
                  max(data_param$dateTime) - min(data_param$dateTime)))
  
  # Prepare data for using WRTDS functions. Get the site information (yes, this
  # queries NWIS and should technically go in `1_Download` but it is super fast
  # so it was easier to just put it in here and duplicate some of that info).
  # Convert flow and water quality data into the correctly structured data sets
  wrtds_info <- readNWISInfo(site_number, param_nwis_cd, interactive = FALSE)
  wrtds_daily <- data_q %>% 
    # Filter the flow data to only be for dates within the time range of `data_param`,
    # so that we are not returning values for extensions of the input data.
    filter(dateTime >= min(data_param$dateTime),
           dateTime <= max(data_param$dateTime)) %>% 
    rename(value = Flow, code = Flow_cd) %>% 
    populateDaily(qConvert = 35.314667, # Convert ft3/s to m3/s
                  verbose = FALSE)
  wrtds_sample <- data_param %>% 
    # For at least one site (01104430) the SC record was one day longer
    # than the Q record. This can cause issues with the EGRET WRTDS methods
    # converging on a solution. In these instances, filter to the time range
    # of the `wrtds_daily` data frame (this shouldn't do anything for sites
    # whose Q data is longer than param data because `wrtds_daily` was already
    # subset to match the param time range).
    filter(dateTime >= min(wrtds_daily$Date),
           dateTime <= max(wrtds_daily$Date)) %>% 
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
  
  wrtds_data %>% 
    left_join(ts_data, by = c('site_no', "dateTime")) %>% 
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
