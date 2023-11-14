
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
#' @return tibble with an additional column called `[PARAM]_adj`, containing the 
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
    mutate(PARAM_adj = ifelse(row_number() %in% ids_to_interp,
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
#' @return tibble with an additional column called `[PARAM]_adj`, containing the 
#' gap-filled values (may still have some NAs) and additional rows so that the 
#' data has one for each day between the min and max dates).
#' 
fill_ts_gaps <- function(ts_data, param_colname, max_gap_days) {
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

#' @title Summarize counts of pre- and post-gap filling.
#' @description Tally counts site-years that now qualify for dynamic time-warping
#' algorithms because there are no NAs. Includes counts of how many site-year
#' timeseries qualified before and after the gap-filling. This should have all
#' sites' data passed in at once.
#' 
#' @param ts_filled_data a tibble with at least the columns `site_no`, `dateTime`,
#' `[PARAM]`, and `[PARAM]_adj`; expects the output of fill_ts_gaps
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
              use_after = !any(is.na(PARAM_adj)),
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
