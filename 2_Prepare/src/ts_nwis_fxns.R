
#' @title Calculate daily means from instantaneous data
#' @description Using the downloaded NWIS timeseries data, this function will
#' collapse instantaneous records to daily means. It assumes that you have 
#' passed in instantaneous data only.
#' 
#' @param out_file a character string indicating a file path to save a feather 
#' file of the daily means
#' @param in_file a character string indicating the file path containing 
#' instantaneous records with at least the columns `site_no`, `dateTime`,
#' `[PARAM]_Inst`, and `[PARAM]_Inst_cd`.
#' @param site_tz_xwalk a tibble with the timezone of each NWIS site so that date
#' times can be converted into days appropriately. Should have the columns `site_no`
#' and `tz_cd`.
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values and value codes. In this workflow, this is likely either
#' `SpecCond` OR `Flow`. No need for `_Inst` here as the code automatically adds that.
#' 
#' @return a feather file containing only daily values for each site; it should have 
#' the columns `site_no`, `dateTime`, `[PARAM]`, and `[PARAM]_cd`.
#'
calculate_dv_from_uv <- function(out_file, in_file, site_tz_xwalk, param_colname) {
  
  data_in <- read_nwis_file(in_file, param_colname)
  
  # Stop now if the data passed in does not represent instantaneous data
  stopifnot(any(grepl('_Inst', names(data_in))))
  
  data_in %>%
    # Rename the data column so that the following code can handle
    # either SC data or Q data. The name will be reinstated at the end.
    rename_with(~gsub(param_colname, 'PARAM', .x)) %>% 
    # Create a column representing the day without the time
    # TODO: use the appropriate timezone for this site
    mutate(dateTime_w_time = dateTime,
           dateTime = as.Date(dateTime)) %>% 
    group_by(site_no, dateTime) %>%
    summarize(PARAM = mean(PARAM_Inst, na.rm=TRUE),
              PARAM_cd = paste(unique(PARAM_Inst_cd), collapse=';'),
              .groups = 'drop') %>%
    # Replace the `PARAM` placeholder column names with the appropriate
    rename_with(~gsub('PARAM', param_colname, .x)) %>%
    # Save the data as a file
    write_feather(out_file)
  
  return(out_file)
}

#' @title Combine all downloaded and calculated daily means
#' @description Using the downloaded NWIS daily data and the output from 
#' `calculate_dv_from_uv()`, create a single timeseries dataset of daily means.
#' 
#' @param out_file a character string indicating a file path to save a feather 
#' file of all the daily means
#' @param in_files vector of feather filepaths of the data to combines; assumes  
#' these have only the following columns: `site_no`, `dateTime`, `[PARAM]`, and `[PARAM]_cd`.
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values and value codes. In this workflow, this is likely either
#' `SpecCond` OR `Flow`. No need for `_Inst` here as the code automatically adds that.
#' 
#' @return a feather file containing all daily value timeseries data; it should have 
#' the columns `site_no`, `dateTime`, `[PARAM]`, and `[PARAM]_cd`.
#'
combine_all_dv_data <- function(out_file, in_files, param_colname) {
  
  # Map over each of the files and read into memory
  in_files %>% 
    map(read_nwis_file, param_colname = param_colname) %>% 
    # Combine the list of loaded tables into a single table
    bind_rows() %>% 
    # Arrange so that site's data are together and ordered chronologically
    arrange(site_no, dateTime) %>% 
    # Save the data as a file
    write_feather(out_file)
  
  return(out_file)
}

#' @title Read in an NWIS file and keep only the standard columns
#' @description Some sites return multiple data streams for the same parameter, 
#' usually indicating a different sensor. Sometimes the sensors are just replacements
#' but sometimes they are positioned at different locations (e.g. bottom, top, left bank,
#' right bank, etc). There is nuance to which to choose so this helps retain the 
#' appropriate data streams for each site in a single column.
#' 
#' @param in_file a single feather filepath for the data to be read in. Should have
#' at least the columns `site_no`, `dateTime`, `[PARAM]`, and `[PARAM]_cd` but may
#' have more than that.
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values and value codes. In this workflow, this is likely either
#' `SpecCond` OR `Flow`. No need for `_Inst` here as the code automatically adds that.
#' 
#' @return a table of NWIS timeseries data with only the columns `site_no`, `dateTime`, 
#' `[PARAM]`, and `[PARAM]_cd`
#' 
read_nwis_file <- function(in_file, param_colname) {
  
  # Only accepts columns named `[PARAM]`, `[PARAM]_cd`, `[PARAM]_Inst`, or `[PARAM]_Inst_cd`
  standard_colname_regex <- sprintf('site_no|dateTime|^%s(_Inst)?$|^%s(_Inst)?_cd$', 
                                    param_colname, param_colname)
  
  data_in <- read_feather(in_file)
  
  # If there are more than the expected columns (meaning additional data stream
  # columns for the same site & parameter),choose which ones to use. 
  # For now, just selecting the standard columns and ignoring the other. This
  # will result in more NAs.
  if(!all(grepl(standard_colname_regex, names(data_in)))) {
    # TODO: Choose the appropriate sensor column for each site and date
    
    # E.g. for `01463500` before 1995-09-30, the data is stored in a different column
    # x <- dataRetrieval::readNWISdv(siteNumber = '01463500', startDate = '1968-06-25', parameterCd = '00095') %>%
    #   renameNWISColumns() %>% 
    #   mutate(SC = ifelse(!is.na(SpecCond), SpecCond, `..2.._SpecCond`))
    # plot(x$dateTime, x$SC)
    
    # For now, just record which files this occurs in.
    message('Found a file with more than `SpecCond` datastream: ', in_file)
  }
  
  data_in %>% 
    # Select only the standard columns to return
    select(matches(standard_colname_regex)) %>% 
    # Filter out the NAs resulting from removing/replacing the non-standard 
    # columns (not including `[PARAM]_cd` NAs)
    filter(if_all(-ends_with('_cd'), ~!is.na(.)))
 
}
