
#' @title Finalize the tibble for modeling
#' @description Add the year column and keep only one parameter column for 
#' modeling. This also prints a summary count for the number of sites and the 
#' number of site-years (or time series) that can be used for modeling.
#' 
#' @param ts_data a tibble with at least the columns `site_no`, `dateTime`, and 
#' `[param_colname][model_colsuffix]`, e.g. (`SpecCond_detrend`).
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values. In this workflow, this is likely `SpecCond`.
#' @param model_colsuffix a character string indicating the suffix used in the  
#' column name along with `param_colname` to identify which data should be used
#' for modeling. In this workflow, this is likely `_detrend`.
#' 
#' @return tibble with all columns related to the parameter and modeling: 
#' `site_no`, `year`, `date`, and `[PARAM]`.
#' 
finalize_ts_modeling <- function(ts_data, param_colname, model_colsuffix) {
  
  ts_data_modeling <- ts_data %>% 
    # Temporarily rename the data column so that this can handle any param
    rename_with(~gsub(param_colname, 'PARAM', .x)) %>% 
    mutate(year = year(dateTime)) %>% 
    dplyr::select(site_no, 
                  year, 
                  date = dateTime,
                  PARAM = !!as.name(sprintf('PARAM%s', model_colsuffix))) %>% 
    # Reverse the `PARAM` placeholder column names 
    rename_with(~gsub('PARAM', param_colname, .x))
  
  nsiteyears <- ts_data_modeling %>% 
    dplyr::select(site_no, year) %>% 
    distinct() %>% nrow()
  nsites <- length(unique(ts_data_modeling$site_no))
  
  message(sprintf('The final time series for modeling has %s sites and %s site-years.',
                  nsites, nsiteyears))
  
  return(ts_data_modeling)
}
