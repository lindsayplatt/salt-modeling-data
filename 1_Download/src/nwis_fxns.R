
#' @title Inventory NWIS for sites within a specific state matching certain criteria
#' @description Use the `dataRetrieval` function `whatNWISsites()` to
#' query the USGS National Water Information System (NWIS) and get a
#' list of sites that have data between the dates for the desired 
#' parameter code in the current state.
#' 
#' @param state_cd a two-digit character string with the state abbreviation; the 
#' function only expects one value to be passed in at a time
#' @param param_cd the USGS NWIS 5-digit parameter code passed in as a character string
#' @param start_date either character string or Date with the start of the window  
#' when sites need to have data; formatted as YYYY-MM-DD
#' @param end_date either character string or Date with the end of the window 
#' when sites need to have data; formatted as YYYY-MM-DD
#' 
#' @return a character vector of site numbers
#' 
inventory_nwis_sites_byState <- function(state_cd, param_cd, start_date, end_date) {
  whatNWISsites(stateCd = state_cd, 
                parameterCd = param_cd,
                startDate = start_date,
                endDate = end_date,
                siteType = 'ST',
                service = 'dv') %>% 
    # Remove any sites with ST-CA, ST-DCH, ST-TS
    filter(site_tp_cd == 'ST') %>% 
    # Return only the site numbers
    pull(site_no)
}

#' @title Check which sites in a list match additional criteria
#' @description Use the `dataRetrieval` function `whatNWISsites()` to
#' query the USGS National Water Information System (NWIS) with a given
#' list of site numbers to check which ones have a match for additional data
#' 
#' @param site_numbers a character vector of NWIS site numbers
#' @param param_cd the USGS NWIS 5-digit parameter code passed in as a character string
#' @param start_date either character string or Date with the start of the window  
#' when sites need to have data; formatted as YYYY-MM-DD
#' @param end_date either character string or Date with the end of the window 
#' when sites need to have data; formatted as YYYY-MM-DD
#' 
#' @return a character vector of site numbers
#' 
inventory_nwis_sites_bySite <- function(site_numbers, param_cd, start_date, end_date) {
  whatNWISsites(siteNumber = site_numbers, 
                parameterCd = param_cd,
                startDate = start_date,
                endDate = end_date,
                service = 'dv') %>% 
    # Return only the site numbers
    pull(site_no)
}
