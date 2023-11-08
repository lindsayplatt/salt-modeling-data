
#' @title Convert NWIS sites into spatial features
#' @description Download NWIS site information (lat/long) and use that
#' to create a spatial features object with the sites as points. This
#' is used to query NHDPlus and match sites to their closest COMID. It
#' could also be used for creating maps.
#' 
#' @param site_metadata a tibble containing at least the columns `site_no`, 
#' `dec_lat_va`, `dec_long_va`, and `datum`, and `tz_cd`
#' 
#' @return an `sf` object with site locations as individual points
#' 
fetch_site_locations <- function(site_metadata) {
  
  # Crosswalk the site datums to EPSG codes. This crosswalk was borrowed
  # from Lauren Koenig's code at https://github.com/DOI-USGS/ds-pipelines-targets-example-wqp/
  # for the `create_site_bbox()` function. Assumes 4326 when none are provided
  datum2epsg <- function(datum) {
    case_when(datum == "NAD83" ~ 4269,
              datum == "WGS84" ~ 4326,
              datum == "NAD27" ~ 4267,
              .default = 4326)
  }
  
  # Transform sites into spatial features objects using the appropriate EPSG code
  # based on the datum. Tranform to the 4326 EPSG before binding together into a single sf
  sites_sf <- site_metadata %>% 
    split(.$datum) %>% 
    map(~{
      epsg_in <- datum2epsg(unique(.x$datum))
      .x %>% 
        st_as_sf(coords = c('dec_long_va', 'dec_lat_va'), crs = epsg_in) %>% 
        st_transform(crs = 4326) %>%
        select(-datum)
    }) %>% bind_rows()
  
  return(sites_sf)
}

#' @title Look up COMID closest to spatial point
#' @description For each site in the `sf` object passed into this function,
#' NHD+ is queried for the closest COMID. Sometimes it is not able to identify 
#' a reach (either too far from an NHD+ reach, or in a tidal location). In these
#' instances, a larger search radius is attempted If that does not return 
#' anything, then an NA is returned for COMID.
#' 
#' @param sites_sf a spatial data frame with locations for NWIS sites. Needs
#' at least a `site_no` and `geometry` column.
#' 
#' @return a tibble with the columns `site_no`, `nhd_comid`, `with_retry`
#' 
identify_site_comids <- function(sites_sf) {
  sites_sf %>% 
    split(.$site_no) %>% 
    map(safely_get_flowline_index) %>% 
    bind_rows(.id = 'site_no') %>%
    select(site_no, nhd_comid = COMID, with_retry)
}

#' @title Wrap `nhdplusTools::get_flow_index()` to catch errors
#' @description If there is no match, this function first tries again
#' with a slightly bigger radius. If there is still nothing found, then
#' an NA is returned for `COMID`
#' 
#' @param site_sf an `sf` object with a single row for one location
#' @param max_radius the size of the radius in degrees to try when the initial 
#' query of 0.01 degrees does not return any COMID. Defaults to 0.02 degrees.
#' 
#' @return a tibble with a single row and the columns `COMID` and `with_retry`
#' which is TRUE/FALSE indicating whether the COMID was found in the first 
#' attempt or a subsequent attempt with a bigger search radius.
#' 
safely_get_flowline_index <- function(site_sf, max_radius = 0.02) {
  comid_out <- suppressMessages(
    tryCatch(get_flowline_index(points = site_sf, 
                                flines = "download_nhdplusv2") %>% 
               mutate(with_retry = FALSE),
             # If it fails, it returns a weird error about st_transform() but the
             # warning that is returned mentions `no nhd features found`, so the
             # retry will be based on that. The error that pops up:
             # Error in UseMethod("st_transform") : 
             #  no applicable method for 'st_transform' applied to an object of class "list"
             warning = function(w) {
               if(grepl('No nhd features found', w)) {
                 # If it doesn't work, try again with 0.05 degrees radius
                 tryCatch(get_flowline_index(
                   points = site_sf, 
                   search_radius =  units::set_units(max_radius, "degrees"),
                   max_matches = 1,
                   flines = "download_nhdplusv2"
                 ) %>% mutate(with_retry = TRUE),
                 warning = function(w) {
                   # And if the bigger radius still doesn't
                   # find anything, an NA should be returned
                   if(grepl('No nhd features found', w)) { 
                     return(tibble(COMID = NA, with_retry = TRUE))
                   }})}})) 
  
  # Sometimes a failed attempt will just return empty rows instead of throwing 
  # an error so they skip the retry. If it returns empty rows, try again!
  if(is.null(comid_out)) {
    comid_out <- suppressMessages(tryCatch(
      get_flowline_index(points = site_sf, 
                         search_radius =  units::set_units(max_radius, "degrees"),
                         max_matches = 1,
                         flines = "download_nhdplusv2"
      ) %>% mutate(with_retry = TRUE),
      warning = function(w) {
        # And if the bigger radius still doesn't
      # find anything, an NA should be returned
      if(grepl('No nhd features found', w)) { 
        return(tibble(COMID = NA, with_retry = TRUE))
      }}))
  }
  
  # If the second attempt once again returns empty rows and doesn't error, then we 
  # need to prepare an output with NA manually. Example: site_no = '295501090190400'
  if(is.null(comid_out)) {
    comid_out <- tibble(COMID = NA, with_retry = TRUE)
  }
  
  # Return with just these two columns:
  select(comid_out, COMID, with_retry)
}

#' @title Download NHD+ attributes
#' @description Use `nhdplusTools` functions to download attributes by COMID
#' from the over 14k attributes the published in Wieczorek et al., 2018 (ver 
#' 3.0, January 2021).
#' 
#' @param attributes character vector of attribute IDs. To see acceptable values
#' for these attributes, see the `ID` column from `get_characteristics_metadata()`
#' @param comids character vector of NHD COMIDs to retrieve attribute values
#' 
#' @return a tibble of attribute values for each COMID with the columns `nhd_comid`,
#' `nhd_attr_id`, `nhd_attr_val`, and `percent_nodata`
#' 
download_nhdplus_attributes <- function(attributes, comids) {
  
  # Get around the issue I logged at https://github.com/DOI-USGS/nhdplusTools/issues/365
  nhdplusTools::nhdplusTools_data_dir(tools::R_user_dir("nhdplusTools"))
  
  # Download the attribute values for the given attributes and comids
  get_catchment_characteristics(varname = attributes, ids = comids) %>% 
    select(nhd_comid = comid,
           nhd_attr_id = characteristic_id,
           nhd_attr_val = characteristic_value,
           percent_nodata)
}

#' @title Load the list of NHDPlus attributes
#' @description Load the input file of NHDPlus attributes to fetch
#' for each of the COMIDs. Keeps them in a list format so that they
#' can be mapped over during the download step. This also removes
#' any of the list elements that don't have any attributes.
#' 
#' @param in_file the filepath to the yaml containing the NHDPlus
#' attributes, formatted as seperate list elements for each theme
#' with attribute ids making up each list's content. 
#' 
#' @return a list object with vectors of character strings for each theme to use
#' as input to the argument `varnames` when downloading static attributes with
#' `nhdplusTools::get_catchment_characteristics()`
#' 
load_nhdplus_attribute_list <- function(in_file) {
  yaml::yaml.load_file(in_file) %>% 
    discard(is.null)
} 
