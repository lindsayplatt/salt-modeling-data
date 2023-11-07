
#' @title Calculate static mean Q for each site
#' @description Using the daily timeseries data for streamflow, calculate a single
#' mean value per site to serve as a static attribute.
#' 
#' @param in_file a character string indicating the file path containing all
#' daily Q records with at least the columns `site_no`, `dateTime`, and `Flow`.
#' 
#' @return a feather file containing only one value for each site; it should have 
#' the columns `site_no` and `attr_meanFlow`.
#'
calculate_mean_q_per_site <- function(in_file) {
  read_feather(in_file) %>% 
    group_by(site_no) %>% 
    summarize(attr_meanFlow = mean(Flow, na.rm = TRUE))
}

#' @title Aggregate road salt application values per site
#' @description Extract and sum cell values from the road salt raster file
#' to get a single road salt application rate value for each site.
#' 
#' @param road_salt_tif filepath to the road salt tif file
#' @param sites_sf a spatial data frame with locations for NWIS sites. Needs
#' at least a `site_no` and `geometry` column.
#' 
#' @return tibble with two columns `site_no` and `attr_roadSalt`
#' 
aggregate_road_salt_per_site <- function(road_salt_tif, sites_sf) {
  
  # Load and reproject the road salt raster data
  road_salt_rast <- raster::raster(road_salt_tif)
  road_salt_rast_proj <- raster::projectRaster(road_salt_rast, 
                                               crs = st_crs(sites_sf)$input)
  
  road_salt_per_site <- road_salt_rast_proj %>% 
    # Extract the cells within a 5 km radius of each site
    # TODO: for some reason, this seems to currently only include cells above
    # or below the point within the buffer provided. I expected to get some in
    # the horizontal direction, too. This will require some more work. Might
    # be able to use COMID reaches to get these values as well?
    raster::extract(st_coordinates(sites_sf), 
                    buffer=5000, # In meters, but only seems to go vertically
                    df=TRUE) %>% 
    as_tibble() %>% 
    # Add the cells around a site together to get one value of 
    # road salt applied per site.
    group_by(ID) %>%
    summarize(attr_roadSalt = sum(road_salt_2015)) %>% 
    # Add site_no as a column
    mutate(site_no = sites_sf$site_no) %>% 
    dplyr::select(site_no, attr_roadSalt)
  
  return(road_salt_per_site)
}
