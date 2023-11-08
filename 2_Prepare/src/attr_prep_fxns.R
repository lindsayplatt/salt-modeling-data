
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

#' @title Pivot downloaded NHD attributes from long to wide
#' @description The output from `download_nhdplus_attributes()` is in 
#' long format but we need each static attribute to be its own column
#' and to identify the site that matches the NHD COMID.
#' 
#' @param nhd_attribute_table a tibble of attribute values for each COMID with  
#' the columns `nhd_comid`, `nhd_attr_id`, `nhd_attr_val`, and `percent_nodata`
#' @param comid_site_xwalk a tibble with the columns `site_no`, `nhd_comid`, `with_retry`
#' 
#' @return tibble with two columns `site_no` and `attr_roadSalt`
#' 
prepare_nhd_attributes <- function(nhd_attribute_table, comid_site_xwalk) {
  
  nhd_attribute_table %>%
    # Pivot the NHD attributes to be columns
    pivot_wider(id_cols = nhd_comid, 
                names_from = nhd_attr_id, 
                values_from = nhd_attr_val) %>% 
    # Join *into* the site number by COMID
    right_join(comid_site_xwalk, by = 'nhd_comid') %>% 
    # Keep only the site_no and NHD attribute columns
    dplyr::select(site_no, everything(), 
                  -nhd_comid, -with_retry) %>% 
    # Rename the columns
    rename(attr_pctSnow = CAT_PRSNOW,
           attr_avgSnowDetrended = CAT_WBM_DT_SNW_2012,
           attr_avgPrecip = CAT_WBM_PPT,
           attr_avgRunoff = CAT_WBM_RUN,
           attr_avgSnow = CAT_WBM_SNW,
           attr_avgSoilStorage = CAT_WBM_STO,
           attr_baseFlowInd = CAT_BFI,
           attr_daysInSubsurface = CAT_CONTACT,
           attr_avgDepth2WT = CAT_EWT,
           attr_avgGWRecharge = CAT_RECHG,
           attr_topoWetInd = CAT_TWI,
           attr_numDams2013 = CAT_NDAMS2013,
           attr_pctHighDev = CAT_NLCD19_24,
           attr_pctLowDev = CAT_NLCD19_22,
           attr_vegIndSpring = CAT_EVI_AMJ_2012,
           attr_vegIndSummer = CAT_EVI_JAS_2012,
           attr_vegIndWinter = CAT_EVI_JFM_2012,
           attr_vegIndAutumn = CAT_EVI_OND_2011,
           attr_pctSoilSand = CAT_SANDAVE,
           attr_pctSoilSilt = CAT_SILTAVE,
           attr_pctSoilClay = CAT_CLAYAVE,
           attr_pctSoilOM = CAT_OM,
           attr_soilPerm = CAT_PERMAVE,
           attr_availWaterCap = CAT_AWCAVE,
           attr_meanSoilSalinity = CAT_SALINAVE,
           attr_avgBasinSlope = CAT_BASIN_SLOPE,
           attr_avgStreamSlope = CAT_STREAM_SLOPE,
           attr_roadStreamXings = CAT_RDX,
           attr_roadDensity = CAT_TOTAL_ROAD_DENS,
           attr_streamDensity = CAT_STRM_DENS)
  
}