
#' @title Calculate static mean Q for each site
#' @description Using the daily timeseries data for streamflow, calculate a single
#' mean value per site to serve as a static attribute.
#' 
#' @param data_q a tibble containing all daily flow records with at least the 
#' columns `site_no`, `dateTime`, and `Flow`.
#' 
#' @return a feather file containing only one value for each site; it should have 
#' the columns `site_no` and `attr_meanFlow`.
#'
calculate_mean_q_per_site <- function(data_q) {
  data_q %>% 
    group_by(site_no) %>% 
    summarize(attr_meanFlow = mean(Flow, na.rm = TRUE))
}

#' @title Z-normalize a set of values
#' @description Find the z-scores for a set of values. This assumes that the
#' values passed in are already grouped appropriately prior to this function
#' being used. Courtesy of https://jmotif.github.io/sax-vsm_site/morea/algorithm/znorm.html
#' 
#' @param ts a vector of numeric values
#' 
#' @return a vector the same length as `ts` but with z-scored values
#' 
znorm <- function(ts){
  ts.mean <- mean(ts)
  ts.dev <- sd(ts)
  (ts - ts.mean)/ts.dev
}

#' @title Aggregate road salt application values per polygon
#' @description Extract and sum cell values from the road salt raster file
#' to get a single road salt application rate value for each polygon.
#' 
#' @param road_salt_tif filepath to the road salt tif file
#' @param polys_sf a spatial data frame with polygons. Needs to be an `sf` class.
#' 
#' @returns a tibble with the columns `nhd_comid` and `attr_roadSalt` with the
#' total road salt per COMID catchment polygon
#' 
aggregate_road_salt_per_poly <- function(road_salt_tif, polys_sf) {
  
  # Load and reproject the road salt raster data
  road_salt_rast <- raster::raster(road_salt_tif)
  road_salt_rast_proj <- raster::projectRaster(road_salt_rast, 
                                               crs = st_crs(polys_sf)$input)
  
  # Now calculate the total amount of salt within each polygon. Note that for
  # this function, sum = "the sum of non-NA raster cell values, multiplied by 
  # the fraction of the cell that is covered by the polygon".
  salt_per_poly <- exactextractr::exact_extract(road_salt_rast_proj, polys_sf, 'sum')
  
  # Now add to a table for export
  road_salt_poly <- polys_sf %>% 
    mutate(attr_roadSalt = salt_per_poly) %>% 
    st_drop_geometry() %>% 
    as_tibble() %>% 
    dplyr::select(nhd_comid, attr_roadSalt)
  
  return(road_salt_poly)
}

#' @title Get road salt value per site
#' @description Using road salt values per NHD+ COMID catchment, map road salt to
#' NWIS sites based on the crosswalk from COMID to site.
#' 
#' @param road_salt_comid a tibble with at least the columns `nhd_comid` and
#' `attr_roadSalt`. Note that as described in `extract_nhdplus_geopackage_layer()`,
#' not all COMIDs had a catchment polygon that were downloaded (e.g. COMID `4672393`
#' did not have a catchment polygon available) and therefore will not have a salt value. 
#' @param comid_site_xwalk a tibble with at least the columns `site_no` and 
#' `nhd_comid`. Note that not all sites are mapped to a COMID and may be NA.
#' @param comid_upstream_tbl a tibble with the columns `nhd_comid` and `nhd_comid_upstream`
#' mapping all upstream comids to each COMID with an NWIS site. It should match
#' the output of `identify_upstream_comids()`.
#' 
#' @return tibble with three columns `site_no`, `attr_roadSalt`, and `attr_roadSaltCumulative`
#' 
map_catchment_roadSalt_to_site <- function(road_salt_comid, comid_site_xwalk, comid_upstream_tbl) {
  
  # Calculate cumulative road salt for each catchment and all upstream catchments
  upstream_roadSalt <- comid_upstream_tbl %>% 
    left_join(road_salt_comid, by = c('nhd_comid_upstream' = 'nhd_comid')) %>% 
    group_by(nhd_comid) %>% 
    summarize(attr_roadSaltCumulative = sum(attr_roadSalt), .groups='keep')
  
  comid_site_xwalk %>% 
    # Join in the road salt for just the one catchment
    left_join(road_salt_comid, by = 'nhd_comid') %>%
    as_tibble() %>%  
    # Now join the upstream road salt for each site's COMID
    left_join(upstream_roadSalt, by = 'nhd_comid') %>% 
    dplyr::select(site_no, attr_roadSalt, attr_roadSaltCumulative)
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
#' @return tibble with the columns `site_no` and any number of columns containing
#' NHD+ static catchment attributes, most prefixed with `attr_[attribute name]`. Any
#' column that is an NHD+ attribute but not prefixed `attr_` is not included in the 
#' function renaming step. You could add it, or leave it as-is. 
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
    rename(any_of(c(
      attr_pctSnow = 'CAT_PRSNOW',
      attr_avgSnowDetrended = 'CAT_WBM_DT_SNW_2012',
      attr_avgPrecip = 'CAT_WBM_PPT',
      attr_avgRunoff = 'CAT_WBM_RUN',
      attr_avgSnow = 'CAT_WBM_SNW',
      attr_avgSoilStorage = 'CAT_WBM_STO',
      attr_baseFlowInd = 'CAT_BFI',
      attr_daysInSubsurface = 'CAT_CONTACT',
      attr_avgDepth2WT = 'CAT_EWT',
      attr_avgGWRecharge = 'CAT_RECHG',
      attr_topoWetInd = 'CAT_TWI',
      attr_numDams2013 = 'CAT_NDAMS2013',
      attr_pctHighDev = 'CAT_NLCD19_24',
      attr_pctLowDev = 'CAT_NLCD19_22',
      attr_vegIndSpring = 'CAT_EVI_AMJ_2012',
      attr_vegIndSummer = 'CAT_EVI_JAS_2012',
      attr_vegIndWinter = 'CAT_EVI_JFM_2012',
      attr_vegIndAutumn = 'CAT_EVI_OND_2011',
      attr_pctSoilSand = 'CAT_SANDAVE',
      attr_pctSoilSilt = 'CAT_SILTAVE',
      attr_pctSoilClay = 'CAT_CLAYAVE',
      attr_pctSoilOM = 'CAT_OM',
      attr_soilPerm = 'CAT_PERMAVE',
      attr_availWaterCap = 'CAT_AWCAVE',
      attr_meanSoilSalinity = 'CAT_SALINAVE',
      attr_avgBasinSlope = 'CAT_BASIN_SLOPE',
      attr_avgStreamSlope = 'CAT_STREAM_SLOPE',
      attr_roadStreamXings = 'CAT_RDX',
      attr_roadDensity = 'CAT_TOTAL_ROAD_DENS',
      attr_streamDensity = 'CAT_STRM_DENS')))
  
}
