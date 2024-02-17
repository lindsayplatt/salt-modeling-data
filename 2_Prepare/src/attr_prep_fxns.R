
#' @title Calculate static Q metrics for each site
#' @description Using the daily timeseries data for streamflow, calculate a single
#' value per site to serve as a static attribute, including median flow, high and
#' low percentiles, and flow based on specific seasons.
#' 
#' @param data_q a tibble containing all daily flow records with at least the 
#' columns `site_no`, `dateTime`, and `Flow`.
#' 
#' @return a feather file containing only one row for each site; it should have 
#' the columns `site_no` and any number of columns with the naming pattern 
#' `attr_[metric]Flow`.
#'
calculate_q_stats_per_site <- function(data_q) {
  
  overall_q <- data_q %>% 
    group_by(site_no) %>% 
    summarize(attr_medianFlow = median(Flow, na.rm = TRUE),
              attr_p05Flow = quantile(Flow, na.rm = TRUE, probs = 0.05, names=F),
              attr_p95Flow = quantile(Flow, na.rm = TRUE, probs = 0.95, names=F))
  
  seasonal_q <- data_q %>% 
    mutate(isWinter = month(dateTime) %in% c(12, 1, 2, 3)) %>% 
    group_by(site_no) %>% 
    summarize(attr_medianNonWinterFlow = median(Flow[!isWinter], na.rm = TRUE),
              attr_medianWinterFlow = median(Flow[isWinter], na.rm = TRUE),
              attr_p05NonWinterFlow = quantile(Flow[!isWinter], na.rm = TRUE, probs = 0.05, names=F),
              attr_p95NonWinterFlow = quantile(Flow[!isWinter], na.rm = TRUE, probs = 0.95, names=F),
              attr_p05WinterFlow = quantile(Flow[isWinter], na.rm = TRUE, probs = 0.05, names=F),
              attr_p95WinterFlow = quantile(Flow[isWinter], na.rm = TRUE, probs = 0.95, names=F))
  
  left_join(overall_q, seasonal_q, by = 'site_no')
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

#' @title Get area of catchment and watershed (cumulative catchments)
#' @description Using the column of polygon area per catchment returned by 
#' NHD+ and saved as `areasqkm`, this function calculates the area per
#' individual catchment and the total cumulative area for catchments upstream
#' of the featured COMID (including that COMIDs area). This is used downstream
#' as an attribute but also to calculate road salt application per sq km.
#' 
#' @param polys_sf a spatial data frame with polygons. Needs to be an `sf` 
#' class. Should have at least the columns `nhd_comid` and `areasqkm`.
#' @param comid_upstream_tbl a tibble with the columns `nhd_comid` and `nhd_comid_upstream`
#' mapping all upstream comids to each COMID with an NWIS site. It should match
#' the output of `identify_upstream_comids()`.
#' @param comid_site_xwalk a tibble with at least the columns `site_no` and 
#' `nhd_comid`. Note that not all sites are mapped to a COMID and may be NA.
#' 
#' @return tibble with four columns `site_no`, `attr_areaSqKm`,
#'  `attr_areaCumulativeSqKm`, `attr_areaRatio`, and `numNACatchments`. The ratio
#'  shows how big this catchment is compared to its full watershed. Values near 
#'  1 indicate that this catchment is either a headwaters catchment -OR- we are
#'  currently missing data for the upstream catchments.
#' 
calculate_catchment_areas <- function(polys_sf, comid_upstream_tbl, comid_site_xwalk) {
  # Using area that is given already - I checked and they are very similar 
  # to calculating the area using `st_area()`.
  catchment_area_tbl <- polys_sf %>% 
    st_drop_geometry() %>% 
    select(nhd_comid_upstream = nhd_comid, area_sqkm = areasqkm)
  
  comid_upstream_tbl %>% 
    left_join(catchment_area_tbl, by = 'nhd_comid_upstream') %>% 
    # Per NHD COMID, calculate the area of *just* that catchment and
    # the area of all upstream catchments. 
    group_by(nhd_comid) %>% 
    summarize(attr_areaSqKm = area_sqkm[nhd_comid_upstream == nhd_comid],
              attr_areaCumulativeSqKm = sum(area_sqkm, na.rm=TRUE),
              attr_areaRatio = attr_areaSqKm / attr_areaCumulativeSqKm,
              numNACatchments = sum(is.na(area_sqkm))) %>% 
    ungroup() %>% 
    # Map these attributes from NHD COMIDs to NWIS sites
    right_join(comid_site_xwalk, by = 'nhd_comid') %>%
    select(site_no, attr_areaSqKm, attr_areaCumulativeSqKm, attr_areaRatio, numNACatchments)
}

#' @title Aggregate road salt application values per polygon
#' @description Extract and sum cell values from the road salt raster file
#' to get a single road salt application rate value for each polygon.
#' 
#' @param road_salt_tif filepath to the road salt tif file
#' @param polys_sf a spatial data frame with polygons. Needs to be an `sf` class.
#' 
#' @returns a tibble with the columns `nhd_comid` and `road_salt_lbs` with the
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
    mutate(road_salt_lbs = salt_per_poly) %>% 
    st_drop_geometry() %>% 
    as_tibble() %>% 
    dplyr::select(nhd_comid, road_salt_lbs)
  
  return(road_salt_poly)
}

#' @title Get road salt value per site
#' @description Using road salt values per NHD+ COMID catchment, map road salt to
#' NWIS sites based on the crosswalk from COMID to site.
#' 
#' @param road_salt_comid a tibble with at least the columns `nhd_comid` and
#' `road_salt_lbs`. Note that as described in `extract_nhdplus_geopackage_layer()`,
#' not all COMIDs had a catchment polygon that were downloaded (e.g. COMID `4672393`
#' did not have a catchment polygon available) and therefore will not have a salt value.
#' @param basin_areas a tibble with at least the columns `site_no`, `attr_areaSqKm`,
#' and `attr_areaCumulativeSqKm` representing catchment and upstream basin total area.
#' @param comid_site_xwalk a tibble with at least the columns `site_no` and 
#' `nhd_comid`. Note that not all sites are mapped to a COMID and may be NA.
#' @param comid_upstream_tbl a tibble with the columns `nhd_comid` and `nhd_comid_upstream`
#' mapping all upstream comids to each COMID with an NWIS site. It should match
#' the output of `identify_upstream_comids()`.
#' 
#' @return tibble with three columns `site_no`, `attr_roadSaltPerSqKm`,
#'  `attr_roadSaltCumulativePerSqKm`, and `attr_roadSaltRatio` (the ratio of 
#'  salt per sq km applied in the catchment vs all upstream catchments; values
#'  larger than 1 mean more salt/km2 applied in this catchment compared to the
#'  rest of the upstream watershed, and values less than one mean less salt/km2
#'  applied in this catchment compared to its upstream watershed).
#' 
map_catchment_roadSalt_to_site <- function(road_salt_comid, basin_areas, comid_site_xwalk, comid_upstream_tbl) {
  
  # Calculate cumulative road salt for each catchment and all upstream catchments
  comid_roadSalt <- comid_upstream_tbl %>% 
    left_join(road_salt_comid, by = c('nhd_comid_upstream' = 'nhd_comid')) %>% 
    group_by(nhd_comid) %>% 
    summarize(roadSalt = road_salt_lbs[nhd_comid_upstream == nhd_comid],
              roadSaltCumulative = sum(road_salt_lbs, na.rm=TRUE),
              .groups='keep')

  comid_site_xwalk %>% 
    # Now join the road salt data per catchment & by upstream catchment
    left_join(comid_roadSalt, by = 'nhd_comid') %>% 
    # Now do the same but for catchment area 
    left_join(basin_areas, by = 'site_no') %>% 
    # Now calculate road salt per area
    mutate(attr_roadSaltPerSqKm = roadSalt / attr_areaSqKm,
           attr_roadSaltCumulativePerSqKm = roadSaltCumulative / attr_areaCumulativeSqKm,
           attr_roadSaltRatio = attr_roadSaltPerSqKm / attr_roadSaltCumulativePerSqKm) %>% 
    dplyr::select(site_no, attr_roadSaltPerSqKm, attr_roadSaltCumulativePerSqKm, attr_roadSaltRatio)
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

# TODO: DOCUMENTATION IF WE USE THIS
prepare_sb_gw_attrs <- function(transmissivity_csv, depth2wt_csv, comid_site_xwalk) {
  
  trnmsv <- read_csv(transmissivity_csv, show_col_types = FALSE) %>% 
    mutate(nhd_comid = comid) %>% 
    select(nhd_comid, attr_transmissivity = trans250)
  
  dtw <- read_csv(depth2wt_csv, show_col_types = FALSE) %>% 
    mutate(nhd_comid = comid) %>% 
    select(nhd_comid, attr_zellSanfordDepthToWT = dtw250)
  
  comid_site_xwalk %>% 
    left_join(trnmsv, by = 'nhd_comid') %>% 
    left_join(dtw, by = 'nhd_comid') %>% 
    select(site_no, starts_with('attr_'))
}
