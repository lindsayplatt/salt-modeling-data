
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

#' @title Calculate a trend in SC per site
#' @description For each site in the data, apply different trend algorithms to 
#' categorize that site's SC data as trending 'positive', 'negative', or 'none'.
#' 
#' @param in_file a character string indicating the file path containing all
#' daily SC records with at least the columns `site_no`, `dateTime`, and `SpecCond`.
#' @param max_pval numeric value indicating the maximum p-value that is allowed
#' to declare a trend significant. Passed on to `extract_mk_trend()`. 
#' 
calculate_sc_trend <- function(in_file, max_pval = 0.05) {
  # TODO: which of these trend tests should we use?
  read_feather(in_file) %>% 
    split(.$site_no) %>% 
    map(~{tibble(
      attr_TrendMannKendall = apply_MannKendall(.x, max_pval = 0.05),
      attr_TrendMannKendall_DS = apply_MannKendall(.x, max_pval = 0.05, deseasonalize = TRUE),
      attr_TrendSeasonalMK = apply_SeasonalMannKendall(.x, max_pval = 0.05)
    )}) %>% 
    bind_rows(.id = 'site_no')
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

#' @title Find a trend in SC using a Mann-Kendall test
#' @description Calculate a trend for SC data using `Kendall::MannKendall()`
#' 
#' @param ts_data a tibble of SC timeseries data for a single site with at least
#' the columns `dateTime` and `SpecCond`.
#' @param max_pval numeric value indicating the maximum p-value that is
#' allowed to declare a trend significant. Passed on to `extract_mk_trend()`
#' @param deseasonalize logical flag indicating whether the data should be
#' "desseasonalized" (aka z-scored) prior to a trend being calculated. TBD
#' on whether this approach will remain. Defaults to FALSE.
#' 
#' @return a character string indicating the trend as "none", "positive", or "negative"
#' 
apply_MannKendall <- function(ts_data, max_pval, deseasonalize=FALSE) {
  
  # Note that you do not need to fill in NAs for missing dates.
  # It does not change the trend results for the `MannKendall()` function.
  
  # Convert the data into a `ts` object to pass to `MannKendall()`
  if(deseasonalize) {
    # TODO: should we "de-seasonalize"? (AKA z-score) to calculate 
    # a trend when using MannKendall?
    ts_data_ds <- ts_data %>% 
      # TODO: Should it be done by day? By month?
      mutate(doy = lubridate::yday(dateTime)) %>% 
      group_by(doy) %>% 
      mutate(SpecCond_ds = znorm(SpecCond)) %>% 
      ungroup() %>% 
      dplyr::select(-doy)
    
    min_date <- min(ts_data_ds$dateTime)
    mk_data <- ts(ts_data_ds$SpecCond_ds, 
                  start = c(lubridate::year(min_date), 
                            lubridate::yday(min_date)), 
                  frequency = 365)
  } else {
    min_date <- min(ts_data$dateTime)
    mk_data <- ts(ts_data$SpecCond, 
                  start = c(lubridate::year(min_date), 
                            lubridate::yday(min_date)), 
                  frequency = 365)
  }
  
  # Run the Mann-Kendall model and extract the trend result
  mk_data %>% 
    Kendall::MannKendall() %>% 
    extract_mk_trend(max_pval = max_pval)
  
}

#' @title Find a trend in SC using a Seasonal Mann-Kendall test
#' @description Calculate a trend for SC data using `Kendall::SeasonalMannKendall()`
#' 
#' @param ts_data a tibble of SC timeseries data for a single site with at least
#' the columns `dateTime` and `SpecCond`.
#' @param max_pval numeric value indicating the maximum p-value that is
#' allowed to declare a trend significant. Passed on to `extract_mk_trend()`
#' 
#' @return a character string indicating the trend as "none", "positive", or "negative"
#' 
apply_SeasonalMannKendall <- function(ts_data, max_pval) {
  
  # According to PNNL: https://vsp.pnnl.gov/help/Vsample/Design_Trend_Seasonal_Kendall.htm
  # Use seasonal MK when the "presence of seasonality implies that the data have different distributions for 
  #   different seasons (e.g., months) of the year. For example, a monotonic upward trend may exist over years 
  #   for January but not for June." AND a "monotonic upward (downward) trend means that the variable consistently 
  #   increases (decreases) over time, but the trend may or may not be linear."
  # Use MK when "seasonality is not expected to be present or when trends occur in different directions 
  #   (up or down) in different seasons." If there is seasonality but trends might be in different directions,
  #   looks like you might be able to "de-seasonalize" first?? See examples in `?Kendall::MannKendall`
  # While I think that the trends may be different by season, I'm not sure we can apply the blanket statement 
  # that they will all either be going up or down. I think there may be instances where winter seasons are going
  # up but summer are going down?
  
  # To use a Seasonal Mann-Kendall, first transform the data into monthly averages
  ts_data_monthly <- ts_data %>% 
    mutate(year = lubridate::year(dateTime),
           month_num = sprintf('%02d', lubridate::month(dateTime))) %>% 
    group_by(year, month_num) %>% 
    summarize(SpecCond_avg = mean(SpecCond), .groups="keep") %>% 
    ungroup() %>% 
    pivot_wider(names_from = month_num, values_from = SpecCond_avg) %>% 
    # Arrange columns so January is first, then replace with abbreviations instead of numbers
    relocate(year, order(names(.))) %>% 
    rename_with(~month.abb[as.numeric(.x)], -year)
  
  # Convert the data into a `ts` object to pass to `SeasonalMannKendall()`
  smk_data <- ts_data_monthly %>% 
    # Remove the `year` column so that just the data is converted to a `ts` obj
    dplyr::select(-year) %>% 
    ts(start = c(min(ts_data_monthly$year), 1), frequency = 1)
  
  # Run the Seasonal Mann-Kendall model and extract the trend result
  smk_data %>% 
    Kendall::SeasonalMannKendall() %>% 
    extract_mk_trend(max_pval = max_pval)
  
}

#' @title Extract the trend from MannKendall (MK output)
#' @description Use the output from a `Kendall::MannKendall()` or 
#' `Kendall::SeasonalMannKendall()` model run to categorize the trend
#' as either `none`, `positive`, or `negative`. 
#' 
#' @param mk_output a model object of the class `Kendall` from running either
#' `Kendall::MannKendall()` or `Kendall::SeasonalMannKendall()`.
#' @param max_pval numeric value indicating the maximum p-value that is
#' allowed to declare a trend significant. Any model output with a p-value
#' above this value will have return "none" for the trend. Defaults to 0.05.
#' 
#' @return a character string indicating the trend as "none", "positive", or "negative"
#' 
extract_mk_trend <- function(mk_output, max_pval = 0.05) {
  
  # Extract the pvalue programmatically
  pval <- capture.output(summary(mk_output))[3] %>% 
    str_split_1("pvalue =") %>% tail(1)
  
  # Drop the '<' that can sometimes be present for
  # super small numbers.
  pval <- as.numeric(gsub('< ', '', pval))
  
  # Return the appropriate trend name based on the
  # MannKendall `Score` (aka S) but only if the
  # p-value was below `max_pval`.
  if(pval >= max_pval | mk_output$S == 0) {
    trend <- "none"
  } else if(mk_output$S < 0) {
    trend <- "negative"
  } else if(mk_output$S > 0) {
    trend <- "positive"
  }
  
  return(trend)
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
    # Extract the cell values for cells within a 5 km radius of each site
    # Note that this is not weighted based on how much of the cell is inside the
    # buffered area around each site - it is either included or not based on the
    # methods descriped in `raster::extract()`: "If the distance between the 
    # sampling point and the center of a cell is less than or equal to the buffer, 
    # the cell is included." AKA the cell's center must be within the buffer
    raster::extract(sites_sf, 
                    buffer=5000, # in meters
                    df=TRUE) %>% 
    as_tibble() %>% 
    # Add the cells around a site together to get one value of 
    # road salt applied per site.
    group_by(ID) %>%
    summarize(attr_roadSalt = sum(road_salt_2015, na.rm = TRUE)) %>% 
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
