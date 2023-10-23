# Testing out baseflow separation and using some of the transmissivity data

library(dataRetrieval)
library(grwat) # devtools::install_github('tsamsonov/grwat')
library(DVstats) # 
library(tidyverse)
library(nhdplusTools)
library(sf)

# Functions:
separate_baseflow <- function(q_site) {
  # Add a baseflow and stormflow column
  q_site_with_baseflow <- q_site %>% 
    mutate(Q_Base = grwat::gr_baseflow(Q))  %>% 
    mutate(Q_Storm = Q - Q_Base)
}

sites <- c(
  '04024000', # MN site
  '04087170' # WI site
)

# Identify NHD comid from sites
sites_sf <- readNWISsite(sites) %>% 
  select(site_no, station_nm, state_cd, huc_cd, dec_long_va, dec_lat_va, drain_area_va) %>% 
  st_as_sf(coords = c('dec_long_va', 'dec_lat_va'), crs = 4326) 
sf::sf_use_s2(FALSE)
nhd_comid <- sites_sf %>% 
  split(.$site_no) %>% 
  purrr::map(get_flowline_index, flines = "download_nhdplusv2") %>% 
  bind_rows() %>% 
  select(COMID, REACHCODE, REACH_meas)
  
# Load the transmissivity and depth-to-water table data and add to site info
# https://www.sciencebase.gov/catalog/item/60be54f6d34e86b9389117f9
trans <- read_csv('trans.csv', col_types = cols()) %>% 
  select(COMID = comid, trans_MEAN = MEAN, trans250, tottrans250) 
dtw <- read_csv('dtw.csv', col_types = cols()) %>% 
  select(COMID = comid, dtw_MEAN = MEAN, dtw250, totdtw250)

# Add percentiles to be able to show relative transmissivity/dtw
trans_ecdf <- ecdf(trans$trans_MEAN)
dtw_ecdf <- ecdf(dtw$dtw_MEAN)

sites_info <- sites_sf %>% 
  bind_cols(nhd_comid) %>% 
  left_join(trans, by="COMID") %>% 
  left_join(dtw, by="COMID") %>% 
  mutate(trans_PERCENTILE = trans_ecdf(trans_MEAN)*100,
         dtw_PERCENTILE = dtw_ecdf(dtw_MEAN)*100)

# Download daily streamflow and specific conductivity
q_sc_data <- readNWISdata(siteNumber = sites, 
                          parameterCd = c('00060', '00095'),
                          startDate = '2002-01-01',
                          endDate = '2022-12-31',
                          service = 'dv') %>% 
  renameNWISColumns() %>% 
  rename(Q = Flow) %>% 
  addWaterYear() %>% 
  mutate(month = lubridate::month(dateTime)) %>% 
  arrange(dateTime) %>% 
  split(.$site_no) %>% 
  purrr::map(separate_baseflow) %>% 
  bind_rows() %>% 
  select(site_no, waterYear, month, dateTime, 
         Q, Q_Base, Q_Storm, SpecCond)

q_sc_data_with_siteinfo <- q_sc_data %>% 
  left_join(sites_info, by = "site_no")

# The following is per site:
site_now <- sites[1]
site_info_now <- sites_info %>% filter(site_no == site_now)
q_sc_data_site <- q_sc_data %>% filter(site_no == site_now)

# Show plot of Flow and baseflow for just a single water year:
q_sc_data_site %>% 
  filter(dateTime >= as.Date('2021-10-01'), dateTime <= as.Date('2022-09-30')) %>% 
  ggplot() +
  geom_area(aes(dateTime, Q), fill = 'steelblue', color = 'black') +
  geom_area(aes(dateTime, Q_Base), fill = 'orangered', color = 'black')

# Now plot baseflow and specific conductivity timeseries
q_sc_data_site %>% 
  mutate(logQ_Base = log10(Q_Base)) %>% 
  select(site_no, dateTime, logQ_Base, SpecCond) %>%
  # SpecCond started later
  filter(!is.na(SpecCond)) %>% 
  pivot_longer(cols = -c("site_no", "dateTime"), names_to = "variable") %>% 
  ggplot(aes(x = dateTime, y = value, color = variable)) +
  geom_line()

# Plot to figure out if baseflow and storm flow are changing SC in the same dir?
# q_sc_data_site %>% 
#   select(site_no, waterYear, Q_Storm, Q_Base, SpecCond) %>% 
#   filter(!is.na(SpecCond)) %>% 
#   pivot_longer(cols = -c("site_no", "waterYear", "SpecCond"), names_to = "FlowType") %>% 
#   ggplot(aes(x = value, y = SpecCond, color = FlowType)) +
#   geom_point() +
#   facet_wrap(vars(waterYear))

# Link transmissivity to baseflow SC

# Specifically think of summer baseflow
q_sc_data %>% 
  filter(month %in% 6:9) %>% 
  filter(Q_Storm == 0) %>%
  ggplot(aes(x = dateTime, y = SpecCond)) +
  geom_point() +
  geom_smooth(method=lm) +
  facet_grid(. ~ site_no) +
  ggtitle('Trend in summer baseflow SC')

# Now think of winter and baseflow
q_sc_data_with_siteinfo %>% 
  filter(month %in% c(1:3, 11, 12)) %>% 
  filter(Q_Storm == 0) %>%
  mutate(label_nm = sprintf(
    '%s\nTs percentile = %s\nDTW percentile = %s',
    site_no, round(trans_PERCENTILE), round(dtw_PERCENTILE))) %>% 
  ggplot(aes(x = dateTime, y = SpecCond)) +
  geom_point() +
  geom_smooth(method=lm) +
  facet_grid(. ~ label_nm) +
  ggtitle('Trend in winter baseflow SC')

# Site with increasing winter baseflow SC trend has very very
# high Ts, while site with decreasing trend has low Ts. Is there
# something here? Trends are very weak, though.

##### Use timeseries decomposition to look at this trend differently #####

decompose_daily_data <- function(value, start_date) {
  
  ts_data <- ts(
    #  There cannot be NAs
    data = na.omit(value), 
    # Frequency indicates how the data is collected
    frequency = 365, 
    start = c(
      # Year, Month
      lubridate::year(start_date), 
      lubridate::month(start_date)))
  
  # decompose the time series into seasonal, trend, and random components
  decomp <- decompose(ts_data)
  
  return(decomp)
}

# convert the date column to a time series object
ts_data_prep <- q_sc_data_with_siteinfo %>% 
  mutate(date = as.Date(dateTime)) %>% 
  filter(site_no == sites[1]) %>% 
  filter(!is.na(SpecCond), !is.na(Q)) %>% 
  mutate(SC_Q_norm = SpecCond/Q)


# plot the decomposed time series

decompose_daily_data(ts_data_prep$SpecCond, min(ts_data_prep$date)) %>% 
  plot()
decompose_daily_data(as.matrix(ts_data_prep[,c('SpecCond', 'Q', 'SC_Q_norm')]), min(ts_data_prep$date)) %>% 
  plot()


##### Basic LM to relate SC from GW to conditions at the site? #####

library(targets)
sc_data <- tar_read(conus_sc_data)
q_data <- tar_read(conus_q_data)
attributes(sc_data) <- attributes(sc_data)[names(attributes(sc_data)) %in% c('names', 'row.names', 'class')]
attributes(q_data) <- attributes(q_data)[names(attributes(q_data)) %in% c('names', 'row.names', 'class')]
q_sc_data <- sc_data %>% 
  # Just WI and NE for now to represent high and low connectivity
  dplyr::filter(state_abbr %in% c("NE", "WY")) %>% 
  left_join(q_data, by = c("state_abbr", "site_no", "dateTime")) %>% 
  filter(!is.na(mean_spec_cond), !is.na(mean_q))

q_sc_sites_sf <- readNWISsite(unique(q_sc_data$site_no)) %>% 
  select(site_no, station_nm, state_cd, huc_cd, dec_long_va, dec_lat_va, drain_area_va) %>% 
  st_as_sf(coords = c('dec_long_va', 'dec_lat_va'), crs = 4326) 
sf::sf_use_s2(FALSE)
q_sc_nhd_comid <- q_sc_sites_sf %>% 
  split(.$site_no) %>% 
  purrr::map(get_flowline_index, flines = "download_nhdplusv2") %>% 
  bind_rows() %>% 
  select(COMID, REACHCODE, REACH_meas)
q_sc_sites_info <- q_sc_sites_sf %>% 
  bind_cols(q_sc_nhd_comid) %>% 
  left_join(trans, by="COMID") %>% 
  left_join(dtw, by="COMID") %>% 
  mutate(trans_PERCENTILE = trans_ecdf(trans_MEAN)*100,
         dtw_PERCENTILE = 100-dtw_ecdf(dtw_MEAN)*100)

q_sc_data_baseq <- q_sc_data %>% 
  mutate(date = as.Date(dateTime)) %>% 
  dplyr::select(site_no, date, SpecCond = mean_spec_cond, Q = mean_q, state_abbr) %>% 
  split(.$site_no) %>% 
  purrr::map(separate_baseflow) %>%
  bind_rows() %>% 
  select(site_no, date, Q, Q_Base, Q_Storm, SpecCond, state_abbr) %>%
  mutate(baseflow_only = Q_Storm == 0) %>% 
  left_join(q_sc_sites_info) %>% 
  mutate(Ts = trans_MEAN, DTW = dtw_MEAN) %>% 
  mutate(SC_flow_normalized = SpecCond / Q)

ts_plot <- q_sc_data_baseq %>% 
  filter(lubridate::month(date) %in% 6:9) %>% 
  filter(baseflow_only) %>%
  ggplot(aes(x = date, y = SpecCond, group = site_no, color = trans_PERCENTILE)) +
  scico::scale_color_scico(palette = 'batlow', limits=c(0, 100)) +
  # geom_point() +
  geom_smooth(method=lm) +
  # facet_wrap(vars(site_no)) +
  ggtitle('Trend in summer baseflow SC')
dtw_plot <- q_sc_data_baseq %>% 
  filter(lubridate::month(date) %in% 6:9) %>% 
  filter(baseflow_only) %>%
  ggplot(aes(x = date, y = SpecCond, group = site_no, color = dtw_PERCENTILE)) +
  scico::scale_color_scico(palette = 'batlow', limits=c(0, 100)) +
  # geom_point() +
  geom_smooth(method=lm) 
  # facet_wrap(vars(site_no)) 
cowplot::plot_grid(ts_plot, dtw_plot)

q_sc_data_baseq %>% 
  filter(lubridate::month(date) %in% 6:9) %>% 
  filter(baseflow_only) %>%
  mutate(gw_connectivity = trans_PERCENTILE + dtw_PERCENTILE) %>% 
  ggplot(aes(x = date, y = SpecCond, group = site_no, color = gw_connectivity)) +
  scico::scale_color_scico(palette = 'batlow', limits=c(0, 200)) +
  # geom_point() +
  geom_smooth(method=lm) 
# facet_wrap(vars(site_no)) 

# What about pulling out positive, none, negative relationships between
# SC over time and then adding in Ts / DTW on a national scale?
q_sc_data_baseq %>% 
  filter(site_no == "06795500") %>% # "06036905"
  mutate(gw_connectivity = trans_PERCENTILE + dtw_PERCENTILE) %>% 
  # filter(lubridate::month(date) %in% 6:9) %>%
  ggplot(aes(x = date, y = SC_flow_normalized, group = site_no, color = gw_connectivity)) +
  scico::scale_color_scico(palette = 'batlow', limits=c(0, 200)) +
  geom_point() +
  geom_smooth(method=lm) 

extract_trend <- function(values, min_date, method = c("LM", "MA", "DECOMP")) {
  data <- ts(values, start = c(lubridate::year(min_date), lubridate::month(min_date)), frequency = 365)
  
  if(method == "LM") {
    
    ##### Option 1: Fit linear regression model #####
    model <- lm(data ~ time(data))
    
    # Check slope of regression line
    slope <- coef(model)[2]
    if (slope > 0) {
      trend <- "positive"
    } else if (slope < 0) {
      trend <- "negative"
    } else {
      trend <- "zero"
    }
    
  } else if(method == "MA") {
    
    ##### Option 2: Calculate 3-month moving average #####
    ma <- na.omit(TTR::SMA(data, n = 3))
    
    # Check direction of moving average
    if (ma[length(ma)] > ma[1]) {
      trend <- "positive"
    } else if (ma[length(ma)] < ma[1]) {
      trend <- "negative"
    } else {
      trend <- "zero"
    }
    
  } else if(method == "DECOMP") {
    ##### Option 3: Decompose time series data #####
    decomp <- stl(data, s.window = "periodic")
    
    # Check direction of trend component
    trend <- decomp$time.series[, "trend"]
    if (trend[length(trend)] > trend[1]) {
      trend <- "positive"
    } else if (trend[length(trend)] < trend[1]) {
      trend <- "negative"
    } else {
      trend <- "zero"
    }
    
  }
  
  return(trend)
}

# Test out the function
# x<-q_sc_data_baseq %>% filter(site_no == "06795500") %>% arrange(date)
# extract_trend(x$SC_flow_normalized, min_date = min(x$date), method="LM")

trend_plot_data <- q_sc_data_baseq %>% 
  # filter(site_no %in% c("06795500", "06036905")) %>%
  group_by(site_no, trans_PERCENTILE, dtw_PERCENTILE) %>% 
  summarize(SC_trend = extract_trend(SpecCond, min(date), method="MA"),
            SC_trend_flow_norm = extract_trend(SC_flow_normalized, min(date), method="MA")) %>% 
  mutate(Ts_plot = trans_PERCENTILE - 50, DTW_plot = dtw_PERCENTILE - 50) %>% 
  mutate(Ts_positive = Ts_plot >= 0, DTW_positive = DTW_plot >= 0)

trend_plot_data %>% 
  group_by(Ts_positive, DTW_positive, SC_trend, SC_trend_flow_norm) %>% 
  tally() %>%
  filter(Ts_positive == DTW_positive)

ggplot(trend_plot_data, aes(x = DTW_plot, y = Ts_plot, color = SC_trend_flow_norm)) +
  geom_point() +
  scico::scale_colour_scico_d(palette = 'bam', begin=0.25, end=0.75) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  xlim(c(-50,50)) + ylim(c(-50,50)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# regdata <- q_sc_data_with_siteinfo %>% 
#   ungroup() %>% 
#   mutate(baseflow_only = Q_Storm == 0) %>% 
#   group_by(site_no) %>% 
#   summarize(avg_sc_base = mean(SpecCond[baseflow_only], na.rm=TRUE),
#             avg_sc_storm = mean(SpecCond[!baseflow_only], na.rm=TRUE),
#             Ts = trans_MEAN,
#             DTW = dtw_MEAN) %>% 
#   mutate(SC_gwsw_ratio = avg_sc_base/avg_sc_storm) %>% 
#   # I don't understand why `summarize()` isn't succesfully reducing the 
#   # number of columns and I am having to add this step
#   distinct()

regdata <- q_sc_data_baseq %>%
  group_by(site_no) %>%
  summarize(avg_sc_base = mean(SpecCond[baseflow_only], na.rm=TRUE),
            avg_sc_storm = mean(SpecCond[!baseflow_only], na.rm=TRUE)) %>%
  mutate(SC_gwsw_ratio = avg_sc_base/avg_sc_storm) %>%
  # I don't understand why `summarize()` isn't succesfully reducing the
  # number of columns and I am having to add this step
  distinct() 

plot(regdata$Ts, regdata$SC_gwsw_ratio)
plot(regdata$DTW, regdata$SC_gwsw_ratio)

sc_base_lm <- lm(avg_sc_base ~ Ts + (1/DTW), data = regdata)
sc_base_lm <- lm(avg_sc_base ~ Ts + (1/DTW), data = regdata)


##### Try out a random forest model #####

# library(randomForest)
# set.seed(123)
# train_idx <- sample(1:nrow(q_sc_data_with_siteinfo), 0.7*nrow(q_sc_data_with_siteinfo))
# train_data <- q_sc_data_with_siteinfo[train_idx,]
# test_data <- q_sc_data_with_siteinfo[-train_idx,]
# 
# model <- randomForest(purchased ~ age + gender + income, data = train_data)
