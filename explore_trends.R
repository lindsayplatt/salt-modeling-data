# Explore trends

library(dataRetrieval)
library(grwat) # devtools::install_github('tsamsonov/grwat')
library(tidyverse)
library(nhdplusTools)
library(sf)
library(targets)

# Get/load data
sc_data <- tar_read(conus_sc_data) %>% distinct()
q_data <- tar_read(conus_q_data) %>% distinct()
attributes(sc_data) <- attributes(sc_data)[names(attributes(sc_data)) %in% c('names', 'row.names', 'class')]
attributes(q_data) <- attributes(q_data)[names(attributes(q_data)) %in% c('names', 'row.names', 'class')]

# Load the transmissivity and depth-to-water table data and add to site info
# https://www.sciencebase.gov/catalog/item/60be54f6d34e86b9389117f9
trans <- read_csv('trans.csv', col_types = cols()) %>% 
  select(COMID = comid, trans_MEAN = MEAN, trans250, tottrans250) 
dtw <- read_csv('dtw.csv', col_types = cols()) %>% 
  select(COMID = comid, dtw_MEAN = MEAN, dtw250, totdtw250)

# Load functions

separate_baseflow <- function(q_site) {
  # Add a baseflow and stormflow column
  q_site_with_baseflow <- q_site %>% 
    mutate(Q_Base = grwat::gr_baseflow(Q))  %>% 
    mutate(Q_Storm = Q - Q_Base)
}

# Add percentiles to be able to show relative transmissivity/dtw
trans_ecdf <- ecdf(trans$trans_MEAN)
dtw_ecdf <- ecdf(dtw$dtw_MEAN)

extract_trend <- function(values, min_date, method = c("LM", "MA", "DECOMP")) {
  if(length(values) < 3) return(as.character(NA))
  
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
    
    if(length(data) < 365*2) return(as.character(NA))
    
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


# Process the data

# Join discharge & SC data
# Filter out missing data & add a spatial limit
north_states <- c("CT", "DE", "IL", "IN", "IA", "KS", "ME", "MD", "MA", "MI", 
                  "MN", "MO", "NE", "NH", "NJ", "NY", "ND", "OH", "PA", "RI", "SD", "VT", "VA", "WV", "WI")

q_sc_data <- sc_data %>% 
  dplyr::filter(state_abbr %in% north_states) %>% ##### CHANGE STATES HERE
  left_join(q_data, by = c("state_abbr", "site_no", "dateTime")) %>% 
  filter(!is.na(mean_spec_cond), !is.na(mean_q))

# Identify the sites & find their matching COMIDs
q_sc_sites_sf <- readNWISsite(unique(q_sc_data$site_no)) %>% 
  select(site_no, station_nm, state_cd, huc_cd, dec_long_va, dec_lat_va, drain_area_va) %>% 
  st_as_sf(coords = c('dec_long_va', 'dec_lat_va'), crs = 4326) %>% 
  # These sites caused an error in `get_flowline_index()` that I could not figure out
  filter(!site_no %in% c("05075720", "01104420", "02290947", "02326993", "02326995",
                         "03353420", "07381324", "10172630", "295124089542100",
                         "410401112134801"))
# sf::sf_use_s2(FALSE) # Some combination of this being on/off allowed the sites removed above to be kept but I can't figure it out
# q_sc_nhd_comid <- q_sc_sites_sf %>% 
#   mutate(row_num = row_number()) %>%
#   split(.$site_no) %>% 
#   purrr::map(~{
#     if(.x$row_num%%50 == 0) {
#       # Print a message for every 50th site so I can see some progress
#       message('Starting flow index for ', .x$row_num, ' out of ', nrow(q_sc_sites_sf))
#     }
#     suppressMessages(get_flowline_index(.x, flines = "download_nhdplusv2"))
#   }) %>% 
#   bind_rows() %>% 
#   select(COMID, REACHCODE, REACH_meas)
# q_sc_site_comid_xwalk <- q_sc_sites_sf %>% 
#   bind_cols(q_sc_nhd_comid)
# saveRDS(q_sc_site_comid_xwalk, 'conus_nhd_xwalk.rds')
q_sc_site_comid_xwalk <- readRDS('conus_nhd_xwalk.rds')

# Join the transmissivity and depth-to-watertable data by COMID
q_sc_sites_info <- q_sc_site_comid_xwalk %>% 
  filter(site_no %in% unique(q_sc_sites_sf$site_no)) %>% 
  left_join(trans, by="COMID") %>% 
  left_join(dtw, by="COMID") %>% 
  mutate(trans_PERCENTILE = trans_ecdf(trans_MEAN)*100,
         dtw_PERCENTILE = 100-dtw_ecdf(dtw_MEAN)*100)

# Separate baseflow in case we want to use that
# Also, calculate flow-normalized SC
q_sc_data_baseq <- q_sc_data %>% 
  mutate(date = as.Date(dateTime)) %>% 
  dplyr::select(site_no, date, SpecCond = mean_spec_cond, Q = mean_q, state_abbr) %>% 
  split(.$site_no) %>% 
  purrr::map(separate_baseflow) %>%
  bind_rows() %>% 
  select(site_no, date, Q, Q_Base, Q_Storm, SpecCond, state_abbr) %>%
  mutate(baseflow_only = Q_Storm == 0, 
         is_summer = lubridate::month(date) %in% 6:9,
         is_spring = lubridate::month(date) %in% 3:5,
         is_fall = lubridate::month(date) %in% 10:11) %>%
  left_join(q_sc_sites_info) %>% 
  mutate(Ts = trans_MEAN, DTW = dtw_MEAN) %>% 
  mutate(SC_flow_normalized = SpecCond / (Q+0.000000001)) # Add this trivial amt to avoid dividing by 0

# Calculate trends

calc_and_plot_trend <- function(trend_method, subtitle_caveat) {
  sc_trend_data <- q_sc_data_baseq %>%
    group_by(site_no, Ts, DTW, trans_PERCENTILE, dtw_PERCENTILE) %>% 
    summarize(trend_SC_all = extract_trend(SpecCond, min(date), method=trend_method), 
              trend_SC_qnorm_all = extract_trend(SC_flow_normalized, min(date), method=trend_method),
              trend_SC_base = extract_trend(SpecCond[baseflow_only], min(date[baseflow_only]), method=trend_method), 
              trend_SC_qnorm_base = extract_trend(SC_flow_normalized[baseflow_only], min(date[baseflow_only]), method=trend_method),
              trend_SC_summer = extract_trend(SpecCond[is_summer], min(date[is_summer]), method=trend_method), 
              trend_SC_qnorm_summer = extract_trend(SC_flow_normalized[is_summer], min(date[is_summer]), method=trend_method),
              trend_SC_summer_base = extract_trend(SpecCond[is_summer & baseflow_only], min(date[is_summer & baseflow_only]), method=trend_method), 
              trend_SC_qnorm_summer_base = extract_trend(SC_flow_normalized[is_summer & baseflow_only], min(date[is_summer & baseflow_only]), method=trend_method),
              trend_SC_summer_base = extract_trend(SpecCond[is_summer & baseflow_only], min(date[is_summer & baseflow_only]), method=trend_method), 
              trend_SC_qnorm_summer_base = extract_trend(SC_flow_normalized[is_spring & baseflow_only], min(date[is_spring & baseflow_only]), method=trend_method),
              trend_SC_spring_base = extract_trend(SpecCond[is_summer & baseflow_only], min(date[is_summer & baseflow_only]), method=trend_method), 
              trend_SC_qnorm_spring_base = extract_trend(SC_flow_normalized[is_spring & baseflow_only], min(date[is_spring & baseflow_only]), method=trend_method)) %>% 
    mutate(Ts_plot = trans_PERCENTILE, DTW_plot = dtw_PERCENTILE) %>% 
    mutate(Ts_high = Ts_plot >= 50, DTW_low = DTW_plot >= 50) %>% 
    ungroup()
  
  # Print counts summary
  sc_trend_data %>% 
    group_by(Ts_high, DTW_low, across(starts_with('trend_SC'))) %>% 
    tally() %>%
    filter(Ts_high == DTW_low) 
  
  # Plot patterns
  trend_plot_data <- sc_trend_data %>% 
    select(site_no, Ts, DTW, Ts_plot, DTW_plot, starts_with("trend_SC")) %>% 
    pivot_longer(cols=starts_with("trend_SC"), names_to = "trend_calc", values_to = "trend") %>% 
    mutate(trend_calc = gsub("trend_SC_", "", trend_calc)) %>% 
    mutate(alpha_val = ifelse(trend == "positive", 1, 0.5))
  
  p1 <- ggplot(trend_plot_data, aes(x = DTW_plot, y = Ts_plot, color = trend, alpha = alpha_val)) +
    geom_point(size=2,shape=16) +
    scale_alpha_identity() +
    scale_color_manual(values = c(negative = "#F8766D", positive = "#00BA38", zero = "#619CFF")) +
    geom_hline(yintercept = 50) + geom_vline(xintercept = 50) +
    facet_wrap(vars(trend_calc)) +
    xlim(c(0,100)) + ylim(c(0,100)) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    xlab('Depth to water table, %ile') +
    ylab('Transmissivity, %ile') +
    ggtitle('SC trends related to GW connectivity, percentiles',
            sub = sprintf("Trends based on %s method, %s", trend_method, subtitle_caveat)) 
  
  p2 <- ggplot(trend_plot_data, aes(x = -log10(DTW), y = log10(Ts), color = trend, alpha = alpha_val)) +
    geom_point(size=2,shape=16) +
    scale_alpha_identity() +
    scale_color_manual(values = c(negative = "#F8766D", positive = "#00BA38", zero = "#619CFF")) +
    facet_wrap(vars(trend_calc)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    xlab('Depth to water table') +
    ylab('Transmissivity') +
    ggtitle('SC trends related to GW connectivity, real values',
            sub = sprintf("Trends based on %s method, %s", trend_method, subtitle_caveat)) 
  
  p3 <- trend_plot_data %>% 
    filter(!is.na(trend)) %>% 
    ggplot(aes(x = log(Ts), color = trend)) +
    stat_ecdf(geom="step") +
    scale_color_manual(values = c(negative = "#F8766D", positive = "#00BA38", zero = "#619CFF")) +
    facet_wrap(vars(trend_calc)) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    xlab('Transmissivity') +
    ylab('Cumulative Distribution') +
    ggtitle('SC trends related to GW connectivity, distribution',
            sub = sprintf("Trends based on %s method, %s", trend_method, subtitle_caveat)) 
  
  p4 <- trend_plot_data %>% 
    filter(!is.na(trend)) %>% 
    ggplot(aes(x = -log(DTW), color = trend)) +
    stat_ecdf(geom="step") +
    scale_color_manual(values = c(negative = "#F8766D", positive = "#00BA38", zero = "#619CFF")) +
    facet_wrap(vars(trend_calc)) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    xlab('Depth to water table') +
    ylab('Cumulative Distribution') +
    ggtitle('SC trends related to GW connectivity, distribution',
            sub = sprintf("Trends based on %s method, %s", trend_method, subtitle_caveat)) 
  
  p5 <- trend_plot_data %>% 
    mutate(Ts_cond = ifelse(Ts_plot >= 50, 'Ts-high', 'Ts-low'), 
           DTW_cond = ifelse(DTW_plot >= 50, 'DTW-shallow', 'DTW-deep')) %>% 
    mutate(GW_Connectivity = factor(sprintf('%s, %s', Ts_cond, DTW_cond), ordered = TRUE,
                                    levels = c('Ts-low, DTW-deep', 'Ts-low, DTW-shallow', 'Ts-high, DTW-deep', 'Ts-high, DTW-shallow'))) %>% 
    filter(!is.na(trend), !is.na(Ts_cond)) %>% 
    group_by(GW_Connectivity, trend_calc, trend) %>%
    tally() %>%
    ggplot(aes(x = GW_Connectivity, y = n, fill = trend)) +
    # geom_bar(stat="identity", position="dodge", width=0.5) +
    geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
    scale_fill_manual(values = c(negative = "#F8766D", positive = "#00BA38", zero = "#619CFF")) +
    facet_wrap(vars(trend_calc)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45,hjust=1)) +
    xlab('Groundwater connectivity (low --> high)') +
    ylab('Number of sites') +
    ggtitle('SC trends related to GW connectivity',
            sub = sprintf("Trends based on %s method, %s", trend_method, subtitle_caveat)) 
  
  return(list(p1,p2,p3,p4,p5))
}

lm_plots <- calc_and_plot_trend(trend_method = "LM", subtitle_caveat = "Northern states")
ma_plots <- calc_and_plot_trend(trend_method = "MA", subtitle_caveat = "Northern states")

cowplot::plot_grid(lm_plots[[1]], ma_plots[[1]], lm_plots[[2]], ma_plots[[2]], nrow = 2)
cowplot::plot_grid(lm_plots[[3]], ma_plots[[3]], lm_plots[[4]], ma_plots[[4]], nrow = 2)
cowplot::plot_grid(lm_plots[[5]], ma_plots[[5]], nrow = 1)
