
separate_baseflow <- function(q_site) {
  # Add a baseflow and stormflow column
  q_site_with_baseflow <- q_site %>% 
    mutate(Q_Base = grwat::gr_baseflow(Q))  %>% 
    mutate(Q_Storm = Q - Q_Base)
}

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

add_trends <- function(qbase_sc_data, trend_method) {
  qbase_sc_data %>%
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
}
