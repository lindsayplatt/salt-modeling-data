
separate_baseflow <- function(q_site) {
  # Add a baseflow and stormflow column
  q_site_with_baseflow <- q_site %>% 
    mutate(Q_Base = grwat::gr_baseflow(Q))  %>% 
    mutate(Q_Storm = Q - Q_Base)
}

trend_filter_data <- function(in_data, hydro_cond, season) {
  
  in_data %>% {
    if(hydro_cond == 'base') 
      filter(., Q_Storm == 0)
    else .
  } %>% {
    if(hydro_cond == 'storm') 
      filter(., Q_Storm != 0)
    else .
  } %>% {
    if(season != 'all') 
      filter(., !!as.symbol(sprintf('is_%s', season)))
    else .
  }
  
}

extract_trend <- function(values, min_date, method = c("LM", "MA", "DECOMP", "MK")) {
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
    
  } else if(method == 'MK') {
    
    ##### Option 4: Perform MannKendall test #####
    trend <- Kendall::MannKendall(data)
    
    pval <- capture.output(summary(trend))[3] %>% 
      str_split_1("pvalue =") %>% tail(1)
    # Drop the '<' that can sometimes be present for
    # super small numbers.
    pval <- as.numeric(gsub('< ', '', pval))
    
    if(pval >= 0.05 | trend$S == 0) {
      trend <- "zero"
    } else if(trend$S < 0) {
      trend <- "negative"
    } else if(trend$S > 0) {
      trend <- "positive"
    }
  }
  
  return(trend)
}

add_trends <- function(qbase_sc_data, trend_method) {
  
  trend_combos <- expand.grid(
    season = c('all', 'winter', 'spring', 'summer', 'fall'),
    hydro_cond = c('all', 'base', 'storm')) %>%
    # Remove factors
    mutate(season = as.character(season),
           hydro_cond = as.character(hydro_cond))
  
  qbase_sc_trends <- qbase_sc_data %>%
    group_by(site_no, Ts, DTW, trans_PERCENTILE, dtw_PERCENTILE) %>% 
    group_map(~ {
      # Map over each of the possible trend combinations (there are 12)
      trend_out <- purrr::map2_chr(trend_combos$hydro_cond, trend_combos$season, function(hydro_cond, season, grp_data) {
        trend_data <- trend_filter_data(grp_data, hydro_cond, season)
        extract_trend(trend_data$SC_flow_normalized, min(trend_data$date), method=trend_method)
      }, grp_data = .x)
      # Take the 12-row trend summary and combine with the data frame for
      # the current site's info
      trend_combos_out <- mutate(trend_combos, trend = trend_out)
      bind_cols(.y, trend_combos_out)
    }, .keep=TRUE) %>% bind_rows()
  
  qbase_sc_trends %>% 
    mutate(Ts_plot = trans_PERCENTILE, DTW_plot = dtw_PERCENTILE) %>% 
    mutate(Ts_high = Ts_plot >= 50, DTW_low = DTW_plot >= 50) %>% 
    ungroup()
}
