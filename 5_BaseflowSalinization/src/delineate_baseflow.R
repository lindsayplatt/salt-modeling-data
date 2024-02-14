
# TODO: documentation
delineate_baseflow <- function(ts_q, site_bfi_data) {
  
  # Use a custom input for the BFI based on known info for this site.
  bfi <- site_bfi_data %>% 
    filter(site_no == unique(ts_q$site_no)) %>% 
    # Change from percent to fraction for `bf_eckhardt()`
    mutate(bfi = attr_baseFlowInd/100) %>% 
    pull(bfi)
  
  # Some of the sites don't have an NHD+ BFI, if that is the
  # case, assume BFI is the average for the sites in this dataset ...
  # TODO: BETTER ASSUMPTION HERE
  if(is.na(bfi)) {
    bfi <- mean(site_bfi_data$attr_baseFlowInd, na.rm=T)/100
  }
  
  # Use a constant for alpha as suggested by Eckhardt 2012 for perennial stream
  alpha <- 0.970 
  
  # Delineate baseflow from quickflow
  ts_q %>%
    mutate(baseFlow = bf_eckhardt(Flow, alpha, bfi),
           quickFlow = Flow - baseFlow) %>% 
    select(site_no,
           dateTime,
           Flow, 
           baseFlow,
           quickFlow)
  
}

# TODO: documentation
identify_baseflow_days <- function(ts_bf, min_baseflow_frac = 0.90, winter_months = c(12,1,2,3)) {
  
  ts_bf %>% 
    # Determine if the day qualifies as a "non-winter baseflow" day
    mutate(is_winter = month(dateTime) %in% winter_months) %>% 
    mutate(is_baseFlow_day = baseFlow >= min_baseflow_frac*Flow & !is_winter) %>% 
    select(site_no,
           dateTime,
           is_baseFlow_day)
  
}
