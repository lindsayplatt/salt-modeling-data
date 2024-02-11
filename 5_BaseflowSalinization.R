# Targets for identifying sites that exhibit "baseflow salinization"

source('5_BaseflowSalinization/src/delineate_baseflow.R')
source('5_BaseflowSalinization/src/baseflow_qualification.R')
source('5_BaseflowSalinization/src/calculate_sc_trend.R')

# TODO: change the name once we are happy with skipping clustering. Don't want
# to overwrite yet.
p5b_targets <- list(
  
  # Prepare to map over each site
  tar_target(p5_q_grp, p3_attr_q_qualified %>% 
               group_by(site_no) %>% 
               tar_group(),
             iteration = 'group'),
  
  # Calculate how much of daily flow is baseflow vs quickflow using 
  # Rumsey et al 2023 methods: `FlowScreen::bf_eckhardt()` with the
  # constant `a=0.97` as suggested in Eckhardt 2012 for perennial streams
  tar_target(p5_baseflow_sep, 
             delineate_baseflow(p5_q_grp, p3_static_attributes),
             pattern = map(p5_q_grp)),
  
  # Identify which site-days meet criteria for being a non-winter baseflow day
  # defaults to Dec, Jan, Feb, Mar as "winter" and baseflow >= 90% of total daily flow
  tar_target(p5_baseflow_days, identify_baseflow_days(p5_baseflow_sep)),
  
  # Filter SC data to only non-winter baseflow days
  tar_target(p5_sc_baseflow, filter_ts_to_baseflow_days(p3_ts_sc_qualified, p5_baseflow_days, 'SpecCond')),
  
  # Calculate SC trends for non-winter baseflow days
  # This calculates the trend only for data that meets our criteria from 3_Filter
  # and has enough non-winter baseflow days that you can make the calculation
  tar_target(p5_sc_baseflow_qualified_info, apply_baseflow_trend_criteria(p5_sc_baseflow)),
  tar_target(p5_sc_baseflow_qualified_sites, identify_trend_sites(p5_sc_baseflow_qualified_info)),
  tar_target(p5_sc_baseflow_qualified, filter(p5_sc_baseflow, site_no %in% p5_sc_baseflow_qualified_sites)),
  
  tar_target(p5_sc_baseflow_trend, calculate_sc_trend(p5_sc_baseflow_qualified, max_pval = 0.05)),
  
  tar_target(p5b_baseflow_sites, c())
  
)
