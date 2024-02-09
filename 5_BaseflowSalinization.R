# Targets for identifying sites that exhibit "baseflow salinization"

source('5_BaseflowSalinization/src/delineate_baseflow.R')

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
  tar_target(p5b_baseflow_sites, c())
  
)
