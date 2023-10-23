# Targets for downloading data used in this analysis

source('1_Download/src/nwis_fxns.R')

p1_targets <- list(
  
  # Define target listing the state abbreviations within the Contiguous United States (CONUS)
  tar_target(p1_conus_state_cds, state.abb[!state.abb %in% c('AK', 'HI')] %>% 
               head(2)), # TODO: remove this filter!
  
  ##### NWIS DATA: streamflow (Q) & specific conductivity (SC) #####
  
  tar_target(p1_nwis_start_date, as.Date('2022-12-01')), # TODO: Set start date back to '1950-01-01'
  tar_target(p1_nwis_end_date, as.Date('2022-12-31')),
  
  # Dynamically branch over states to identify sites with SC data
  tar_target(p1_nwis_sc_sites,
             inventory_nwis_sites_byState(state_cd = p1_conus_state_cds, 
                                          param_cd = '00095', 
                                          start_date = p1_nwis_start_date,
                                          end_date = p1_nwis_end_date),
             pattern = map(p1_conus_state_cds),
             iteration = 'list'),
  
  # Of the SC sites, identify ones which also have Q data
  tar_target(p1_nwis_sc_sites_with_q,
             inventory_nwis_sites_bySite(site_number = p1_nwis_sc_sites, 
                                         param_cd = '00060', 
                                         start_date = p1_nwis_start_date,
                                         end_date = p1_nwis_end_date),
             pattern = map(p1_nwis_sc_sites),
             iteration = 'list'),
  
  # Turn the list of sites with SC & Q data per state into a 
  # table with `state_cd` and `site_no` columns
  tar_target(p1_nwis_site_tbl, p1_nwis_sc_sites_with_q %>%
               setNames(p1_conus_state_cds) %>% 
               purrr::map(as_tibble_col, column_name = 'site_no') %>% 
               bind_rows(.id = 'state_cd'))
  
)
