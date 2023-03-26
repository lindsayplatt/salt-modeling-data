
library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c(
  'arrow',
  'dataRetrieval',
  'geofacet',
  'grwat', # devtools::install_github('tsamsonov/grwat')
  'lubridate',
  'maps',
  'nhdplusTools',
  'sbtools',
  'scico',
  'sf',
  'tidyverse'
))

start_date <- '2010-01-01'
end_date <- '2021-12-31'

north_states <- c("CT", "DE", "IL", "IN", "IA", "KS", "ME", "MD", "MA", "MI", 
                  "MN", "MO", "NE", "NH", "NJ", "NY", "ND", "OH", "PA", "RI", 
                  "SD", "VT", "VA", "WV", "WI")

source('1_fetch.R')
source('2_process.R')
source('3_visualize.R')

c(p1_targets, p2_targets, p3_targets)
