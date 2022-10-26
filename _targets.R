
library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c(
  'arrow',
  'dataRetrieval',
  'geofacet',
  'lubridate',
  'maps',
  'scico',
  'sf',
  'tidyverse'
))

start_date <- '2010-01-01'
end_date <- '2021-12-31'

source('1_fetch.R')
source('2_process.R')
source('3_visualize.R')

c(p1_targets, p2_targets, p3_targets)
