
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
  'raster',
  'sbtools',
  'scico',
  'sf',
  'tidyverse',
  'usmap'
))

start_date <- '2010-01-01'
end_date <- '2021-12-31'

# Exclude these states because they are either missing or have zero 
# reported salt application rates as determined by Dugan et al., 2017
states_to_exclude <- c(
  # No data states 
  "MT", "WY", "UT", "AZ", "AR", "LA",
  "MS", "AL", "KY", "TN", "GA", "SC",
  # States with salt application rate = 0
  "WA", "OR", "ID", "NM", "TX", "FL", "NC"
)

source('1_fetch.R')
source('2_process.R')
source('3_visualize.R')

c(p1_targets, p2_targets, p3_targets)
