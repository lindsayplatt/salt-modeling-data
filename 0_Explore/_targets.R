
library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c(
  'arrow',
  'cowplot',
  'dataRetrieval',
  'geofacet',
  'grwat', # devtools::install_github('tsamsonov/grwat')
  'Kendall',
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

# Set query criteria
start_date <- '1950-01-01'
end_date <- '2022-12-31'
record_min_yrs <- 10
recent_date <- '2000-01-01' # Date cutoff for being considered "active" and having "recent" data
site_type <- 'ST'
data_service <- 'dv'

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
