
library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c(
    'arrow',
    'dataRetrieval',
    'EnvStats',
    'FlowScreen',
    'nhdplusTools',
    'qs',
    'randomForest',
    'raster',
    'sbtools',
    'sf',
    'tidyverse'
  ), 
  format =  'qs',
  workspace_on_error = TRUE
)

source('1_Download.R')
source('2_Prepare.R')
source('3_Filter.R')
source('4_EpisodicSalinization.R')
source('5_BaseflowSalinization.R')
source('6_DefineCharacteristics.R')

select <- dplyr::select # The raster pkg keeps overriding this one so make sure this is correct

c(p1_targets, p2_targets, p3_targets,
  p4_targets, p5_targets, p6_targets)
