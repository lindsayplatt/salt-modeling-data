
library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c(
    'archive',
    'arrow',
    'dataRetrieval',
    'dtwclust',
    'EGRET',
    'feasts',
    'FlowScreen',
    'nhdplusTools',
    'qs',
    'randomForest',
    'raster',
    'sbtools',
    'sf',
    'tidyverse',
    'tsibble'
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

# TODO: DELETE THESE LATER
source('4_ClusterTS.R')
source('5_DefineClusters.R')

select <- dplyr::select # The raster pkg keeps overriding this one so make sure this is correct

c(p1_targets, p2_targets, p3_targets,
  p4_targets, p5_targets, # TODO: DELETE THESE ONES!
  p4b_targets, p5b_targets, p6_targets)
