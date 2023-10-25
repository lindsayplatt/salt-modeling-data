
library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c(
    'arrow',
    'dataRetrieval',
    'tidyverse'
  ), 
  format =  'qs',
  workspace_on_error = TRUE
)

source('1_Download.R')
source('2_Prepare.R')
source('3_Filter.R')
source('4_ClusterTS.R')
source('5_DefineClusters.R')

c(p1_targets, p2_targets, p3_targets,
  p4_targets, p5_targets)
