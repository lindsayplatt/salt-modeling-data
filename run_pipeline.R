# Use this to start a background job in RStudio that will run the full pipeline

# RUN ORIGINAL, PRE-PROPOSAL CODE
wd <- getwd()
setwd('0_Explore')
library(targets)
tar_make()
setwd(wd)

# RUN CURRENT PIPELINE, POST-PROPOSAL
# library(targets)
# tar_make()
