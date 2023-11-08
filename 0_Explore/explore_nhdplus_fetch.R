# Is it better to map over COMIDs, vars, or both??
# Based on test below, the winner is map over attributes but pass in all comids

library(nhdplusTools)
library(purrr)
attrs <- c('CAT_BFI', 'TOT_BFI', 'ACC_BFI')
comids <- c(6333538L, 2048005L, 3788105L, 2087785L, 3468457L, 
            22846155L, 17930376L, 1508043L, 1520083L, 8272527L)

# Elapsed time = 8.82
system.time({
  x<-purrr::map(attrs, get_catchment_characteristics, ids = comids)
})

# Elapsed time = 112.76
system.time({
  y<-purrr::map(comids, get_catchment_characteristics, varname = attrs)
})

# Elapsed time = 91.58 
combos <- expand.grid(comid = comids, attr = attrs)
system.time({
  z<-purrr::map2(combos$comid, combos$attr, ~get_catchment_characteristics(varname=.y, ids=.x))
})


