library(targets)
library(tidyverse)
library(sf)

library(raster)
tar_load(road_salt_2015_raster)
tar_load(q_sc_sites_sf)

# TODO: I DON'T BELIEVE THAT THIS EXTRACT METHOD IS WORKING

site1_xy <- slice(q_sc_sites_sf, 501) %>% st_coordinates()
site_road_salt_vals <- raster::extract(
  road_salt_2015_raster, 
  site1_xy, 
  buffer=10^6, # 1 km
  df=TRUE, 
  cellnumbers=T,
  layer = "road_salt_2015") %>% as_tibble()
site_buffer_cells <- rasterFromCells(road_salt_2015_raster, site_road_salt_vals$cells)
plot(site_buffer_cells)
points(site1_xy[1], site1_xy[2])

raster::extract(road_salt_2015_raster, st_coordinates(q_sc_sites_sf), df=TRUE)
summary(road_salt_2015_raster@data)
