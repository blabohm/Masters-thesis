library(dplyr)
library(sf)
library(ggplot2)



################################################################################
# SNAP BUILDING / PARK CENTROIDS TILE-WISE TO STREET NETWORK
baseDir <- "C:/Berlin/"
cityCode <- "DE001"
source("C:/Users/labohben/Documents/GitHub/MA/functions/centBuild0_1.R")


################################################################################
# LOAD RESIDENTIAL AREAS (UA)
ua_res <- 
  paste0(baseDir,"UA2018/") %>% 
  loadUAres(cityCode)


################################################################################
# LOAD STREET NETWORK (OSM)
network_dir <- 
  paste0(baseDir, "highway_clean.gpkg") 


################################################################################
# LOAD BUILDINGS (OSM)
building_dirs <- 
  paste0(baseDir,"osm_buildings_temp/") %>% 
  list.files(pattern = cityCode, 
             full.names = TRUE)


################################################################################
# CALCULATE POP / BUILDING (OSM + UA)

# urban atlas (ua) residential classes
res_class <- c(11100, 11210, 11220, 
               11230, 11240, 11300)

# CLIP STREET NETWORK TO BUILDING LAYER + 100(?) m
# SNAP 