################################################################################
# MODULE 2 - DATA PREPARATION
# PART 1 - NETWORK CLEANING
################################################################################

# INPUT:
# - NETWORK TILES FOR FUA
# - BOUNDARY LAYER OF CITY CORE

# OUTPUT:
# - CLEAN NETWORK

################################################################################
# OVERVIEW:
# LOAD CITY CORE BOUNDARY
# LIST NETWORK TILES FOR CITY CODE
# LOAD AND COMBINE NETWORK TILES
#    -> CHECK IF TILE IS INSIDE CITY CORE
#    -> COMBINE NETWORK TILES
# CLEAN NETWORK
#    -> DOUBLE ENTRIES
#    -> UNCONNECTED EDGES
################################################################################
# INPUT VALUES FOR TESTING CODE
# DATA DIRECTORIES FOR UA AND OSM DATA
boundaryFile <- "E:/citiesEurope/Cities.shp"
netTileDir <- "E:/osm_paths/"
# FUA CITY CODE
cityCode <- "DE001"

#buildingEntries(osm_directory = osm_dir, ua_directory = ua_dir,
#                city_code = cityCode)
################################################################################
# LOAD PACKAGES AND FUNCTIONS
require(dplyr, quietly = TRUE)
getwd() %>%
  paste0("/tool/Module 2 - data preparation/functions/") %>%
  list.files(pattern = "2-1[A-Za-z].*\\.R", full.names = TRUE) %>%
  for (file in .) source(file)
# LOAD CITY CORE BOUNDARY
cityBoundary <- boundaryLoader(boundaryFile, cityCode)
# LIST NETWORK TILES FOR FUA CODE
netTiles <- listTiles(netTileDir, cityCode)
# LOAD AND COMBINE NETWORK TILES
network <- combinator(netTiles, cityBoundary) %>%
  # CLEAN NETWORK
  #    -> DOUBLE ENTRIES
  #    -> UNCONNECTED EDGES
  networkCleaner(network)

