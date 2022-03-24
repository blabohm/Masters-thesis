################################################################################
# MODULE 2 - DATA PREPARATION
# PART 1 - NETWORK CLEANING
################################################################################

# INPUT:
# - CITY CODE (CHARACTER STRING WITH FIRST 5 DIGITS OF URAU CODE)
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
# boundaryFile <- "E:/citiesEurope/Cities.shp"
# netTileDir <- "E:/osm_paths/"
# # # FUA CITY CODE
# cityCode <- "DE001"
#
# net <- networkPrep(network_tile_dir = netTileDir,
#                    city_boundaries = boundaryFile,
#                    city_code = cityCode)
################################################################################

networkPrep <- function(city_boundaries,
                        city_code,
                        network_tile_dir,
                        output_directory)
{
  # LOAD PACKAGES AND FUNCTIONS
  require(dplyr, quietly = TRUE)
  getwd() %>%
    paste0("/tool/Module 2 - data preparation/functions/") %>%
    list.files(pattern = "2-1[A-Za-z].*\\.R|2_.*\\.R", full.names = TRUE) %>%
    for (file in .) source(file)
  # LOAD CITY CORE BOUNDARY
  city_boundary <- boundaryLoader(city_boundaries = city_boundaries,
                                  city_code = city_code,
                                  buffer_dist = 1000)
  # LIST NETWORK TILES FOR FUA CODE
  listTiles(network_tile_directory = network_tile_dir,
            city_code = city_code) %>%
    # LOAD AND COMBINE NETWORK TILES
    combinator(file_list = ., boundary = city_boundary,
               tmp_dir = output_directory) %>%
    # CLEAN NETWORK
    #    -> DOUBLE ENTRIES
    #    -> UNCONNECTED EDGES
    networkCleaner() %>%
    st_write(output_directory, quiet = TRUE, append = FALSE)
}
