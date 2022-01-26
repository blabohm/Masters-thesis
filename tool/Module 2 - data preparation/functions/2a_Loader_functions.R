################################################################################
# MODULE 2 - DATA PREPARATION
# PART 2 - BUILDING ENTRIES
# LOADER FUNCTIONS
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################
# INPUT:
# - OSM BUILDING TILE
# - UA 2018
# - OSM NETWORK
#
# OUTPUT:
# - BUILDING ENTRIES (TILE) SNAPPED TO NETWORK
#
################################################################################
# FUNCTIONS:
# 1. loadUAres
#    -> LOAD URBAN ATLAS RESIDENTIAL AREAS
# 2. OSMloader
#    -> LOAD OSM DATA AN UNITING LAYERS
#
################################################################################
# 1. LOAD URBAN ATLAS RESIDENTIAL AREAS
# REQUIRED SETTINGS:
# ua_dir: DIRECTORY WHERE URBAN ATLAS FILES ARE STORED
# city_code: FUA CODE OF DESIRED CITY (WILL BE SEARCHED FOR IN ua_dir)
# OPTIONAL SETTINGS:
# crs: DESIRED CRS; DEFAULT IS ETRS3035
# res_class: DESIRED UA RESIDENTIAL CLASSES PROVIDED AS A STRING OF NUMBERS;
# DEFAULT: ALL UA RESIDENTIAL CLASSES
loadUAres <- function(ua_dir, city_code,
                      crs = 3035, res_class = c(11100, 11210, 11220,
                                                11230, 11240, 11300)) {
  #required packages
  require(dplyr)
  require(sf)
  # get ua file with right code
  ua_file <- ua_dir %>%
    list.files(pattern = cityCode,
               full.names = TRUE) %>%
    list.files(pattern = "gpkg$",
               full.names = TRUE,
               recursive = TRUE)
  # get name of UA land-use layer
  lr <- st_layers(ua_file)$name[1]
  # load UA file and filter residential classes
  ua <- ua_file %>%
    st_read(lr, quiet = TRUE) %>%
    filter(code_2018 %in% res_class) %>%
    st_transform(crs) %>%
    select(Pop2018, identifier, code_2018)
  return(ua)}

# 2. READ UA 2018 DATA
#    -> FILTER FOR RESIDENTIAL AREAS
