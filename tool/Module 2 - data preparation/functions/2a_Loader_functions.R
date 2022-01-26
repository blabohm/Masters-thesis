################################################################################
# MODULE 2 - DATA PREPARATION
# PART 2 - BUILDING ENTRIES
# 2a - LOADER FUNCTIONS
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################
#
# FUNCTIONS:
# 1. UAresLoader
#    -> Load urban atlas residential areas
# 2. OSMloader
#    -> Load OSM buildings - uniting layers
# 3. networkLoader
#    -> Load network
#
################################################################################
# 1. LOAD URBAN ATLAS RESIDENTIAL AREAS
# REQUIRED SETTINGS:
# ua_dir: Directory of urban atlas files
# city_code: FUA code of desired city (will be searched for in ua_dir)
# OPTIONAL SETTINGS:
# crs: Desired crs - DEFAULT is ETRS3035
# res_class: Desired urban atlas residential class-codes. Provided as vector of
# numbers - DEFAULT is all UA residential classes
################################################################################

UAresLoader <- function(ua_dir, city_code,
                        crs = 3035, res_class = c(11100, 11210, 11220,
                                                  11230, 11240, 11300)) {
  # required packages
  require(dplyr)
  require(sf)
  # user communication
  message("\n loading residential areas... \n")
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


################################################################################
# 2. LOAD OSM BUILDINGS
# REQUIRED SETTINGS:
# osm_file: Directory of OSM file
# OPTIONAL SETTINGS:
# crs: Desired crs - DEFAULT is ETRS3035
################################################################################

OSMloader <- function(osm_file, crs = 3035) {
  # required packages
  require(dplyr)
  require(sf)
  # user communication
  message("\n loading osm buildings... \n")
  # get name of OSM building layers (only using polygon and multipolygon layers)
  lr <- st_layers(osm_file)$name %>%
    grep("poly", ., ignore.case = TRUE, value = TRUE)
  # load individual layers
  for (i in lr) {
    # load OSM file and combine layers
    if (grepl("osm_multipolygons", i)) {
      tmp <- osm_file %>%
        st_read(i, quiet = TRUE) %>%
        st_make_valid() %>%
        st_cast("POLYGON")
    } else if (grepl("osm_polygons", i)) {
      tmp <- osm_file %>%
        st_read(i, quiet = TRUE) %>%
        st_make_valid() }
    if (i == first(lr)) osm <- tmp else osm <- bind_rows(osm, tmp)}
  # delete temporary object
  rm(tmp)
  # transform to EPSG 3035 and drop columns
  osm %>%
    st_transform(crs) %>%
    select(building, geom) %>%
    return() }


################################################################################
# 3. LOAD NETWORK
# REQUIRED SETTINGS:
# network_dir: Directory of network file
# OPTIONAL SETTINGS:
# crs: Desired crs - DEFAULT is ETRS3035
################################################################################

networkLoader <- function(network_dir, crs = 3035) {
  #required packages
  require(dplyr)
  require(sf)
  # user communication
  message("\n loading osm network... \n")
  network_dir %>%
    st_read(quiet = TRUE) %>%
    select(highway) %>%
    st_transform(crs) %>%
    distinct() %>%
    return()}


################################################################################
