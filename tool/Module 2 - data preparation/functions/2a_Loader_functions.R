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
# 1. UAresLoader
#    -> LOAD URBAN ATLAS RESIDENTIAL AREAS
# 2. OSMloader
#    -> LOAD OSM BUILDINGS - UNITING LAYERS
# 3. networkLoader
#    -> LOAD NETWORK AND CLIP TO TILE + 100 m BUFFER
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
UAresLoader <- function(ua_dir, city_code,
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


################################################################################
# 2. LOAD OSM BUILDINGS - UNITING LAYERS
OSMloader <- function(osm_file) {
  message("loading osm buildings... \n")
  # get name of OSM building layers (only using polygon and multipolygon layers)
  lr <- osm_file %>%
    st_layers(.) %>%
    .$name %>%
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
      tmp <- st_read(osm_file, i, quiet = TRUE)}
    if (i == first(lr)) osm <- tmp else osm <- bind_rows(osm, tmp)}
  # delete temporary object
  rm(tmp)
  message("filtering osm buildings... \n")
  # transform to EPSG 3035 and select necessary columns
  osm %>%
    st_transform(3035) %>%
    select(Pop2018, identifier, code_2018) %>%
    return()}


################################################################################
# 3. LOAD NETWORK AND CLIP TO TILE + 100 m BUFFER
networkLoader <- function(network_dir, crs = 3035) {
  message("loading osm network... \n")
  network_dir %>%
    st_read(quiet = TRUE) %>%
    select(highway) %>%
    st_transform(3035) %>%
    distinct() %>%
    return()}


################################################################################
