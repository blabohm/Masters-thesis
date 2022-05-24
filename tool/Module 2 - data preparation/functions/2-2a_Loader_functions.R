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
# ua_dir = paste0(input_directory, "/UA2018/")
# city_code = cityCode
# boundary = boundaryLoader(city_boundaries = paste0(input_directory, "/cities.gpkg"),
#                           city_code = city_code)
# crs = 3035
# res_class = c(11100, 11210, 11220,
#               11230, 11240, 11300)
################################################################################

UAresLoader <- function(ua_dir, fua_code, boundary,
                        crs = 3035, res_class = c(11100, 11210, 11220,
                                                  11230, 11240, 11300))
{
  # required packages
  require(dplyr)
  require(sf)

  # get ua file with right code
  ua_file <- ua_dir %>%
    list.files(pattern = fua_code,
               full.names = TRUE) %>%
    list.files(pattern = "gpkg$",
               full.names = TRUE,
               recursive = TRUE)
  # get name of UA land-use layer
  lr <- st_layers(ua_file)$name[1]
  boundary <- boundary %>%
    st_geometry() %>%
    st_as_text()
  # user communication
  message("\n loading residential areas... \n")
  # load UA file and filter residential classes
  ua_file %>%
    st_read(lr, wkt_filter = boundary, quiet = TRUE) %>%
    filter(code_2018 %in% res_class) %>%
    st_transform(crs) %>%
    select(Pop2018, identifier, code_2018) %>%
    return()
}


################################################################################
# 2. LOAD OSM BUILDINGS
# REQUIRED SETTINGS:
# osm_file: Directory of OSM file
# OPTIONAL SETTINGS:
# crs: Desired crs - DEFAULT is ETRS3035
################################################################################

OSMloader <- function(osm_file, boundary, crs = 3035)
{
  # required packages
  require(dplyr)
  require(sf)
  boundary <- boundary %>%
    st_transform(4326) %>%
    st_geometry() %>%
    st_as_text()
  tmp <- osm_file %>%
    st_read(wkt_filter = boundary, quiet = TRUE) %>%
    st_make_valid() %>%
    filter(!st_is_empty(.)) %>%
    st_cast("MULTIPOLYGON", do_split = TRUE, warn = FALSE) %>%
    st_cast("POLYGON", do_split = TRUE, warn = FALSE)
  if (!exists("tmp")) return(NULL)
  tmp %>%
    st_transform(crs) %>%
    select(matches("build|geom")) %>%
    return()
}


################################################################################
# 3. LOAD NETWORK
# REQUIRED SETTINGS:
# network_dir: Directory of network file
# OPTIONAL SETTINGS:
# crs: Desired crs - DEFAULT is ETRS3035
################################################################################

networkLoader <- function(network_dir, osm_buildings = NULL, crs = 3035)
{
  #required packages
  require(dplyr)
  require(sf)
  # user communication
  message("\n loading osm network... \n")
  net <- network_dir %>%
    st_read(quiet = TRUE) %>%
    select(highway) %>%
    st_transform(crs)
  if (!is.null(osm_buildings)) {
    st_filter(net, sfc2bb(osm_buildings), .predicate = st_intersects) %>%
      return() } else return(net)
}


################################################################################
