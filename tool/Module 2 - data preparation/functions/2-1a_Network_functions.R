################################################################################
# MODULE 2 - DATA PREPARATION
# PART 1 - NETWORK CLEANING
# 1a - NETWORK FUNCTIONS
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################
#
# FUNCTIONS:
# 1. LOAD BOUNDARY FILE
#    -> Select city boundary by URAU-style city code
#    -> Buffer with 1 km
# 2. LIST NETWORK TILES
#    -> List files in network tile directory matching city code
# 3. LOAD AND COMBINE FILES FROM FILE LIST
#    -> Remove motorways
# 4. CLEANING NETWORK
#    -> Remove double entries, round coordinates, remove pseudo- and
#       unconnected edges, subdivide edges at unconnected nodes
#
################################################################################
# 1. LOAD BOUNDARY FILE
# REQUIRED SETTINGS:
# boundary_file: File containing boundaries of desired city and a column with
# city codes (beware of URAU / FUA code confusion)
# city_code: URAU code of desired city
# OPTIONAL SETTINGS:
# crs: Desired crs - DEFAULT is ETRS3035
################################################################################

# boundaryLoader <- function(boundary_file, city_code, crs = 3035)
# {
#   # load packages
#   require(dplyr, quietly = TRUE)
#   require(sf, quietly = TRUE)
#   # laod boundary file
#   boundary_file %>%
#     st_read(quiet = TRUE) %>%
#     select(code = matches("URAU_COD")) %>%
#     # filter for city core polygon
#     filter(substr(code, 1, 5) %in% city_code) %>%
#     st_cast("POLYGON") %>%
#     st_buffer(1000) %>%
#     st_transform(crs) %>%
#     return()
# }


################################################################################
# 2. LIST NETWORK TILES
# network_tile_directory: string of network tile directory
# city_code: string of city code
################################################################################

listTiles <- function(network_tile_directory, city_code)
{
  # load packages
  require(dplyr, quietly = TRUE)
  # list files and filter
  network_tile_directory %>%
    list.files(pattern = city_code, full.names = TRUE) %>%
    tibble(path = .) %>%
    filter(grepl(".gpkg$", path)) %>%
    pull(path)
}


################################################################################
# 3. LOAD AND COMBINE FILES FROM FILE LIST
# REQUIRED SETTINGS:
# file_list: vector of network tile file locations
# OPTIONAL SETTINGS:
# boundary: city boundary for intersection with network tile - DEFAULT NULL
# crs: Desired crs - DEFAULT is ETRS3035
################################################################################

combinator <- function(file_list, tmp_dir, boundary = NULL, crs = 3035)
{
  # load packages
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  # temp directory for temp storage of output
  if (grepl("//$", tmp_dir)) tmpOut <- tmp_dir else {
  tmpOut <- gsub("network_clean.gpkg", "", tmp_dir) %>%
    paste0("tmp-file.gpkg") }
  if (file.exists(tmpOut)) unlink(tmpOut)
  # converting boundary to wkt_filter
  if (!is.null(boundary)) boundary <- boundary %>%
      st_transform(4326) %>%
      st_geometry() %>%
      st_as_text()
  # User communication
  message("\n Starting network combination and filtering \n")
  # iterate through files
  for (file in file_list) {
    # get layer name
    lr <- st_layers(file)$name %>%
      grep("line", ., ignore.case = TRUE, value = TRUE)
    # read file
    tmp <- file %>%
      st_read(quiet = TRUE, layer = lr, wkt_filter = boundary) %>%
      st_transform(crs)
    # combine
    if (nrow(tmp) == 0) next else tmp %>%
      select(highway = matches("^highway$")) %>%
      filter(!grepl("motorway", highway)) %>%
      st_cast("LINESTRING", do_split = TRUE, warn = FALSE) %>%
      st_write(dsn = tmpOut, layer = "osm_paths",
               quiet = TRUE, append = TRUE)}
  # clear temp object
  rm(tmp)
  # read output and return
  output <- st_read(tmpOut, quiet = TRUE)
  unlink(tmpOut)
  return(output)
}


################################################################################
# 4. CLEANING NETWORK
# REQUIRED SETTINGS:
# network: sfc object of class LINESTRING containing city network
# OPTIONAL SETTINGS:
# crs: Desired crs - DEFAULT is ETRS3035
################################################################################

networkCleaner <- function(network, crs = 3035)
{
  # load packages
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(tidygraph, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)
  # User communication
  network <- network
  message("\n Starting network cleaning \n")
  # network cleaning
  network %>%
    # remove double entries
    distinct() %>%
    # make sure no MULTILINESTRINGS in network
    #st_cast("LINESTRING") %>%
    st_geometry() %>%
    # make sure point coordinates match
    lapply(function(x) round(x, 0)) %>%
    st_sfc(crs = 3035) %>%
    as_sfnetwork() %>%
    # subdivide edges at interior points
    convert(to_spatial_subdivision) %>%
    # remove pseudo nodes
    convert(to_spatial_smooth) %>%
    # remove unconnected edges
    filter(group_components() == 1) %>%
    activate("edges") %>%
    st_geometry() %>%
    st_as_sf() %>%
    rename(geom = x) %>%
    return()
}
