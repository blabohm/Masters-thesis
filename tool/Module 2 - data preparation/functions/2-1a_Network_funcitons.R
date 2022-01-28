################################################################################
# MODULE 'NUMBER' - MODULE NAME
# PART 'NUMBER' - SUBMODULE NAME
# PART 'NUMBER' + letter - FUNCTIONS THEME
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################
#
# FUNCTIONS:
# 1. FUNCTION NAME
#    -> Function description
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

boundaryLoader <- function(boundary_file, city_code, crs = 3035)
{
  # load packages
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  # laod boundary file
  boundary_file %>%
    st_read(quiet = TRUE) %>%
    select(code = matches("URAU_COD")) %>%
    # filter for city core polygon
    filter(substr(code, 1, 5) %in% city_code) %>%
    st_cast("POLYGON") %>%
    st_buffer(1000) %>%
    st_transform(crs) %>%
    return()
}


################################################################################
# 2. LIST NETWORK TILES
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
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
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################

combinator <- function(file_list, boundary = NULL, crs = 3035)
{
  # load packages
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  # make sure output is empty
  outSF <- NULL
  # iterate through files
  for (file in file_list) {
    # check layers
    lr <- st_layers(file)$name %>%
      grep("line", .,
           ignore.case = TRUE, value = TRUE)
    # read file
    tmp <- file %>%
      st_read(quiet = TRUE, layer = lr[1]) %>%
      st_transform(crs)
    # check if tmp is inside boundary
    isIn <- ifelse(is.null(boundary), TRUE,
                   any(st_intersects(tmp$geom, boundary, sparse = FALSE)))
    # combine
    if (isIn) outSF <- ifelse(is.null(outSF),
                              st_read(file, quiet = TRUE),
                              st_read(file, quiet = TRUE) %>%
                                bind_rows(outSF)) }
  rm(tmp)
  return(outSF)
}


################################################################################
library(dplyr)
library(sf)
library(igraph)
library(tidygraph)
library(sfnetworks)

baseDir <- "C:/Berlin/"

net_clean <-
  paste0(baseDir, "network_clean.gpkg") %>%
  st_read(quiet = TRUE) %>%
  select(highway) %>%
  st_transform(3035) %>%
  distinct() %>%
  st_cast("LINESTRING") %>%
  as_sfnetwork() %>%
  filter(group_components() == 1) %>%
  activate("edges") %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_geometry() %>%
  lapply(function(x) round(x, 0)) %>%
  st_sfc(crs = 3035) %>%
  as_sfnetwork() %>%
  convert(to_spatial_smooth) %>%
  convert(to_spatial_subdivision)
