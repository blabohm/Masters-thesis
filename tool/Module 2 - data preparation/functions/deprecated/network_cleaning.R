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
