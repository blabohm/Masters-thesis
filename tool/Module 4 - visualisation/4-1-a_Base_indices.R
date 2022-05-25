# Scenario 3 - Population increase
library(dplyr)
library(sf)
library(sfnetworks)
library(ggplot2)
library(tidygraph)
library(stringr)
getwd() %>%
  paste0("/tool/Module 3 - index building/functions/") %>%
  list.files(pattern = "3.*\\.R", full.names = TRUE) %>%
  for (file in .) source(file)

wd <- "C:/Users/labohben/Desktop/DE008/"
id <- "23473-DE008L2"
d <- 1000
nodes <- paste0(wd, "nodes.gpkg")
edges <- paste0(wd, "edges_new.gpkg")

# ACTUAL PARK ENTRIES
lvp_query <- paste0("SELECT * FROM nodes WHERE identifier IS '", id, "'")
lvp_entries <- read_sf(nodes, query = lvp_query)
lvp_filter <- st_buffer(lvp_entries, d) %>% st_geometry() %>% st_as_text()

# BLEND NEW BUILDINGS TO NETWORK
#CALC INDICES
# be <- read_sf(nodes, wkt_filter = lvp_filter) %>%
#   filter(population > 0)
#
# new_gse <- read_sf(nodes, wkt_filter = lvp_filter) %>%
#   filter(!is.na(area))
#
# net <- read_sf(edges, wkt_filter = lvp_filter)
#
# out <- add_params(build_entries = be, gs_entries = new_gse, network = net)

# write_output(out, network = net, out_dir = out_dir, ID = id)

gs_ids <- read_sf(nodes, wkt_filter = lvp_filter) %>%
  filter(!is.na(identifier)) %>%
  pull(identifier) %>%
  unique()

# flist <- list.files(paste0(wd, "indices/"),
#                     pattern = paste(gs_ids, collapse = "|"),
#                     full.names = TRUE)
# file.copy(flist, out_dir)
out_dir <- paste0(wd, "base_indices/")
index_dir <- paste0(out_dir, "indices/")
dir.create(index_dir, recursive = TRUE)
file.copy(nodes, paste0(out_dir, "nodes.gpkg"))
file.copy(edges, paste0(out_dir, "edges.gpkg"))
calcIndices(green_space_IDs = gs_ids, in_directory = out_dir,
            out_directory = index_dir)

build_poly <- paste0(wd, "buildings.gpkg")
gatherDI(building_polygons = build_poly, index_dir = index_dir,
         output_dir = paste0(out_dir, "di.gpkg"))
gatherLS(edges = edges, index_dir = index_dir,
         output_dir = paste0(out_dir, "ls.gpkg"))
