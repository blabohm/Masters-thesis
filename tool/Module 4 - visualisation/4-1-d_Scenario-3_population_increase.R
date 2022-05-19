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

wd <- "D:/output/DE008/"
wd <- "C:/Users/labohben/Desktop/DE008/"
id <- "23473-DE008L2"
d <- 1000
nodes <- paste0(wd, "nodes.gpkg")
edges <- paste0(wd, "scenario1/edges.gpkg")

# ACTUAL PARK ENTRIES
lvp_query <- paste0("SELECT * FROM nodes WHERE identifier IS '", id, "'")
lvp_entries <- read_sf(nodes, query = lvp_query)
lvp_filter <- st_buffer(lvp_entries, d) %>% st_geometry() %>% st_as_text()

# REPLACE PARK ENTRIES WITH BUILDING ENTRIES
ua_dir <- paste0(wd, "DE008L2_LEIPZIG_UA2018_v013.gpkg")
ua_lyr <-  st_layers(ua_dir)$name[1]

ua_pop <- read_sf(ua_dir, wkt_filter = lvp_filter, layer = ua_lyr) %>%
  filter(grepl("^11",  code_2018)) %>%
  st_drop_geometry()
hd_pop <- ua_pop %>%
  mutate(pop_per_m = Pop2018 / area) %>%
  group_by(code_2018) %>%
  summarise(pop_high = quantile(pop_per_m, .95))


# BLEND NEW BUILDINGS TO NETWORK
#CALC INDICES
be <- read_sf(nodes, wkt_filter = lvp_filter) %>%
  filter(population > 0) %>%
  select(-c(area)) %>%
  mutate(identifier = str_extract(ID, ".*L2")) %>%
  left_join(ua_pop) %>%
  left_join(hd_pop) %>%
  mutate(Pop2018_new = area * pop_high,
         population_new = round(population / Pop2018 * Pop2018_new),
         population_new = ifelse(population_new < population,
                                 round(population * 1.2), population_new)) %>%
  transmute(identifier = NA,
            area = NA,
            population = population_new,
            ID,
            geom)

# new_gse <- read_sf(nodes, wkt_filter = lvp_filter) %>%
#   filter(!is.na(area))
# net <- edges %>%
#   read_sf(wkt_filter = lvp_filter) %>%
#   mutate(edge_id = row_number()) %>%
#   select(edge_id)
#out <- add_params(build_entries = be, gs_entries = new_gse, network = net)
out_dir <- paste0(wd, "scenario3/")
index_dir <- paste0(out_dir, "indices/")
dir.create(index_dir, recursive = TRUE)

nodes %>%
  read_sf() %>%
  filter(!(ID %in% be$ID)) %>%
  bind_rows(be) %>%
  write_sf(paste0(out_dir, "nodes.gpkg"))
#write_output(out, network = net, out_dir = out_dir, ID = id)
gs_ids <- read_sf(nodes, wkt_filter = lvp_filter) %>%
  filter(!is.na(identifier),
         identifier != id) %>%
  pull(identifier) %>%
  unique()
file.copy(edges, out_dir)
calcIndices(green_space_IDs = gs_ids, in_directory = out_dir,
            out_directory = index_dir)

# flist <- list.files(paste0(wd, "indices/"),
#                     pattern = paste(gs_ids, collapse = "|"),
#                     full.names = TRUE)
# file.copy(flist, out_dir)

build_poly <- paste0(wd, "buildings.gpkg")
gatherDI(building_polygons = build_poly, index_dir = index_dir,
         output_dir = paste0(out_dir, "di.gpkg"))
gatherLS(edges = edges, index_dir = index_dir,
         output_dir = paste0(out_dir, "ls.gpkg"))
