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
d <- 2000
nodes <- paste0(wd, "nodes.gpkg")
edges <- paste0(wd, "edges_new.gpkg")

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
         population_new = round(population / Pop2018 * Pop2018_new)) %>%
  transmute(identifier = NA,
            area = NA,
            population = population_new,
            ID,
            geom)

new_gse <- read_sf(nodes, wkt_filter = lvp_filter) %>%
  filter(!is.na(area))
net <- read_sf(edges, wkt_filter = lvp_filter)
out <- add_params(build_entries = be, gs_entries = new_gse, network = net)
out_dir <- paste0(wd, "scenario3/")
dir.create(out_dir)
write_output(out, network = net, out_dir = out_dir, ID = id)

gs_ids <- read_sf(nodes, wkt_filter = lvp_filter) %>%
  filter(!is.na(identifier),
         identifier != id) %>%
  pull(identifier) %>%
  unique()

flist <- list.files(paste0(wd, "indices/"),
                    pattern = paste(gs_ids, collapse = "|"),
                    full.names = TRUE)
file.copy(flist, out_dir)

build_poly <- paste0(wd, "buildings.gpkg")
gatherDI(building_polygons = build_poly, index_dir = out_dir, output_dir = paste0(out_dir, "di.gpkg"))
gatherLS(edges = edges, index_dir = out_dir, output_dir = paste0(out_dir, "ls.gpkg"))
