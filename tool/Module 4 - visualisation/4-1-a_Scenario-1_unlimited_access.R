# Scenario 1 - Unlimited access
library(dplyr)
library(sf)
library(sfnetworks)
library(ggplot2)
library(tidygraph)
getwd() %>%
  paste0("/tool/Module 3 - index building/functions/") %>%
  list.files(pattern = "3.*\\.R", full.names = TRUE) %>%
  for (file in .) source(file)

wd <- "C:/Users/labohben/Desktop/DE008/"
id <- "23473-DE008L2"
d <- 2000
nodes <- paste0(wd, "nodes.gpkg")
edges <- paste0(wd, "edges.gpkg")

# ACTUAL PARK ENTRIES
lvp_query <- paste0("SELECT * FROM nodes WHERE identifier IS '", id, "'")
lvp_entries <- read_sf(nodes, query = lvp_query)
lvp_filter <- st_buffer(lvp_entries, d) %>% st_geometry() %>% st_as_text()

# PARK ENTRIES EVERY 2M
gs_dir <- paste0(wd, "DE008L2_LEIPZIG_UA2018_v012.gpkg")
gsq <- paste0("SELECT area, geom FROM ",  st_layers(gs_dir)$name[1],
              " WHERE identifier LIKE '", id, "'")
gs <- read_sf(gs_dir, query = gsq)

lvp_outline <- wd %>%
  paste0("lvp_outline.gpkg") %>%
  read_sf() %>%
  st_union() %>%
  st_cast("MULTILINESTRING", warn = FALSE)

n <-  (st_length(lvp_outline) / 5) %>% round() %>% as.numeric()

# BLEND NEW PARK ENTRIES INTO NETWORK
new_gse <- lvp_outline %>%
  st_sample(size = n, type = "regular") %>%
  st_cast("POINT") %>%
  st_as_sf() %>%
  mutate(area = gs$area) %>%
  rename(geom = x)
write_sf(new_gse, paste0(wd, "new_gse.gpkg"))
net <- edges %>%
  read_sf(wkt_filter = lvp_filter) %>%
  as_sfnetwork() %>%
  convert(to_undirected) %>%
  st_network_blend(new_gse) %>%
  activate("edges") %>%
  st_as_sf()

#CALC INDICES
be <- read_sf(nodes, wkt_filter = lvp_filter) %>%
  filter(population > 0)

out <- add_params(build_entries = be, gs_entries = new_gse, network = net)
out_dir <- paste0(wd, "scenario1/")
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
gatherDI(building_polygons = build_poly, index_dir = out_dir,
         output_dir = paste0(out_dir, "di.gpkg"))
gatherLS(edges = edges, index_dir = out_dir,
         output_dir = paste0(out_dir, "ls.gpkg"))

