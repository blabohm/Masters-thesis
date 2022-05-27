# PACKAGES
library(dplyr)
library(sf)
library(sfnetworks)
library(ggplot2)
library(tidygraph)
getwd() %>%
  paste0("/tool/Module 3 - index building/functions/") %>%
  list.files(pattern = "3.*\\.R", full.names = TRUE) %>%
  for (file in .) source(file)

# INPUT DIRECTORIES
wd <- "D:/output/DE008/"
nodes <- paste0(wd, "nodes.gpkg")
edges <- paste0(wd, "edges.gpkg")

# PARK ID AND DISTANCE
id <- "23473-DE008L2"
d <- 1000

# ACTUAL PARK ENTRIES
lvp_query <- paste0("SELECT * FROM nodes WHERE identifier IS '", id, "'")
lvp_entries <- read_sf(nodes, query = lvp_query)
lvp_filter <- st_buffer(lvp_entries, d) %>% st_union() %>% st_geometry() %>% st_as_text()

# PARK ENTRIES EVERY 2M
gs_dir <- paste0(wd, "DE008L2_LEIPZIG_UA2018_v012.gpkg")
gsq <- paste0("SELECT area, geom FROM ",  st_layers(gs_dir)$name[1],
              " WHERE identifier LIKE '", id, "'")
gs <- read_sf(gs_dir, query = gsq)

# BLEND NEW GREEN SPACE ENTRIES TO NETWORK (FOR COMPARABILITY OF SCENARIOS)
lvp_outline <- read_sf(paste0(wd, "lvp_outline.gpkg")) %>% st_union() %>%
  st_cast("MULTILINESTRING") %>% st_as_sf()
n <-  (st_length(lvp_outline) / 10) %>% round() %>% as.numeric()

# BLEND NEW PARK ENTRIES INTO NETWORK
new_gse <- lvp_outline %>%
  st_sample(size = n, type = "regular") %>%
  st_cast("POINT") %>%
  st_as_sf() %>%
  mutate(area = gs$area,
         identifier = id,
         ID = NA,
         population = NA) %>%
  rename(geom = x)

net <- edges %>%
  read_sf(wkt_filter = lvp_filter) %>%
  as_sfnetwork() %>%
  convert(to_undirected) %>%
  st_network_blend(new_gse) %>%
  activate("edges") %>%
  st_as_sf()

out_dir <- paste0(wd, "scenarios/")
dir.create(out_dir)

edges %>%
  read_sf() %>%
  filter(!(edge_id %in% net$edge_id)) %>%
  bind_rows(net) %>%
  mutate(edge_id = row_number()) %>%
  select(edge_id) %>%
  write_sf(paste0(out_dir, "edges.gpkg"))
edges <- paste0(out_dir, "edges.gpkg")

file.copy(nodes, out_dir)
index_dir <- paste0(out_dir, "indices/")
dir.create(index_dir)

# Base indices
gs_ids <- read_sf(nodes, wkt_filter = lvp_filter) %>%
  filter(!is.na(identifier)) %>%
  pull(identifier) %>%
  unique()

calcIndices(green_space_IDs = gs_ids, in_directory = out_dir,
            out_directory = index_dir)
build_poly <- paste0(wd, "buildings.gpkg")
gatherDI(building_polygons = build_poly, index_dir = index_dir,
         output_dir = paste0(out_dir, "di.gpkg"))
gatherLS(edges = edges, index_dir = index_dir,
         output_dir = paste0(out_dir, "ls.gpkg"))


################################################################################
# Scenario 1 - Unlimited access
be <- read_sf(nodes, wkt_filter = lvp_filter) %>%
  filter(population > 0)
out <- add_params(build_entries = be, gs_entries = new_gse, network = net)
index_dir1 <- paste0(out_dir, "indices1/")
dir.create(index_dir1)
flist <- list.files(index_dir,
                    pattern = paste(gs_ids[!grepl(id, gs_ids)], collapse = "|"),
                    full.names = TRUE)
file.copy(flist, index_dir1)
write_output(out, network = net, out_dir = index_dir1, ID = id)

gatherDI(building_polygons = build_poly, index_dir = index_dir1,
         output_dir = paste0(out_dir, "di1.gpkg"))
gatherLS(edges = edges, index_dir = index_dir1,
         output_dir = paste0(out_dir, "ls1.gpkg"))


################################################################################
# Scenario 2
# REPLACE PARK ENTRIES WITH BUILDING ENTRIES
target_ids <- c("23502-DE008L2", "23493-DE008L2", "23485-DE008L2",
                "23508-DE008L2", "23509-DE008L2")
target_query <- paste0("SELECT * FROM nodes WHERE ",
                       paste0("identifier = '",
                              target_ids, "'", collapse = " OR "))
target_gs <- read_sf(nodes, query = target_query)

ua_dir <- paste0(wd, "/DE008L2_LEIPZIG_UA2018_v013.gpkg")
ua_lyr <-  st_layers(ua_dir)$name[1]
pop_query <- paste0("SELECT Pop2018, area FROM ", ua_lyr, " WHERE code_2018 = 11100")
hd_pop <- ua_dir %>%
  read_sf(wkt_filter = lvp_filter, query = pop_query) %>%
  mutate(pop_per_m = Pop2018 / area)
pop_per_m_95 <- quantile(hd_pop$pop_per_m, probs = .95)

new_building_entries <- target_gs %>%
  group_by(identifier) %>%
  mutate(population = round(area * pop_per_m_95 / n()),
         ID = identifier,
         area = NA,
         identifier = NA)

# BLEND NEW BUILDINGS TO NETWORK
#CALC INDICES
be <- read_sf(nodes, wkt_filter = lvp_filter) %>%
  filter(population > 0) %>%
  bind_rows(new_building_entries)
index_dir2 <- paste0(out_dir, "indices2/")
dir.create(index_dir2)
gs_ids2 <- gs_ids[!(gs_ids %in% target_ids)]
for (i in gs_ids2) {
  gs <- read_sf(nodes, wkt_filter = lvp_filter) %>%
    filter(identifier == i)
  out <- add_params(build_entries = be, gs_entries = gs, network = net)
  write_output(out, network = net, out_dir = index_dir2, ID = i)
}

appendDI(building_polygons = build_poly, index_dir = index_dir2,
         output_dir = paste0(out_dir, "di.gpkg"), lyr = "scenario_2")
appendLS(edges = edges, index_dir = index_dir2,
         output_dir = paste0(out_dir, "ls.gpkg"), lyr = "scenario_2")


################################################################################
# Scenario 3