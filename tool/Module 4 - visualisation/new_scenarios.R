# PACKAGES
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

# INPUT DIRECTORIES
wd <- "C:/Users/labohben/Desktop/DE008/"
nodes <- paste0(wd, "nodes.gpkg")
edges <- paste0(wd, "edges.gpkg")

# PARK ID AND DISTANCE
id <- "23473-DE008L2"
d <- 2000

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
n <-  (st_length(lvp_outline) / 5) %>% round() %>% as.numeric()

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

net %>%
  mutate(edge_id = row_number()) %>%
  select(edge_id) %>%
  write_sf(paste0(out_dir, "edges.gpkg"))
edges_new <- paste0(out_dir, "edges.gpkg")

nodes %>%
  read_sf(wkt_filter = lvp_filter) %>%
  write_sf(paste0(out_dir, "nodes.gpkg"))
nodes_new <- paste0(out_dir, "nodes.gpkg")
index_dir <- paste0(out_dir, "indices/")
dir.create(index_dir)

# Base indices
gs_ids <- read_sf(nodes_new, wkt_filter = lvp_filter) %>%
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
net <- read_sf(edges_new)
be1 <- read_sf(nodes_new) %>%
  filter(population > 0)
out1 <- add_params(build_entries = be1, gs_entries = new_gse, network = net)
index_dir1 <- paste0(out_dir, "indices1/")
dir.create(index_dir1)
flist1 <- list.files(index_dir,
                     pattern = paste(gs_ids[!grepl(id, gs_ids)], collapse = "|"),
                     full.names = TRUE)
file.copy(flist1, index_dir1)
write_output(out1, network = net, out_dir = index_dir1, ID = id)

gatherDI(building_polygons = build_poly, index_dir = index_dir1,
         output_dir = paste0(out_dir, "di1.gpkg"))
gatherLS(edges = edges_new, index_dir = index_dir1,
         output_dir = paste0(out_dir, "ls1.gpkg"))


################################################################################
# Scenario 2 - Green space development
# REPLACE PARK ENTRIES WITH BUILDING ENTRIES
target_ids <- c("23502-DE008L2", "23493-DE008L2", "23485-DE008L2",
                "23508-DE008L2", "23509-DE008L2")
target_query <- paste0("SELECT * FROM nodes WHERE ",
                       paste0("identifier = '",
                              target_ids, "'", collapse = " OR "))
target_gs <- read_sf(nodes_new, query = target_query)

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
net <- read_sf(edges_new)
be2 <- read_sf(nodes_new) %>%
  filter(population > 0) %>%
  bind_rows(new_building_entries)
index_dir2 <- paste0(out_dir, "indices2/")
dir.create(index_dir2)
gs_ids2 <- gs_ids[!(gs_ids %in% target_ids)]
for (i in gs_ids2) {
  gse_q <- paste0("SELECT * FROM nodes WHERE identifier IS '", i, "'")
  gse <- read_sf(nodes_new, query = gse_q)
  out <- add_params(build_entries = be2, gs_entries = gse, network = net)
  write_output(out, network = net, out_dir = index_dir2, ID = i)
}

gatherDI(building_polygons = build_poly, index_dir = index_dir2,
         output_dir = paste0(out_dir, "di2.gpkg"))
gatherLS(edges = edges_new, index_dir = index_dir2,
         output_dir = paste0(out_dir, "ls2.gpkg"))


################################################################################
# Scenario 3 - Population increase
ua_pop <- read_sf(ua_dir, wkt_filter = lvp_filter, layer = ua_lyr) %>%
  filter(grepl("^11",  code_2018)) %>%
  st_drop_geometry()
hd_pop <- ua_pop %>%
  mutate(pop_per_m = Pop2018 / area) %>%
  group_by(code_2018) %>%
  summarise(pop_high = quantile(pop_per_m, .95))
be3 <- read_sf(nodes_new) %>%
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
write_sf(be3, paste0(wd, "scen3_be.gpkg"))
index_dir3 <- paste0(out_dir, "indices3/")
dir.create(index_dir3)
for (i in gs_ids) {
  gse_q <- paste0("SELECT * FROM nodes WHERE identifier IS '", i, "'")
  gse <- read_sf(nodes_new, query = gse_q)
  out <- add_params(build_entries = be3, gs_entries = gse, network = net)
  write_output(out, network = net, out_dir = index_dir3, ID = i)
}

gatherDI(building_polygons = build_poly, index_dir = index_dir3,
         output_dir = paste0(out_dir, "di3.gpkg"))
gatherLS(edges = edges_new, index_dir = index_dir3,
         output_dir = paste0(out_dir, "ls3.gpkg"))


################################################################################
# Scenario 4
# be: pop-increase + gs development
# gse: inside lvp_filter - lvp gse + new_gse
# network: normal
net <- read_sf(edges_new)
be4 <- bind_rows(be3, new_building_entries)

gse_q4 <- paste0("SELECT * FROM nodes WHERE identifier IS not null")
gse4 <- nodes_new %>%
  read_sf(query = gse_q4) %>%
  filter(!(identifier %in% c(id, target_ids)),
         area > 0) %>%
  bind_rows(new_gse)
gs_ids4 <- gs_ids2

index_dir4 <- paste0(out_dir, "indices4/")
dir.create(index_dir4)
for (i in gs_ids4) {
  gse <- filter(gse4, identifier == i)
  out <- add_params(build_entries = be4, gs_entries = gse, network = net)
  write_output(out, network = net, out_dir = index_dir4, ID = i)
}

gatherDI(building_polygons = build_poly, index_dir = index_dir4,
         output_dir = paste0(out_dir, "di4.gpkg"))
gatherLS(edges = edges_new, index_dir = index_dir4,
         output_dir = paste0(out_dir, "ls4.gpkg"))
