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
d <- 1000
nodes <- paste0(wd, "nodes.gpkg")
edges <- paste0(wd, "edges.gpkg")

# ACTUAL PARK ENTRIES
lvp_query <- paste0("SELECT * FROM nodes WHERE identifier IS '", id, "'")
lvp_entries <- read_sf(nodes, query = lvp_query)
lvp_filter <- st_buffer(lvp_entries, d) %>% st_geometry() %>% st_as_text()

# REPLACE PARK ENTRIES WITH BUILDING ENTRIES
target_ids <- c("23502-DE008L2", "23493-DE008L2", "23485-DE008L2")
target_query <- paste0("SELECT * FROM nodes WHERE identifier = '",
                       target_ids, "'")
for(q in target_query) { tmp <- read_sf(nodes, query = q)
  if (q == first(target_query)) target_gs <- tmp else target_gs <- bind_rows(target_gs, tmp) }

ua_dir <- "Z:/input/UA2018/DE008L2_LEIPZIG_UA2018_v013/Data/DE008L2_LEIPZIG_UA2018_v013.gpkg"
ua_lyr <-  st_layers(ua_dir)$name[1]

pop_query <- paste0("SELECT Pop2018, area FROM ", ua_lyr, " WHERE code_2018 = 11100")
hd_pop <- read_sf(ua_dir, wkt_filter = lvp_filter,
                  query = pop_query)
hd_pop <- hd_pop %>%
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

new_gse <- read_sf(nodes, wkt_filter = lvp_filter) %>%
  filter(!is.na(area),
         !(identifier %in% target_ids))

out <- add_params(build_entries = be, gs_entries = new_gse, network = net)
out_dir <- paste0(wd, "scenario2/")
dir.create(out_dir)
write_output(out, network = net, out_dir = out_dir, ID = id)

gs_ids <- read_sf(nodes, wkt_filter = lvp_filter) %>%
  filter(!is.na(identifier),
         identifier != id) %>%
  pull(identifier) %>%
  unique()

lvp_dir <- paste0(wd, "lvp/")
flist <- list.files(paste0(wd, "indices/"),
                    pattern = paste(gs_ids, collapse = "|"),
                    full.names = TRUE)
file.copy(flist, out_dir)

build_poly <- paste0(wd, "buildings.gpkg")
gatherDI(building_polygons = build_poly, index_dir = out_dir, output_dir = paste0(out_dir, "di.gpkg"))
gatherLS(edges = edges, index_dir = out_dir, output_dir = paste0(out_dir, "ls.gpkg"))
