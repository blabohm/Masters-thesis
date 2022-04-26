


require(sf, quietly = TRUE)
require(sfnetworks, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidygraph, quietly = TRUE)

getwd() %>%
  paste0("/tool/Module 3 - index building/functions/") %>%
  list.files(pattern = "3.*\\.R", full.names = TRUE) %>%
  for (file in .) source(file)
ID <- "23473-DE008L2"
wd <- "D:/output/DE008/"

gs_dir <- paste0(wd, "DE008L2_LEIPZIG_UA2018_v012.gpkg")
gse_dir <- paste0(wd, "green_space_entries.gpkg")
b_dir <- paste0(wd, "buildings.gpkg")
node_dir <- paste0(wd, "nodes.gpkg")
net_dir <- paste0(wd, "edges.gpkg")

gsq <- paste0("SELECT area, geom FROM ",  st_layers(gs_dir)$name[1],
              " WHERE identifier LIKE '", ID, "'")
gs <- read_sf(gs_dir, query = gsq)

gseq <- paste0("SELECT geom FROM green_space_entries
              WHERE identifier LIKE '", ID, "'")
gse <- read_sf(gse_dir, query = gseq)

aq <- gse %>%
  st_buffer(500) %>%
  st_union() %>%
  st_geometry() %>%
  st_as_text()

bq <- "SELECT * FROM 'osm_buildings' WHERE population is not null"
b <- read_sf(b_dir, wkt_filter = aq, query = bq)
nq <- "SELECT population, geom FROM 'nodes' WHERE population is not null"
be <- read_sf(node_dir, wkt_filter = aq, query = nq) %>%
  filter(population > 0)
net <- read_sf(net_dir, wkt_filter = aq) %>%
  mutate(weight = st_length(.))

net1 <- as_sfnetwork(net) %>% convert(to_undirected)
be$ne_id <- calc_OD_cost(build_entries = be, gs_entries = gse, sf_network = net1)
sp <- st_network_paths(net1, from = be$geom[301], gse$geom[be$ne_id[301]]) %>%
  pull(edge_paths) %>%
  unlist()

plot(gs$geom, col = "lightgreen")
plot(b$geom, col = "darkred", add = TRUE)
plot(net$geom, add = TRUE)
plot(be$geom, col = "blue", add = TRUE, lwd = 3)
plot(gse$geom, col = "orange", add = TRUE, pch = 3, cex = 2, lwd = 4)
plot(b$geom[300], add = TRUE, pch = 5, cex = 3, lwd = 4, col = "yellow")
plot(be$geom[301], add = TRUE, pch = 5, cex = 3, lwd = 4, col = "yellow")
plot(gse$geom[be$ne_id[301]], add = TRUE, pch = 5, cex = 3, lwd = 4, col = "yellow")
plot(net$geom[sp], add = TRUE, col = "red", lwd = 3)

sum(net$weight[sp])
st_distance(be$geom[301], gse$geom[be$ne_id[301]])

di <- 118 / 169
ls <- b$population[300] * gs$area / 169

gatherDI("D:/output/DE008/buildings.gpkg",
         "D:/output/DE008/lvp/",
         "D:/output/DE008/lvp/di.gpkg")

gatherLS("D:/output/DE008/edges.gpkg",
         "D:/output/DE008/lvp/",
         "D:/output/DE008/lvp/ls.gpkg")
