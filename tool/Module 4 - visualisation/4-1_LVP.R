library(dplyr)
library(sf)
library(sfnetworks)

wd <- "C:/Users/labohben/Desktop/DE008/"
ID <- "23473-DE008L2"
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
be <- read_sf(node_dir, wkt_filter = aq, query = nq)
net <- read_sf(net_dir, wkt_filter = aq) %>%
  as_sfnetwork()

plot(net)
plot(b$geom, col = "red", add = TRUE)
plot(be$geom, col = "blue", add = TRUE)
plot(gse$geom, col = "green", add = TRUE)

