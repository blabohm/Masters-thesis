library(ggplot2)
library(sf)
library(dplyr)

wd <- "D:/output/DE008/"
edges <- paste0(wd, "edges.gpkg")
id <- "23473-DE008L2"
gs_dir <- paste0(wd, "DE008L2_LEIPZIG_UA2018_v012.gpkg")
lvp_q <- paste0("SELECT area, geom FROM ",  st_layers(gs_dir)$name[1],
              " WHERE identifier LIKE '", id, "'")
lvp <- read_sf(gs_dir, query = lvp_q)
gs_q <- paste0("SELECT area, geom FROM ",  st_layers(gs_dir)$name[1],
                " WHERE code_2018 is 14100 OR code_2018 is 31000")
bbox <- read_sf("D:/output/DE008/bbox.gpkg") %>% st_bbox()
bbox_filter <- bbox %>% st_as_sfc() %>% st_as_text()

gs <- read_sf(gs_dir, query = gs_q, wkt_filter = bbox_filter)

build_poly <- paste0(wd, "buildings.gpkg")

base_di <- paste0(wd, "scenarios/di.gpkg")
scen1_di <- paste0(wd, "scenarios/di1_delta.gpkg")
scen2_di <- paste0(wd, "scenarios/di2_delta.gpkg")
scen3_di <- paste0(wd, "scenarios/di3_delta.gpkg")
scen4_di <- paste0(wd, "scenarios/di4_delta.gpkg")

base_ls <- paste0(wd, "scenarios/ls.gpkg")
scen1_ls <- paste0(wd, "scenarios/ls1_delta.gpkg")
scen2_ls <- paste0(wd, "scenarios/ls2_delta.gpkg")
scen3_ls <- paste0(wd, "scenarios/ls3_delta.gpkg")
scen4_ls <- paste0(wd, "scenarios/ls4_delta.gpkg")

base_plot <- ggplot() +
  geom_sf(data = read_sf(build_poly, wkt_filter = bbox_filter) %>% select(geom),
          fill = "white", color = "white") +
  geom_sf(data = gs, fill = "lightgrey", color = "lightgrey") +
  geom_sf(data = lvp, fill = "lightgreen", color = "lightgreen") +
  geom_sf(data = read_sf(edges, wkt_filter = bbox_filter) %>% select(geom),
          color = "darkgrey", size = 1)

di_query <- paste0("SELECT * FROM di WHERE di is not null")
di_plot <- base_plot +
  geom_sf(data = read_sf(base_di, query = di_query), aes(fill = di)) +
  scale_fill_distiller(palette = "BuGn") +
  coord_sf(xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4]))

ls_query <- paste0("SELECT * FROM ls WHERE ls is not null")
ls_plot <- base_plot +
  geom_sf(data = read_sf(base_ls, query = ls_query) %>% arrange(ls), aes(color = ls), size = 2) +
  scale_color_distiller(palette = "RdBu", trans = "log") +
  coord_sf(xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4]))

ls3_query <- paste0("SELECT * FROM ls3 WHERE ls is not null")
base_plot +
  geom_sf(data = read_sf(scen3_ls, query = ls3_query) %>% arrange(ls), aes(color = ls), size = 2) +
  scale_color_distiller(palette = "RdBu", trans = "log") +
  coord_sf(xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4]))

ggplot() +
  coord_equal(xlim = c(0, 3), ylim = c(0, 1), expand = FALSE) +
  annotation_custom(ggplotGrob(di_plot), xmin = 0, xmax = 1.5, ymin = 0, ymax = 1) +
  annotation_custom(ggplotGrob(ls_plot), xmin = 1.5, xmax = 3, ymin = 0, ymax = 1)

