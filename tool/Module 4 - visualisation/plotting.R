library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)
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

ls_values <- read_sf(paste0(wd, "scenarios/ls_values.gpkg"))

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
  geom_sf(data = select(ls_values, ls) %>% arrange(ls), aes(color = ls), size = 2) +
  scale_color_distiller(palette = "RdBu", trans = "log") +
  coord_sf(xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4]))

# bp <- brewer.pal(6, "Spectral")
n <- ls_values %>%
  filter(!is.na(d_ls1) ,
         d_ls1 != 0) %>%
  pull(d_ls1) %>%
  quantile(c(0, .05, .25, .5, .75, 1))# %>%
#   scales::rescale(to = c(0, 1))
ls1_plot <- base_plot +
  geom_sf(data = select(ls_values, d_ls1) %>% na.omit() %>%
            filter(d_ls1 != 0) %>% arrange(d_ls1),
          aes(color = d_ls1), size = 2) +
  scale_color_distiller(palette = "RdBu") +
#  scale_color_stepsn(colours = bp, values = n) +
  coord_sf(xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4]))

ggplot() +
  coord_equal(xlim = c(0, 3), ylim = c(0, 1), expand = FALSE) +
  annotation_custom(ggplotGrob(di_plot), xmin = 0, xmax = 1.5, ymin = 0, ymax = 1) +
  annotation_custom(ggplotGrob(ls_plot), xmin = 1.5, xmax = 3, ymin = 0, ymax = 1)

