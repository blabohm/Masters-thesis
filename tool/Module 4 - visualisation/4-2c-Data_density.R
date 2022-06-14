library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)
library(ggspatial)

wd <- "Z:/output/"
wd1 <- "Z:/input/"
ua <- "Z:/input/UA2018/"
codes <- c("PT001", "AL001")

edges <- paste0(wd, code, "/edges.gpkg")
gs_q <- paste0("SELECT area, geom, identifier FROM ",  st_layers(ua_dir)$name[1],
               " WHERE code_2018 is 14100 OR code_2018 is 31000")

build_poly <- paste0(wd, code, "/buildings.gpkg")

bbox <- read_sf(paste0(wd1, code, "_cov.gpkg")) %>% st_bbox()
bbox_filter <- bbox %>% st_as_sfc() %>% st_as_text()

ua_dir <- list.files(ua, pattern = code, full.names = TRUE) %>%
  list.files(pattern = ".gpkg$", recursive = TRUE, full.names = TRUE)
ua_q <- paste0("SELECT * FROM ",  st_layers(ua_dir)$name[1])
ua <- read_sf(ua_dir, query = ua_q, wkt_filter = bbox_filter)

gs <- filter(ua, code_2018 %in% c(14100, 31000))
res <- filter(ua, grepl("^11.*", code_2018))
buildings <- read_sf(build_poly, wkt_filter = bbox_filter) %>% select(geom)

xmin <- bbox[1] + 2100
xmax <- bbox[3] - 2100
ymin <- bbox[2] + 2100
ymax <- bbox[4] - 2100

build_plot <- ggplot() +
  geom_sf(data = res, color = "red3", fill = "red3") +
  geom_sf(data = buildings, color = "black") +
  coord_sf(xlim = c(xmin, xmax),
           ylim = c(ymin, ymax))
build_plot

base_plot <- ggplot() +
  geom_sf(data = st_buffer(st_as_sfc(bbox), 1000), fill = "gray93") +
  geom_sf(data = buildings, fill = "gray99", color = "gray99") +
  geom_sf(data = gs, fill = "gray85", color = "gray85") +
  geom_sf(data = read_sf(edges, wkt_filter = bbox_filter) %>% select(geom),
          color = "gray80", size = 1)

base_plot


################################################################################
ls_query <- paste0("SELECT * FROM local_significance WHERE ls is not null")
ls_values <- read_sf(paste0(wd, code, "/local_significance.gpkg"),
                     query = ls_query, wkt_filter = bbox_filter)
ls_plot <- base_plot +
  ls_values %>%
  select(ls) %>%
  filter(!is.na(ls)) %>%
  mutate(ls = log(ls)) %>%
  arrange(ls) %>%
  geom_sf(data = ., aes(color = ls), size = 2) +
  scale_color_distiller(palette = "RdBu") +
  coord_sf(xlim = c(xmin, xmax),
           ylim = c(ymin, ymax)) +
  labs(color = "LS") +
  annotation_scale(aes(style = "ticks"))
ls_plot

################################################################################

di_query <- paste0("SELECT * FROM detour_index WHERE di is not null")
di_values <- read_sf(paste0(wd, code, "/detour_index.gpkg"),
                     query = di_query, wkt_filter = bbox_filter)

di_plot <- base_plot +
  di_values %>%
  select(di) %>%
  filter(!is.na(di)) %>%
  geom_sf(data = ., aes(fill = di, color = di)) +
  scale_fill_distiller(palette = "RdBu") +
  scale_color_distiller(palette = "RdBu") +
  coord_sf(xlim = c(xmin, xmax),
           ylim = c(ymin, ymax)) +
  labs(fill = "DI", color = "DI") +
  annotation_scale(aes(style = "ticks"))
di_plot
