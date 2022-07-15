library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)
library(ggspatial)
library(cowplot)

wd <- "Z:/output/"
wd1 <- "Z:/input/"
ua <- "Z:/input/UA2018/"
codes <- c("PT001", "AT001")

code <- codes[1]

build_poly <- paste0(wd, code, "/buildings.gpkg")

boundaries <- paste0(wd1, "cities.gpkg")
ua_q_bound <- paste0("SELECT * FROM cities WHERE URAU_CODE = '", code, "'")
bound <- read_sf(boundaries, query = ua_q_bound)
bound_filter <- bound %>% st_as_sfc() %>% st_as_text()

# bbox <- read_sf(paste0(wd1, code, "_cov.gpkg")) %>% st_bbox()
# bbox_filter <- bbox %>% st_as_sfc() %>% st_as_text()

ua_dir <- list.files(ua, pattern = code, full.names = TRUE) %>%
  list.files(pattern = ".gpkg$", recursive = TRUE, full.names = TRUE)
ua_q <- paste0("SELECT * FROM ",  st_layers(ua_dir)$name[1])
#ua_data <- read_sf(ua_dir, query = ua_q, wkt_filter = bbox_filter)
ua_data_full <- read_sf(ua_dir, query = ua_q, wkt_filter = bound_filter)


buildings_full <- read_sf(build_poly, wkt_filter = bound_filter) %>% select(geom)
res_full <- filter(ua_data_full, grepl("^11.*", code_2018))
cov_res_full <- st_filter(res_full, buildings_full)
not_cov_res_full <- filter(res_full, !(identifier %in% cov_res_full$identifier))

ggplot() +
  geom_sf(data = bound, color = "grey30", fill = "grey70") +
  geom_sf(data = cov_res_full, color = "red3", fill = "red3") +
  geom_sf(data = not_cov_res_full, color = "grey50", fill = "grey50")

# base_plot <- ggplot() +
#   geom_sf(data = st_buffer(st_as_sfc(bbox), 1000), fill = "gray93") +
#   geom_sf(data = buildings, fill = "gray99", color = "gray99") +
#   geom_sf(data = gs, fill = "gray85", color = "gray85") +
#   geom_sf(data = read_sf(edges, wkt_filter = bbox_filter) %>% select(geom),
#           color = "gray80", size = 1)

base_plot

#
# ################################################################################
# ls_query <- paste0("SELECT * FROM local_significance WHERE ls is not null")
# ls_values <- read_sf(paste0(wd, code, "/local_significance.gpkg"),
#                      query = ls_query, wkt_filter = bbox_filter)
# ls_plot <- base_plot +
#   ls_values %>%
#   select(ls) %>%
#   filter(!is.na(ls)) %>%
#   mutate(ls = log(ls)) %>%
#   arrange(ls) %>%
#   geom_sf(data = ., aes(color = ls), size = 2) +
#   scale_color_distiller(palette = "RdBu") +
#   coord_sf(xlim = c(xmin, xmax),
#            ylim = c(ymin, ymax)) +
#   labs(color = "LS") +
#   annotation_scale(aes(style = "ticks"))
# ls_plot
#
# ################################################################################
#
# di_query <- paste0("SELECT * FROM detour_index WHERE di is not null")
# di_values <- read_sf(paste0(wd, code, "/detour_index.gpkg"),
#                      query = di_query, wkt_filter = bbox_filter)
#
# di_plot <- base_plot +
#   di_values %>%
#   select(di) %>%
#   filter(!is.na(di)) %>%
#   geom_sf(data = ., aes(fill = di, color = di)) +
#   scale_fill_distiller(palette = "RdBu") +
#   scale_color_distiller(palette = "RdBu") +
#   coord_sf(xlim = c(xmin, xmax),
#            ylim = c(ymin, ymax)) +
#   labs(fill = "DI", color = "DI") +
#   annotation_scale(aes(style = "ticks"))
# di_plot
