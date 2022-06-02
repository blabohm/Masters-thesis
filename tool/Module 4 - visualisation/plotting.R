library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)
library(ggspatial)
wd <- "C:/Users/labohben/Desktop/DE008/"
edges <- paste0(wd, "edges.gpkg")
id <- "23473-DE008L2"
gs_dir <- paste0(wd, "DE008L2_LEIPZIG_UA2018_v012.gpkg")
lvp_q <- paste0("SELECT area, geom FROM ",  st_layers(gs_dir)$name[1],
              " WHERE identifier LIKE '", id, "'")
lvp <- read_sf(gs_dir, query = lvp_q)
gs_q <- paste0("SELECT area, geom, identifier FROM ",  st_layers(gs_dir)$name[1],
                " WHERE code_2018 is 14100 OR code_2018 is 31000")

build_poly <- paste0(wd, "buildings.gpkg")
ls_values <- read_sf(paste0(wd, "scenarios/ls_values.gpkg"))

bbox <- ls_values %>% st_bbox()
bbox_filter <- bbox %>% st_as_sfc() %>% st_as_text()
xmin <- bbox[1] + 1100
xmax <- bbox[3] - 1100
ymin <- bbox[2] + 1100
ymax <- bbox[4] - 1100
gs <- read_sf(gs_dir, query = gs_q, wkt_filter = bbox_filter)


base_plot <- ggplot() +
  geom_sf(data = st_buffer(st_as_sfc(bbox), 1000), fill = "gray93") +
  geom_sf(data = read_sf(build_poly, wkt_filter = bbox_filter) %>% select(geom),
          fill = "gray99", color = "gray99") +
  geom_sf(data = gs, fill = "gray85", color = "gray85") +
  geom_sf(data = lvp, fill = "gray75", color = "gray75") +
  geom_sf(data = read_sf(edges, wkt_filter = bbox_filter) %>% select(geom),
          color = "gray80", size = 1)

base_plot
################################################################################
ls_query <- paste0("SELECT * FROM ls WHERE ls is not null")
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
  labs(title = "Lene Voigt Park, Leipzig \nLocal Significance (LS)",
       color = "LS") +
  annotation_scale(aes(style = "ticks"))
ls_plot

################################################################################
# display.brewer.pal(5, "RdBu")
# bp <- brewer.pal(5, "RdBu")
# n <- ls_values %>%
#   filter(!is.na(d_ls1) ,
#          d_ls1 != 0) %>%
#   pull(d_ls1) %>%
#   quantile(c(0, .05, .25, .5, .75, 1))# %>%
#n <- scales::rescale(c(1500000, 500000, 0, -250000, -500000), to = c(0, 1))

mk_log <- function(numbers)
{
  tibble(numbers) %>%
    mutate(num_log = log(abs(numbers))) %>%
    mutate(num_log = case_when(numbers == 0 ~ 0,
                               numbers > 0 ~ num_log,
                               numbers < 0 ~ num_log * -1,
                               TRUE ~ num_log)) %>%
    pull(num_log)
}


################################################################################
ls1_plot <- base_plot +
  ls_values %>%
  select(d_ls1) %>%
  filter(!is.na(d_ls1), d_ls1 != 0) %>%
  arrange(d_ls1) %>%
  mutate(d_ls1 = mk_log(d_ls1)) %>%
  geom_sf(data = ., aes(color = d_ls1), size = 2) +
  scale_color_distiller(palette = "RdBu") +
#  scale_color_stepsn(colours = bp, values = n) +
#  scale_color_steps2(low = bp[4], mid = bp[3], high = bp[1], midpoint = 0) +
  coord_sf(xlim = c(xmin + 650, xmax - 400),
           ylim = c(ymin + 400, ymax - 300)) +
  labs(title = "Scenario 1: Unlimited access",
       color = expression(Delta ~ "LS")) +
  annotation_scale(aes(style = "ticks"))
ls1_plot

################################################################################
target_ids <- c("23502-DE008L2", "23493-DE008L2", "23485-DE008L2",
                "23508-DE008L2", "23509-DE008L2")

ls2_plot <- base_plot +
  geom_sf(data = filter(gs, identifier %in% target_ids), aes(fill = "")) +
  ls_values %>%
  select(d_ls2) %>%
  filter(!is.na(d_ls2), d_ls2 != 0) %>%
  arrange(d_ls2) %>%
  mutate(d_ls2 = mk_log(d_ls2)) %>%
  geom_sf(data = ., aes(color = d_ls2), size = 2) +
  scale_color_distiller(palette = "RdBu") +
  coord_sf(xlim = c(xmin + 600, xmax - 450),
           ylim = c(ymin + 700, ymax + 100)) +
  labs(title = "Scenario 2: Green space development",
       color = expression(Delta ~ "LS"),
       fill = "Developed \ngreen spaces") +
  scale_fill_manual(aesthetics = c(color = "brown2")) +
  annotation_scale(aes(style = "ticks"))
ls2_plot

################################################################################
ls3_plot <- base_plot +
  ls_values %>%
  select(d_ls3) %>%
  filter(!is.na(d_ls3), d_ls3 != 0) %>%
  arrange(d_ls3) %>%
  mutate(d_ls3 = mk_log(d_ls3)) %>%
  geom_sf(data = ., aes(color = d_ls3), size = 2) +
  scale_color_distiller(palette = "RdBu") +
  coord_sf(xlim = c(xmin, xmax),
           ylim = c(ymin, ymax)) +
  labs(title = "Scenario 3: Population increase",
       color = expression(Delta ~ "LS")) +
  annotation_scale(aes(style = "ticks"))
ls3_plot

################################################################################
ls4_plot <- base_plot +
  ls_values %>%
  select(d_ls4) %>%
  filter(!is.na(d_ls4), d_ls4 != 0) %>%
  arrange(d_ls4) %>%
  mutate(d_ls4 = mk_log(d_ls4)) %>%
  geom_sf(data = ., aes(color = d_ls4), size = 2) +
  scale_color_distiller(palette = "RdBu") +
  coord_sf(xlim = c(xmin, xmax),
           ylim = c(ymin, ymax)) +
  labs(title = "Scenario 4: Ensemble model",
       color = expression(Delta ~ "LS")) +
  annotation_scale(aes(style = "ticks"))

ls4_plot
# ggplot() +
#   coord_equal(xlim = c(0, 3), ylim = c(0, 1), expand = FALSE) +
#   annotation_custom(ggplotGrob(di_plot), xmin = 0, xmax = 1.5, ymin = 0, ymax = 1) +
#   annotation_custom(ggplotGrob(ls_plot), xmin = 1.5, xmax = 3, ymin = 0, ymax = 1)


################################################################################
################################################################################
di_values <- read_sf(paste0(wd, "scenarios/di_values.gpkg"))

di_plot <- base_plot +
  di_values %>%
  select(di) %>%
  filter(!is.na(di)) %>%
  geom_sf(data = ., aes(fill = di, color = di)) +
  scale_fill_distiller(palette = "RdBu") +
  scale_color_distiller(palette = "RdBu") +
  coord_sf(xlim = c(xmin, xmax),
           ylim = c(ymin, ymax)) +
  labs(title = "Lene Voigt Park, Leipzig \nDetour index (DI)",
       fill = "DI", color = "DI") +
  annotation_scale(aes(style = "ticks"))
di_plot


################################################################################

bp <- brewer.pal(5, "RdBu")
#n <- scales::rescale(c(1, .1, .01, .005, 0, -.005, -.01, -.1, -1), to = c(0, 1))
di1_plot <- base_plot +
  di_values %>%
  select(d_di1) %>%
  mutate(d_di1 = ifelse((d_di1 <= .005 & d_di1 >= -.005), 0, d_di1)) %>%
  filter(!is.na(d_di1), d_di1 != 0) %>%
  geom_sf(data = ., aes(fill = d_di1, color = d_di1)) +
  # scale_fill_stepsn(colours = bp, values = n[2:8]) +
  # scale_color_stepsn(colours = bp, values = n[2:8]) +
  scale_color_steps2(low = bp[5], mid = bp[3], high = bp[1], midpoint = 0) +
  scale_fill_steps2(low = bp[5], mid = bp[3], high = bp[1], midpoint = 0) +
  coord_sf(xlim = c(xmin + 650, xmax - 400),
           ylim = c(ymin + 400, ymax - 300)) +
  labs(title = "Scenario 1: Unlimited access",
       fill = expression(Delta ~ "DI"), color = expression(Delta ~ "DI")) +
  annotation_scale(aes(style = "ticks"))
di1_plot


################################################################################

bp <- brewer.pal(5, "RdBu")
di2_plot <- base_plot +
  geom_sf(data = filter(gs, identifier %in% target_ids), fill = "brown2") +
  di_values %>%
  select(d_di2) %>%
  mutate(d_di2 = ifelse((d_di2 <= .01 & d_di2 >= -.01), 0, d_di2)) %>%
  filter(!is.na(d_di2), d_di2 != 0) %>%
  geom_sf(data = ., aes(fill = d_di2, color = d_di2)) +
  scale_color_steps2(low = bp[5], mid = bp[3], high = bp[1], midpoint = 0) +
  scale_fill_steps2(low = bp[5], mid = bp[3], high = bp[1], midpoint = 0) +
  coord_sf(xlim = c(xmin + 600, xmax - 450),
           ylim = c(ymin + 700, ymax + 100)) +
  labs(title = "Scenario 2: Green space development",
       fill = expression(Delta ~ "DI"), color = expression(Delta ~ "DI")) +
  annotation_scale(aes(style = "ticks"))
di2_plot


################################################################################

bp <- brewer.pal(5, "RdBu")
di4_plot <- base_plot +
  di_values %>%
  select(d_di4) %>%
  mutate(d_di4 = ifelse((d_di4 <= .01 & d_di4 >= -.01), 0, d_di4)) %>%
  filter(!is.na(d_di4), d_di4 != 0) %>%
  geom_sf(data = ., aes(fill = d_di4, color = d_di4)) +
  scale_color_steps2(low = bp[5], mid = bp[3], high = bp[1], midpoint = 0) +
  scale_fill_steps2(low = bp[5], mid = bp[3], high = bp[1], midpoint = 0) +
  coord_sf(xlim = c(xmin, xmax),
           ylim = c(ymin, ymax)) +
  labs(title = "Scenario 4: Ensemble model",
       fill = expression(Delta ~ "DI"), color = expression(Delta ~ "DI")) +
  annotation_scale(aes(style = "ticks"))
di4_plot

ua <- paste0(wd, "DE008L2_LEIPZIG_UA2018_v013.gpkg")
lyr <- st_layers(ua)$name[1]
#q <- paste0("SELECT * FROM ", lyr, " WHERE code_2018 LIKE '11' + '%'")
ua_res <- read_sf(ua, layer = lyr, wkt_filter = bbox_filter) %>%
  select(code_2018) %>%
  filter(grepl("^11", .$code_2018)) %>%
  mutate(code_2018 = factor(code_2018))

ua3_plot <- base_plot +
  geom_sf(data = ua_res, aes(fill = code_2018)) %>%
  scale_fill_viridis_b()
  scale_fill_manual(values = c("red", "green", "blue"))
ua3_plot
# saving
ggsave(filename = "C:/Users/labohben/Documents/GitHub/MA/plots/ls.pdf",
       plot = ls_plot, width = 11.69, height = 8.27)
ggsave(filename = "C:/Users/labohben/Documents/GitHub/MA/plots/ls1.pdf",
       plot = ls1_plot, width = 11.69, height = 8.27)
ggsave(filename = "C:/Users/labohben/Documents/GitHub/MA/plots/ls2.pdf",
       plot = ls2_plot, width = 11.69, height = 8.27)
ggsave(filename = "C:/Users/labohben/Documents/GitHub/MA/plots/ls3.pdf",
       plot = ls3_plot, width = 11.69, height = 8.27)
ggsave(filename = "C:/Users/labohben/Documents/GitHub/MA/plots/ls4.pdf",
       plot = ls4_plot, width = 11.69, height = 8.27)

ggsave(filename = "C:/Users/labohben/Documents/GitHub/MA/plots/di.pdf",
       plot = di_plot, width = 11.69, height = 8.27)
ggsave(filename = "C:/Users/labohben/Documents/GitHub/MA/plots/di1.pdf",
       plot = di1_plot, width = 11.69, height = 8.27)
ggsave(filename = "C:/Users/labohben/Documents/GitHub/MA/plots/di2.pdf",
       plot = di2_plot, width = 11.69, height = 8.27)
ggsave(filename = "C:/Users/labohben/Documents/GitHub/MA/plots/di4.pdf",
       plot = di4_plot, width = 11.69, height = 8.27)

