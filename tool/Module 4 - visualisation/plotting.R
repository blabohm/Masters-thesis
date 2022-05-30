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
gs_q <- paste0("SELECT area, geom, identifier FROM ",  st_layers(gs_dir)$name[1],
                " WHERE code_2018 is 14100 OR code_2018 is 31000")
bbox <- read_sf("D:/output/DE008/bbox.gpkg") %>% st_bbox()
bbox_filter <- bbox %>% st_as_sfc() %>% st_as_text()

gs <- read_sf(gs_dir, query = gs_q, wkt_filter = bbox_filter)

build_poly <- paste0(wd, "buildings.gpkg")

ls_values <- read_sf(paste0(wd, "scenarios/ls_values.gpkg"))

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
  coord_sf(xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4])) +
  labs(title = "Lene Voigt Park, Leipzig \nLocal Significance (LS)",
       color = "LS")


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
  coord_sf(xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4])) +
  labs(title = "Scenario 1: Unlimited access",
       color = expression(Delta ~ "LS"))


################################################################################
target_ids <- c("23502-DE008L2", "23493-DE008L2", "23485-DE008L2",
                "23508-DE008L2", "23509-DE008L2")
ls2_plot <- base_plot +
  geom_sf(data = filter(gs, identifier %in% target_ids), fill = "brown2") +
  ls_values %>%
  select(d_ls2) %>%
  filter(!is.na(d_ls2), d_ls2 != 0) %>%
  arrange(d_ls2) %>%
  mutate(d_ls2 = mk_log(d_ls2)) %>%
  geom_sf(data = ., aes(color = d_ls2), size = 2) +
  scale_color_distiller(palette = "RdBu") +
  coord_sf(xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4])) +
  labs(title = "Scenario 2: Green place development",
       color = expression(Delta ~ "LS"))


################################################################################
ls3_plot <- base_plot +
  ls_values %>%
  select(d_ls3) %>%
  filter(!is.na(d_ls3), d_ls3 != 0) %>%
  arrange(d_ls3) %>%
  mutate(d_ls3 = mk_log(d_ls3)) %>%
  geom_sf(data = ., aes(color = d_ls3), size = 2) +
  scale_color_distiller(palette = "RdBu") +
  coord_sf(xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4])) +
  labs(title = "Scenario 3: Population increase",
       color = expression(Delta ~ "LS"))

################################################################################
ls4_plot <- base_plot +
  ls_values %>%
  select(d_ls4) %>%
  filter(!is.na(d_ls4), d_ls4 != 0) %>%
  arrange(d_ls4) %>%
  mutate(d_ls4 = mk_log(d_ls4)) %>%
  geom_sf(data = ., aes(color = d_ls4), size = 2) +
  scale_color_distiller(palette = "RdBu") +
  coord_sf(xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4])) +
  labs(title = "Scenario 4: Ensemble model",
       color = expression(Delta ~ "LS"))

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
  coord_sf(xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4])) +
  labs(title = "Lene Voigt Park, Leipzig \nDetour index (DI)",
       fill = "DI", color = "DI")
di_plot


################################################################################

bp <- brewer.pal(5, "RdBu")
di1_plot <- base_plot +
  di_values %>%
  mutate(di = ifelse(di > 1, 1, di),
         di1 = ifelse(di1 > 1, 1, di1),
         d_di1 = di1 - di) %>%
  select(d_di1) %>%
  filter(!is.na(d_di1),
         d_di1 != 0) %>%
  geom_sf(data = ., aes(fill = d_di1, color = d_di1)) +
  scale_color_steps2(low = bp[1], mid = bp[3], high = bp[5], midpoint = 0) +
  scale_fill_steps2(low = bp[1], mid = bp[3], high = bp[5], midpoint = 0) +
  coord_sf(xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4])) +
  labs(title = "Scenario 1: Unlimited access",
       fill = expression(Delta ~ "DI"), color = expression(Delta ~ "DI"))
di1_plot


################################################################################

bp <- brewer.pal(5, "RdBu")
di2_plot <- base_plot +
  di_values %>%
  mutate(di = ifelse(di > 1, 1, di),
         di2 = ifelse(di2 > 1, 1, di2),
         d_di2 = di2 - di) %>%
  select(d_di2) %>%
  filter(!is.na(d_di2),
         d_di2 != 0) %>%
  geom_sf(data = ., aes(fill = d_di2, color = d_di2)) +
  scale_color_steps2(low = bp[1], mid = bp[3], high = bp[5], midpoint = 0) +
  scale_fill_steps2(low = bp[1], mid = bp[3], high = bp[5], midpoint = 0) +
  coord_sf(xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4])) +
  labs(title = "Scenario 2: Green place development",
       fill = expression(Delta ~ "DI"), color = expression(Delta ~ "DI"))
di2_plot


################################################################################

bp <- brewer.pal(5, "RdBu")
di4_plot <- base_plot +
  di_values %>%
  mutate(di = ifelse(di > 1, 1, di),
         di4 = ifelse(di4 > 1, 1, di4),
         d_di4 = di4 - di) %>%
  select(d_di4) %>%
  filter(!is.na(d_di4),
         d_di4 != 0) %>%
  geom_sf(data = ., aes(fill = d_di4, color = d_di4)) +
  scale_color_steps2(low = bp[1], mid = bp[3], high = bp[5], midpoint = 0) +
  scale_fill_steps2(low = bp[1], mid = bp[3], high = bp[5], midpoint = 0) +
  coord_sf(xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4])) +
  labs(title = "Scenario 4: Ensemble model",
       fill = expression(Delta ~ "DI"), color = expression(Delta ~ "DI"))
di4_plot


