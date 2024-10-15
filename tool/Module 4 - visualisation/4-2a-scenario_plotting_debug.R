library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)
library(ggspatial)
library(cowplot)

#github <- "C:/Users/labohben/Documents/GitHub/MA/"
#wd <- "C:/Users/labohben/Desktop/DE008/"
DRIVE <- "D:/"
github <- paste0(DRIVE, "MA/")
wd <- paste0(DRIVE,"MA/DE008/")
edges <- paste0(wd, "edges.gpkg")
id <- "23473-DE008L2"
gs_dir <- paste0(wd, "DE008L2_LEIPZIG_UA2018_v012.gpkg")
lvp_q <- paste0("SELECT area, geom FROM ",  st_layers(gs_dir)$name[1],
                " WHERE identifier LIKE '", id, "'")
lvp <- read_sf(gs_dir, query = lvp_q)
lvp_label <- st_point_on_surface(lvp) %>% mutate(name = "Lene Voigt Park")
gs_q <- paste0("SELECT area, geom, identifier FROM ",  st_layers(gs_dir)$name[1],
               " WHERE code_2018 is 14100 OR code_2018 is 31000")


build_poly <- paste0(wd, "buildings.gpkg")
ls_values <- read_sf(paste0(wd, "scenarios/ls_values.gpkg"))
di_values <- read_sf(paste0(wd, "scenarios/di_values.gpkg"))


bbox <- ls_values %>% st_bbox()
bbox_filter <- bbox %>% st_as_sfc() %>% st_as_text()
xmin <- bbox[1] + 1100
xmax <- bbox[3] - 1100
ymin <- bbox[2] + 1100
ymax <- bbox[4] - 1100
umlaute <- function(variable) {
  variable <- gsub("Ã¼","ü",variable)
  variable <- gsub("ÃŸ","ß",variable)
  variable <- gsub("Ã¶r|Ã¶","ö",variable)
  variable <- gsub("Ã¤","ä",variable)
  return(variable)
}

street_labs <- read_sf(paste0(wd, "lvp_osm.gpkg")) %>%
  st_transform(3035) %>%
  filter(!is.na(name), highway != "highway") %>%
  st_filter(st_as_sfc(bbox)) %>%
  mutate(name = umlaute(name)) %>%
  select(name) %>%
  filter(name %in% c("Riebeckstraße", "Josephinenstraße")) %>%
  group_by(name) %>%
  summarise() %>%
  st_union(by_feature = TRUE) %>%
  st_centroid() %>%
  bind_rows(lvp_label) %>%
  mutate(lab = LETTERS[1:nrow(.)])

gs <- read_sf(gs_dir, query = gs_q, wkt_filter = bbox_filter)
build_sf <- read_sf(build_poly, wkt_filter = bbox_filter) %>% select(geom)
net_sf <- read_sf(edges, wkt_filter = bbox_filter) %>% select(geom)

base_plot <- ggplot() +
  geom_sf(data = st_buffer(st_as_sfc(bbox), 1000), fill = "gray93") +
  geom_sf(data = build_sf,
          fill = "gray99", color = "gray99") +
  geom_sf(data = lvp, fill = "darkolivegreen4", color = NA,
          alpha = .2) +
  geom_sf(data = net_sf, color = "gray80", size = 1) +
  geom_sf(data = gs, fill = "darkolivegreen1", color = NA,
          alpha = .2)

################################################################################
lpz <- paste0(wd, "cities.gpkg") %>%
  read_sf(query = "SELECT * FROM cities WHERE URAU_CODE = 'DE008'") %>%
  st_transform(3035)
lpz_box <- st_bbox(lpz)
ua <- paste0(wd, "DE008L2_LEIPZIG_UA2018_v013.gpkg")
water <- read_sf(ua, query = paste("SELECT * FROM", st_layers(ua)$name[1],
                                   "WHERE code_2018 = '50000'"),
                 wkt_filter = st_as_text(lpz$geom))
# roads <- read_sf(ua, query = paste("SELECT * FROM", st_layers(ua)$name[1],
#                                    "WHERE code_2018 = '12210'"),
#                  wkt_filter = st_as_text(lpz_box$geom))
overview_plot <- ggplot(lpz) +
  geom_sf(data = water, color = "lightblue", fill = "lightblue") +
  #  geom_sf(data = roads, color = "grey30") +
  geom_sf(fill = NA) +
  geom_sf(data = st_point_on_surface(lvp), color = "red", size = 3) +
  coord_sf(xlim = c(lpz_box[1], lpz_box[3]),
           ylim = c(lpz_box[2], lpz_box[4]))  +
  geom_sf_text(data = lvp, aes(label = "Lene Voigt Park"), size = 3, nudge_y = 2e3) +
  ggtitle("Leipzig") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(color = "black"),
        plot.title.position = "panel",
  )

ls_query <- paste0("SELECT * FROM ls WHERE ls is not null")
ls_plot <- base_plot +
  ls_values %>%
  select(ls) %>%
  filter(!is.na(ls)) %>%
  mutate(ls = log(ls)) %>%
  arrange(ls) %>%
  geom_sf(data = ., aes(color = ls), lwd = 1.2) +
  #geom_sf(data = street_labs, aes(shape = name)) +
  scale_color_distiller(palette = "RdBu") +
  coord_sf(xlim = c(xmin, xmax),
           ylim = c(ymin, ymax)) +
  labs(title = "Local Significance (LS)",
       color = "LS") +
  annotation_scale(aes(style = "ticks")) +
  #geom_sf_label(data = street_labs, aes(label = lab), color = "gray10",
  #              nudge_y = 50, show.legend = TRUE) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), legend.title = element_blank())


di_plot <- base_plot +
  di_values %>%
  select(di) %>%
  filter(!is.na(di)) %>%
  geom_sf(data = ., aes(fill = di, color = di)) +
  scale_fill_distiller(palette = "RdYlBu", direction = 1,
                       breaks = c(0, .2, .4, .6, .8)) +
  scale_color_distiller(palette = "RdYlBu", direction = 1,
                        breaks = c(0, .2, .4, .6, .8)) +
  coord_sf(xlim = c(xmin, xmax),
           ylim = c(ymin, ymax)) +
  labs(title = "Detour Index (DI)", fill = "DI", color = "DI") +
  annotation_scale(aes(style = "ticks"))  +
  annotation_custom(grob = ggplotGrob(overview_plot),
                    xmin = xmin - 100,
                    xmax = (xmin + ((ymax - ymin) / 3) * .88) - 100,
                    ymin = ymin,
                    ymax = ymin + (ymax - ymin) / 3) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank())

plot_grid(ls_plot, di_plot, nrow = 2) %>%
  ggsave(plot = ., filename = paste0(github, "/3-1_ls_di_plot_no_labs.pdf"),
         width = 8.27, height = 11.69)
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
# DATA PREPARATION FOR EACH PLOT
ls1_data <- ls_values %>%
  select(d_ls1) %>%
  filter(!is.na(d_ls1), d_ls1 != 0) %>%
  arrange(d_ls1) %>%
  mutate(d_ls1 = mk_log(d_ls1))

di1_data <-  di_values %>%
  select(d_di1) %>%
  mutate(d_di1 = ifelse((d_di1 <= .005 & d_di1 >= -.005), 0, d_di1)) %>%
  filter(!is.na(d_di1), d_di1 != 0)

target_ids <- c("23502-DE008L2", "23493-DE008L2", "23485-DE008L2",
                "23508-DE008L2", "23509-DE008L2")
ls2_data <- ls_values %>%
  select(d_ls2) %>%
  filter(!is.na(d_ls2), d_ls2 != 0) %>%
  arrange(d_ls2) %>%
  mutate(d_ls2 = mk_log(d_ls2))

di2_data <- di_values %>%
  select(d_di2) %>%
  mutate(d_di2 = ifelse((d_di2 <= .005 & d_di2 >= -.005), 0, d_di2)) %>%
  filter(!is.na(d_di2), d_di2 != 0)

build <- read_sf(build_poly) %>% select(ID)
pop_dat <- read_sf(paste0(wd, "scen3_be.gpkg")) %>%
  st_drop_geometry() %>%
  mutate(population = ifelse(population > 50, 50, population)) %>%
  left_join(build) %>% st_as_sf()

ls3_data <- ls_values %>%
  select(d_ls3) %>%
  filter(!is.na(d_ls3), d_ls3 != 0) %>%
  arrange(d_ls3) %>%
  mutate(d_ls3 = mk_log(d_ls3))

ls4_data <- ls_values %>%
  select(d_ls4) %>%
  filter(!is.na(d_ls4), d_ls4 != 0) %>%
  arrange(d_ls4) %>%
  mutate(d_ls4 = mk_log(d_ls4))

di4_data <-  di_values %>%
  select(d_di4) %>%
  mutate(d_di4 = ifelse((d_di4 <= .005 & d_di4 >= -.005), 0, d_di4)) %>%
  filter(!is.na(d_di4), d_di4 != 0)

################################################################################
# PLOTTING PARAMETERS

p_theme <- theme(axis.title = element_blank(), axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 legend.position = "none")
p_scale <- annotation_scale(aes(style = "ticks"))

# LS params
ls_rng <- range(c(ls1_data$d_ls1, ls2_data$d_ls2,
                  ls3_data$d_ls3, ls4_data$d_ls4))
ls_bp <- brewer.pal(11, "RdYlBu") %>% rev()

s <- scales::rescale(c(-10, -7.5, -5, 2.5, 0, 2.5, 5, 7.5, 10, 12.5, 15),
                     c(0,1))
ls_color <- scale_color_stepsn(colours = ls_bp,
                               breaks = c(-10, -5, 0, 5, 10, 15),
                               limits = c(-10, 15),
                               #values = s,
                               oob = scales::squish("ls"))

line_size <- 1.2

# DI params
di_rng <- range(c(di1_data$d_di1, di2_data$d_di2, di4_data$d_di4))
di_bp <- brewer.pal(5, "RdBu")
di_color <- scale_color_steps2(low = di_bp[1], mid = di_bp[3], high = di_bp[5],
                               midpoint = 0, aesthetics = c("color", "fill"),
                               limits = c(-.3, .3))

################################################################################
# PLOTS
ls1_plot <- base_plot +
  geom_sf(data = ls1_data, aes(color = d_ls1), size = line_size
  ) +
  coord_sf(xlim = c(xmin + 650, xmax - 400),
           ylim = c(ymin + 400, ymax - 300)) +
  ls_color + p_scale + p_theme

di1_plot <- base_plot +
  geom_sf(data = di1_data, aes(fill = d_di1, color = d_di1)) +
  coord_sf(xlim = c(xmin + 650, xmax - 400),
           ylim = c(ymin + 400, ymax - 300)) +
  di_color + p_scale + p_theme

################################################################################
ls2_plot <- base_plot +
  geom_sf(data = filter(gs, identifier %in% target_ids), fill = "grey50") +
  geom_sf(data = ls2_data, aes(color = d_ls2), size = line_size
  ) +
  coord_sf(xlim = c(xmin + 525, xmax - 375),
           ylim = c(ymin + 700, ymax + 100)) +
  ls_color + p_scale + p_theme

di2_plot <- base_plot +
  geom_sf(data = filter(gs, identifier %in% target_ids), fill = "grey50") +
  geom_sf(data = di2_data, aes(fill = d_di2, color = d_di2)) +
  coord_sf(xlim = c(xmin + 525, xmax - 375),
           ylim = c(ymin + 700, ymax + 100)) +
  di_color + p_scale + p_theme

################################################################################
ls3_plot <- base_plot +
  # geom_sf(data = pop_dat, aes(fill = population)) +
  scale_fill_distiller(palette = "RdBu", ) +
  geom_sf(data = ls3_data, aes(color = d_ls3), size = line_size
  ) +
  coord_sf(xlim = c(xmin + 70, xmax - 70),
           ylim = c(ymin, ymax)) +
  ls_color + p_scale + p_theme

################################################################################
ls4_plot <- base_plot +
  geom_sf(data = ls4_data, aes(color = d_ls4), size = line_size
  ) +
  coord_sf(xlim = c(xmin + 70, xmax - 70),
           ylim = c(ymin, ymax)) +
  ls_color + p_scale + p_theme

################################################################################
di4_plot <- base_plot +
  geom_sf(data = di4_data, aes(fill = d_di4, color = d_di4)) +
  coord_sf(xlim = c(xmin + 70, xmax - 70),
           ylim = c(ymin, ymax)) +
  di_color + p_scale + p_theme

legend_alt2 <- (ggplot() +
                  geom_sf(data = filter(gs, identifier %in% target_ids),
                          aes(fill = "Converted \ngreen spaces")) +
                  scale_fill_manual("", values = "grey50")) %>%
  get_legend()

legend_ls <- (ls4_plot +
                theme(legend.position = "left") +
                labs(color = expression(Delta ~ "LS"))) %>%
  get_legend()

legend_di <- (di1_plot +
                theme(legend.position = "left") +
                labs(fill = expression(Delta ~ "DI"),
                     color = expression(Delta ~ "DI"))) %>%
  get_legend()

legends <- plot_grid(legend_alt2, legend_ls, legend_di, nrow = 1)
first_col <- plot_grid(ls1_plot, ls2_plot, ls3_plot, ls4_plot,
                       nrow = 4, axis = "tlbr",
                       labels = c("a", "c", "e", "f"))
second_col <- plot_grid(di1_plot, di2_plot, legends, di4_plot,
                        nrow = 4,
                        labels = c("b", "d", "", "g"))

plot_grid(first_col, second_col,
          nrow = 1, ncol = 2, #align = "h",
          axis = "tlbr"#, labels = c("a", "b", "c", "d", "e", "", "", "f", "g")
) %>%
  ggsave(plot = ., filename = paste0(github, "/plots/3-4_alternatives_plot.pdf"),
         width = 8.27, height = 11.69)

#di4_plot
#
# ua <- paste0(wd, "DE008L2_LEIPZIG_UA2018_v013.gpkg")
# lyr <- st_layers(ua)$name[1]
# #q <- paste0("SELECT * FROM ", lyr, " WHERE code_2018 LIKE '11' + '%'")
# ua_res <- read_sf(ua, layer = lyr, wkt_filter = bbox_filter) %>%
#   #select(code_2018) %>%
#   filter(grepl("^11", .$code_2018)) %>%
#   mutate(code_2018 = factor(code_2018))
#
# ua3_plot <- base_plot +
#   geom_sf(data = ua_res, aes(fill = Pop2018)) +
# #  scale_fill_manual(values = c("red4", "red3", "red1")) +
#   coord_sf(xlim = c(xmin, xmax),
#            ylim = c(ymin, ymax)) +
#   # labs(title = "Scenario 3: Population increase",
#   #      color = expression(Delta ~ "LS")) +
#   annotation_scale(aes(style = "ticks"))
# #ua3_plot

# saving

#
# ggsave(filename = paste0(github, "/plots/3-1a_ls.pdf"),
#        plot = ls_plot, width = 11.69, height = 8.27)
# ggsave(filename = paste0(github, "/plots/3-2a_ls1.pdf"),
#        plot = ls1_plot, width = 11.69, height = 8.27)
# ggsave(filename = paste0(github, "/plots/3-3a_ls2.pdf"),
#        plot = ls2_plot, width = 11.69, height = 8.27)
# ggsave(filename = paste0(github, "/plots/3-4a_ls3.pdf"),
#        plot = ls3_plot, width = 11.69, height = 8.27)
# ggsave(filename = paste0(github, "/plots/3-5a_ls4.pdf"),
#        plot = ls4_plot, width = 11.69, height = 8.27)
#
# ggsave(filename = paste0(github, "/plots/3-1b_di.pdf"),
#        plot = di_plot, width = 11.69, height = 8.27)
# ggsave(filename = paste0(github, "/plots/3-2b_di1.pdf"),
#        plot = di1_plot, width = 11.69, height = 8.27)
# ggsave(filename = paste0(github, "/plots/3-3b_di2.pdf"),
#        plot = di2_plot, width = 11.69, height = 8.27)
# ggsave(filename = paste0(github, "/plots/3-5b_di4.pdf"),
#        plot = di4_plot, width = 11.69, height = 8.27)

# ggsave(filename = paste0(github, "/plots/ua3.pdf"),
#        plot = ua3_plot, width = 11.69, height = 8.27)

