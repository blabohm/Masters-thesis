library(dplyr)
library(sf)
library(ggplot2)
library(plotly)
library(ggrepel)
library(cowplot)

wd <- "Z:/"
wd <- "D:/output/"
github <- "C:/Users/labohben/Documents/GitHub/MA/plots/"
github <- "D:/MA/plots/"
#cities <- list.files(wd)
#out <- tibble()
#city <- cities[1]
nuts <- read_sf(paste0(wd, "/NUTS_RG_20M_2021_3035.gpkg")) %>%
  filter(LEVL_CODE == 0) %>%
  select(NUTS_ID)

# for (city in cities) {
#   print(paste(which(cities == city), "of", length(cities)))
#   try({
#     out <- wd %>%
#       paste0(city, "/detour_index.gpkg") %>%
#       read_sf() %>%
#       st_drop_geometry() %>%
#       mutate(city = city,
#              di = case_when(di > 1 ~ 1,
#                             #is.na(di) ~ 0,
#                             TRUE ~ di)) %>%
#       arrange(di) %>%
#       mutate(pop_cum = cumsum(population),
#              pop_sum = sum(population),
#              pop_rel = pop_cum / pop_sum,
#              di = round(di, digits = 2)) %>%
#       group_by(di) %>%
#       summarise(pop = mean(pop_rel), city = first(city)) %>%
#       bind_rows(out)
#   })}

#write.csv(out, "Z:/MA/di_per_pop_cum1.csv")
out <- read.csv(paste0(wd, "di_per_pop_cum1.csv")) %>% na.omit()

city_boundaries <- gsub("/", "/input/cities.gpkg", wd)
city_boundaries <- gsub("/output/", "/input/cities.gpkg", wd)

city_info <- read.csv(paste0(wd, "city_info.csv")) %>%
  tibble() %>%
  mutate(city_code = substr(URAU_COD_1, 1, 5),
         URAU_NAME = ifelse(city_code == "XK003", "Mitrovica", URAU_NAME))
perc_coverage <- read.csv(paste0(wd, "percent_OSM_coverage.csv"))
ls_data <- read.csv(paste0(wd,"/ls_summary.csv"))

pop_top20 <- out %>%
  filter(di >= .8) %>%
  group_by(city) %>%
  mutate(max_pop = max(pop),
         pop_at_di80 = min(pop)) %>%
  summarise(max_pop = first(max_pop),
            pop_at_di80 = first(pop_at_di80)) %>%
  mutate(top20 = max_pop - pop_at_di80) %>%
  rename(city_code = city)

city_sf <- read_sf(city_boundaries) %>%
  rename(city_code = URAU_CODE) %>%
  left_join(perc_coverage) %>%
  left_join(city_info, by = "city_code") %>%
  filter(percent_coverage > .85)

plot_df <- city_sf %>%
  st_drop_geometry() %>%
  right_join(rename(out, "city_code" = "city"))

plot_sf <- out %>%
  mutate(di = ifelse(is.na(di), 0, di)) %>%
  group_by(city) %>%
  summarise(di = mean(di), pop = max(pop)) %>%
  rename(city_code = city) %>%
  inner_join(city_sf) %>%
  left_join(pop_top20) %>%
  left_join(ls_data) %>%
  st_as_sf() %>%
  st_point_on_surface()

city_labs <- filter(plot_sf, grepl("001|NL002", city_code)) %>%
  mutate(URAU_NAME = ifelse(grepl("NL002", city_code), "Amsterdam", URAU_NAME)) %>%
  filter(city_code != "NL001") %>% select(URAU_NAME)
cntr_labs <- st_point_on_surface(nuts)



out_cntr <- plot_df %>%
  group_by(CNTR_CODE, di) %>%
  summarise(pop = mean(pop)) %>%
  mutate(CNTR_CODE = factor(CNTR_CODE))

out_region <- plot_df %>%
  group_by(RegionUN, di) %>%
  summarise(pop_round = mean(pop))

out %>%
  na.omit() %>%
  ggplot(aes(x = di, y = pop, col = city)) + geom_line()

num_df <- plot_df %>%
  filter(!is.na(CNTR_CODE)) %>%
  group_by(city_code) %>%
  summarise(CNTR_CODE = first(CNTR_CODE)) %>%
  group_by(CNTR_CODE) %>%
  summarize(n = n()) %>%
  left_join(data_ends)

#########
# DI MAP
#########

di_labs <- c(10, 20, 30)
di_breaks <- c(0, .25, .5, .75, 1)
di_vals <- quantile(plot_sf$top20, di_breaks) %>%
  scales::rescale(c(0, 1))
n_groups <- length(di_breaks)
di_cols <- RColorBrewer::brewer.pal(6, "RdYlBu")

di_map <- ggplot() +
  geom_sf(data = nuts, fill = "gray60") +
  geom_sf(data = arrange(plot_sf, top20),
          aes(color = top20), shape = 18, size = 5) +
  scale_color_stepsn(colors = di_cols, values = di_vals,
                     n.breaks = n_groups, labels = di_labs) +
  geom_sf(data = st_cast(nuts, "MULTILINESTRING"),
          alpha = 0.5, color = "gray85", size = .75) +
  coord_sf(xlim = c(2700000, 5748970),
           ylim = c(1500000, 4500000)) +
  geom_sf_text(data = cntr_labs, aes(label = NUTS_ID), color = "gray30",
               check_overlap = TRUE) +
  ggtitle("Percent of population with DI > 0.8") +
  labs(color = "DI > 0.8 \n[%]") +
  annotation_scale(aes(style = "ticks"), bar_cols = rep("gray95", 2))  +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())#,
        #panel.background = element_rect("grey30")
#########
# DI PLOT
#########
countries <- paste(c("FI", "SE", "LU", "AL", "XK", "RS", "RO", "DE", "PL", "FR",
                     "IT", "NO"), collapse = "|")
plot_colors <- transmute(out_cntr, CNTR_CODE, plot_colors = case_when(
  grepl(countries, CNTR_CODE) ~ "red",
  TRUE ~ "grey50")) %>% distinct()
data_ends <- out_cntr %>%
  group_by(CNTR_CODE) %>%
  summarise(pop = last(pop), di = last(di)) %>%
  arrange(pop) %>%
  left_join(plot_colors)
data_ends1 <- data_ends %>% filter(plot_colors == "grey50")
data_ends2 <- data_ends %>% filter(plot_colors != "grey50")
scale <- rep(RColorBrewer::brewer.pal(11, "RdYlBu")[c(1:4, 8:11)], 2)

di_plot <- out_cntr %>%
  filter(!grepl(countries, CNTR_CODE)) %>%
  ggplot(aes(x = di, y = pop)) +
  geom_line(aes(... = CNTR_CODE),
            color = "grey50", size = 1) + out_cntr %>%
  filter(grepl(countries, CNTR_CODE)) %>%
  geom_line(data = ., aes(col = CNTR_CODE), size = 1) +
  scale_color_manual(values = sample(scale, 12)) +
  theme(legend.position = "none") +
  # geom_label_repel(aes(label = CNTR_CODE), color = "grey50",
  #                  data = data_ends1, max.overlaps = 5, size = 3,
  #                  direction = "y", nudge_x = .05,
  #                  force = 25, force_pull = .5) +
  geom_label_repel(aes(label = CNTR_CODE, color = CNTR_CODE),
                   data = data_ends2, max.overlaps = 5, size = 4,
                   direction = "y", nudge_x = -.05,
                   force = .5, force_pull = 5
                   ) +
  xlim(c(.4, 1)) +
  xlab("DI") + ylab("population [%]") #+ ggtitle("DI vs. cumulative population")

plot_grid(di_map, di_plot, nrow = 2, axis = "lr", #align = "hv",
          rel_heights = c(1, .5)) %>%
ggsave(filename = paste0(github, "/3-3a_di_map+plot.pdf"),
       plot = ., height = 11.69, width = 8.27, )

#########
# LS MAP
#########
ls_plot_df <- mutate(plot_sf, ls_cov = n_parks_ls / n_parks) %>%
  arrange((log(ls_mean)))
ls_labs <- c("lowest", "below average", "above average", "highest")
ls_breaks <- c(0, .25, .5, .75, 1)
ls_vals <- quantile(log(ls_plot_df$ls_mean), ls_breaks) %>%
  scales::rescale(c(0, 1))
n_groups <- length(ls_breaks)
ls_cols <- RColorBrewer::brewer.pal(n_groups, "RdYlBu") %>% rev()

ls_map <- ggplot() +
  geom_sf(data = nuts, fill = "gray60") +
  geom_sf(data = ls_plot_df, aes(color = log(ls_mean)), shape = 18, size = 5) +
  scale_color_stepsn(colors = ls_cols, values = ls_vals,
                     n.breaks = n_groups, labels = ls_labs) +
  geom_sf(data = st_cast(nuts, "MULTILINESTRING"),
          alpha = 0.5, color = "gray85", size = .75) +
  coord_sf(xlim = c(2700000, 5748970),
           ylim = c(1500000, 4500000)) +
  geom_sf_text(data = cntr_labs, aes(label = NUTS_ID), color = "gray30",
               check_overlap = TRUE) +
  ggtitle("Mean LS at green space entries (city mean)") +
  labs(color = expression(bar("LS")), size = "LS \ncoverage") +
  annotation_scale(aes(style = "ticks"), bar_cols = rep("gray95", 2))  +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()#,
        #panel.background = element_rect("grey30")
        )

#########
# LS PLOT
#########
ls_mean_plot <- plot_sf %>%
  group_by(CNTR_CODE) %>%
  mutate(med_ls = quantile(log(ls_mean), .5)) %>%
  ggplot(aes(x = reorder(factor(CNTR_CODE), med_ls) , y = log(ls_mean))) +
  geom_jitter(alpha = .25, width = 0) +
  geom_boxplot(alpha = .2, width = .5) +
#  ggtitle("Mean LS at green space entries (city mean)") +
  theme(axis.title.x = element_blank()) +
  ylab(expression(bar("LS"))) +
  geom_hline(yintercept = quantile(log(plot_sf$ls_mean), .75),
             col = "green", linetype = "dashed", size = .8) +
  geom_hline(yintercept = quantile(log(plot_sf$ls_mean), .25), col = "green",
             linetype = "dashed", size = .8) +
  coord_fixed(ratio = 1 / 2)

plot_grid(ls_map, ls_mean_plot, nrow = 2, axis = "lr", #align = "hv",
          rel_heights = c(1, .5)) %>%
  ggsave(filename = paste0(github, "/3-3b_ls_map+plot.pdf"),
       plot = ., height = 11.69, width = 8.27, )
