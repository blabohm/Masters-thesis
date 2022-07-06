library(dplyr)
library(sf)
library(ggplot2)
library(plotly)
library(ggrepel)
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

p_top20 <- ggplot() +
  geom_sf(data = nuts, fill = "gray60") +
  geom_sf(data = arrange(plot_sf, top20),
          aes(color = top20#, size = pop
              ), size = 3, alpha = .8) +
  geom_sf(data = st_cast(nuts, "MULTILINESTRING"),
          alpha = 0.5, color = "gray85", size = .75) +
  coord_sf(xlim = c(2700000, 5748970),
           ylim = c(1500000, 4500000)) +
  geom_sf_text(data = cntr_labs, aes(label = NUTS_ID), color = "gray30",
               check_overlap = TRUE) +
  scale_color_distiller(palette = "RdYlBu") +
  ggtitle("Percent of population with DI > 0.8") +
  labs(color = "DI > 0.8 \n[%]"#, size = "DI \ncoverage"
       ) +
  theme(legend.position = c(.075, .75),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
p_top20
ggsave(filename = paste0(github,"/3-6a_di_map.pdf"),
       plot = p_top20, width = 11.69, height = 8.27)

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
  geom_label_repel(aes(label = CNTR_CODE), color = "grey50",
                   data = data_ends1, max.overlaps = 5, size = 4,
                   direction = "y", nudge_x = .05,
                   force = 0.5, force_pull = 5) +
  geom_label_repel(aes(label = CNTR_CODE, color = CNTR_CODE), #color = "red3",
                   data = data_ends2, max.overlaps = 5, size = 4,
                   direction = "y", nudge_x = -.05,
                   force = 0.5, force_pull = 5) +
  xlim(c(.4, 1)) +
  xlab("DI") + ylab("population [%]") + ggtitle("DI vs. cumulative population")
ggsave(filename = paste0(github, "/3-6b_di_plot.pdf"),
       plot = di_plot, width = 11.69, height = 8.27)


# out_region %>%
#   na.omit() %>%
#   ggplot(aes(x = di, y = pop_round, col = RegionUN)) + geom_line()

# plot_ly(data = out, x = ~di, y = ~pop, color = ~city) %>%
#   add_lines()
# plot_ly(data = out_cntr, x = ~di, y = ~pop, color = ~CNTR_CODE) %>%
#   add_lines()


ls_mean_plot <- plot_sf %>%
  group_by(CNTR_CODE) %>%
  mutate(med_ls = quantile(log(ls_mean), .5)) %>%
  ggplot(aes(x = reorder(factor(CNTR_CODE), med_ls) , y = log(ls_mean))) +
  geom_jitter(alpha = .25, width = 0) +
  geom_boxplot(alpha = .2) +
  ggtitle("Mean LS at green space entries (city mean)") +
  theme(axis.title.x = element_blank()) +
  ylab(expression(bar("LS"))) +
  geom_hline(yintercept = quantile(log(plot_sf$ls_mean), .75),
             col = "green", linetype = "dashed", size = .8) +
  geom_hline(yintercept = quantile(log(plot_sf$ls_mean), .25), col = "green",
             linetype = "dashed", size = .8)
ggsave(filename = paste0(github, "/3-7b_ls_plot.pdf"),
       plot = ls_mean_plot, width = 11.69, height = 2.5)

# ls_plot <- plot_sf %>%
#   group_by(CNTR_CODE) %>%
#   mutate(med_ls = quantile(log(ls), .5)) %>%
#   ggplot(aes(x = reorder(factor(CNTR_CODE), med_ls) , y = log(ls))) +
#   geom_jitter(alpha = .5, width = 0) +
#   geom_boxplot(alpha = .2) +
#   ggtitle("Sum of LS at green space entries (city mean)")
# ggsave(filename = "D:/MA/plots/ls_plot.pdf",
#        plot = ls_plot, width = 11.69, height = 8.27)

ls_map <- ggplot() +
  geom_sf(data = nuts, fill = "gray60") +
  geom_sf(data = mutate(plot_sf, ls_cov = n_parks_ls / n_parks) %>%
            arrange((log(ls_mean))),
          aes(
            #size = ls_cov,
            color = log(ls_mean)), shape = 18, size = 5) +
  scale_color_step
  scale_color_distiller(palette = "RdYlBu") +
  geom_sf(data = st_cast(nuts, "MULTILINESTRING"),
          alpha = 0.5, color = "gray85", size = .75) +
  coord_sf(xlim = c(2700000, 5748970),
           ylim = c(1500000, 4500000)) +
  geom_sf_text(data = cntr_labs, aes(label = NUTS_ID), color = "gray30",
               check_overlap = TRUE) +
  # geom_sf_text(data = cntr_labs, aes(label = NUTS_ID), color = "gray95",
  #              check_overlap = TRUE, size = 5) +
  #  theme_dark() +
  ggtitle("Mean LS at green space entries (city mean)") +
  labs(color = expression(bar("LS")), size = "LS \ncoverage") +
  theme(legend.position = c(.075, .75),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect("grey30"))

ls_map
ggsave(filename = paste0(github, "/3-7a_ls_map.pdf"),
       plot = ls_map, width = 11.69, height = 8.27)
