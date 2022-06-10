library(dplyr)
library(sf)
library(ggplot2)
library(plotly)
library(ggrepel)

wd <- "D:/output/"
cities <- list.files(wd)
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
city_boundaries <- gsub("output/", "input/cities.gpkg", wd)
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

p_top20 <- ggplot() +
  geom_sf(data = nuts, fill = "gray75") +
  # geom_sf(data = plot_cent, aes(fill = top20, color = top20,
  #                               size = pop)#, alpha = .8
  # ) +
  geom_sf(data = plot_sf, aes(fill = pop, color = pop,
                              size = top20)#, alpha = .8
  ) +
  coord_sf(xlim = c(2700000, 5748970),
           ylim = c(1500000, 4500000)) +
  geom_sf_text(data = city_labs, aes(label = URAU_NAME), color = "white",
               nudge_x = -50000, nudge_y = -50000, check_overlap = TRUE) +
  theme_dark()

ggsave(filename = "C:/Users/labohben/Documents/GitHub/MA/plots/pop_in_top20di.pdf",
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

data_ends <- out_cntr %>%
  group_by(CNTR_CODE) %>%
  summarise(pop = last(pop), di = last(di))

num_df <- plot_df %>%
  filter(!is.na(CNTR_CODE)) %>%
  group_by(city_code) %>%
  summarise(CNTR_CODE = first(CNTR_CODE)) %>%
  group_by(CNTR_CODE) %>%
  summarize(n = n()) %>%
  left_join(data_ends)

di_pop_plot <- out_cntr %>%
  na.omit() %>%
  arrange(pop) %>%
  ggplot(aes(x = di, y = pop)) +
  geom_line(aes(col = CNTR_CODE), size = 1) +
  scale_color_viridis_d() +
  theme(legend.position = "none") +
  geom_label_repel(aes(label = CNTR_CODE), data = data_ends,
  max.overlaps = 5, nudge_x = .1, size = 3, direction = "y",
  hjust = 0, force = 0.5, force_pull = 0.1) +
  xlim(c(.4, 1))

# out_region %>%
#   na.omit() %>%
#   ggplot(aes(x = di, y = pop_round, col = RegionUN)) + geom_line()

plot_ly(data = out, x = ~di, y = ~pop, color = ~city) %>%
  add_lines()
plot_ly(data = out_cntr, x = ~di, y = ~pop, color = ~CNTR_CODE) %>%
  add_lines()


ls_mean_plot <- plot_sf %>%
  group_by(CNTR_CODE) %>%
  mutate(med_ls = quantile(log(ls_mean), .5)) %>%
  ggplot(aes(x = reorder(factor(CNTR_CODE), med_ls) , y = log(ls_mean))) +
  geom_jitter(alpha = .5, width = 0) +
  geom_boxplot(alpha = .2) +
  ggtitle("Mean LS at green space entries (city mean)")
ls_plot <- plot_sf %>%
  group_by(CNTR_CODE) %>%
  mutate(med_ls = quantile(log(ls), .5)) %>%
  ggplot(aes(x = reorder(factor(CNTR_CODE), med_ls) , y = log(ls))) +
  geom_jitter(alpha = .5, width = 0) +
  geom_boxplot(alpha = .2) +
  ggtitle("Sum of LS at green space entries (city mean)")

ggsave(filename = "C:/Users/labohben/Documents/GitHub/MA/plots/ls_mean_plot.pdf",
       plot = ls_mean_plot, width = 11.69, height = 8.27)
ggsave(filename = "C:/Users/labohben/Documents/GitHub/MA/plots/ls_plot.pdf",
       plot = ls_plot, width = 11.69, height = 8.27)

ls_map <- ggplot() +
  geom_sf(data = nuts, fill = "gray75") +
  geom_sf(data = mutate(plot_sf, ls_cov = n_parks_ls / n_parks) %>%
            arrange(desc(log(ls_mean))),
          aes(color = ls_cov, size = log(ls_mean))) +
  coord_sf(xlim = c(2700000, 5748970),
           ylim = c(1500000, 4500000)) +
  geom_sf_text(data = city_labs, aes(label = URAU_NAME), color = "white",
               nudge_x = -50000, nudge_y = -50000, check_overlap = TRUE) +
  theme_dark() +
  ggtitle("Mean LS at green space entries (city mean)")
ls_map
ggsave(filename = "C:/Users/labohben/Documents/GitHub/MA/plots/ls_map.pdf",
       plot = ls_map, width = 11.69, height = 8.27)
