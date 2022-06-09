library(dplyr)
library(sf)
library(ggplot2)
library(plotly)

wd <- "Z:/output/"
cities <- list.files(wd)
out <- tibble()
#city <- cities[1]
nuts <- read_sf("Z:/nuts/NUTS_RG_20M_2021_3035.gpkg") %>%
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
out <- read.csv("Z:/MA/di_per_pop_cum1.csv") %>% na.omit()
city_boundaries <- gsub("output/", "input/cities.gpkg", wd)
city_info <- read.csv("Z:/city_info.csv") %>%
  tibble() %>%
  mutate(city_code = substr(URAU_COD_1, 1, 5),
         URAU_NAME = ifelse(city_code == "XK003", "Mitrovica", URAU_NAME))
perc_coverage <- read.csv("Z:/MA/percent_OSM_coverage.csv")

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
  st_as_sf() %>%
  arrange(top20)

# ggplot() +
#   geom_sf(data = plot_sf, aes(fill = di, color = di))
# ggplot() +
#   geom_sf(data = plot_sf, aes(fill = top20, color = top20))
# ggplot() +
#   geom_sf(data = plot_sf, aes(fill = pop, size = di))

plot_cent <- st_point_on_surface(plot_sf)

city_labs <- filter(plot_cent, grepl("001|NL002", city_code)) %>%
  mutate(URAU_NAME = ifelse(grepl("NL002", city_code), "Amsterdam", URAU_NAME)) %>%
  filter(city_code != "NL001") %>% select(URAU_NAME)

p_top20 <- ggplot() +
  geom_sf(data = nuts, fill = "gray75") +
  # geom_sf(data = plot_cent, aes(fill = top20, color = top20,
  #                               size = pop)#, alpha = .8
  # ) +
  geom_sf(data = plot_cent, aes(fill = pop, color = pop,
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

library(ggrepel)
data_ends <- out_cntr %>%
  group_by(CNTR_CODE) %>%
  summarise(pop = last(pop), di = last(di))

out_cntr %>%
  na.omit() %>%
  arrange(pop) %>%
  ggplot(aes(x = di, y = pop)) +
  geom_line(aes(col = CNTR_CODE), size = 1) +
  scale_color_viridis_d() +
  theme(legend.position = "top") +
  geom_label_repel(aes(label = #paste0(CNTR_CODE, ":", round(pop, digits = 2))
                       CNTR_CODE
                       ),
                   data = data_ends,
                   max.overlaps = 5,
                   nudge_x = .1,
                   size = 3,
                   direction = "y",
                   hjust = 0,
                   force = 0.5, force_pull = 0.1
                   ) +
  xlim(c(.4, 1))

out_region %>%
  na.omit() %>%
  ggplot(aes(x = di, y = pop_round, col = RegionUN)) + geom_line()

plot_ly(data = out, x = ~di, y = ~pop, color = ~city) %>%
  add_lines()
plot_ly(data = out_cntr, x = ~di, y = ~pop, color = ~CNTR_CODE) %>%
  add_lines()

# max pop - pop wert bei .8

# ls <- read_sf("C:/project/DE001/local_significance.gpkg") %>%
#   filter(!is.na(ls)) %>%
#   mutate(edge_length = st_length(.)) %>%
#   st_drop_geometry() %>%
#   mutate(ls_meter = ls / edge_length,
#          city = "Berlin",
#          x_bar = mean(ls),
#          x_bar1 = mean(ls_meter))

# di1 <- read_sf("C:/project/DE008/detour_index.gpkg") %>%
#   st_drop_geometry() %>%
#   filter(!is.na(di)) %>%
#   mutate(di_pop = di * population,
#          city = "Leipzig",
#          x_bar = mean(di))
# ls1 <- read_sf("C:/project/DE008/local_significance.gpkg") %>%
#   filter(!is.na(ls)) %>%
#   mutate(edge_length = st_length(.)) %>%
#   st_drop_geometry() %>%
#   mutate(ls_meter = ls / edge_length,
#          city = "Leipzig",
#          x_bar = mean(ls),
#          x_bar1 = mean(ls_meter))
#
# diDF <-  di %>%
#   select(di, city, x_bar) %>%
#   bind_rows(select(di1, di, city, x_bar))
#
# lsDF <-  ls %>%
#   select(ls, city, x_bar) %>%
#   bind_rows(select(ls1, ls, city, x_bar))
#
# lsmDF <-  ls %>%
#   select(ls = ls_meter, city, x_bar) %>%
#   bind_rows(select(ls1, ls = ls_meter, city, x_bar1)) %>%
#   mutate(ls = as.double(ls))
#
# pDI <- diDF %>%
#   ggplot(aes(x = di)) +
#   geom_histogram(bins = 100) +
#   geom_vline(data = select(diDF, city, x_bar), aes(xintercept = x_bar), col = "red", lty = 3) +
#   facet_grid(. ~ city, scales = "free_y") +
#   xlim(c(0,1)) +
#   labs(title = "detour index")
# pLS <- lsDF %>%
#   ggplot(aes(x = ls)) +
#   geom_histogram(bins = 100) +
#   geom_vline(data = select(lsDF, city, x_bar), aes(xintercept = x_bar), col = "red", lty = 3) +
#   facet_grid(. ~ city, scales = "free_y") +
#   xlim(c(0, 1e+5)) +
#   labs(title = "local significance")
#
# pLSM <- lsmDF %>%
#   ggplot(aes(x = ls)) +
#   geom_histogram(bins = 100) +
#  # geom_vline(data = select(lsmDF, city, x_bar1), aes(xintercept = x_bar1), col = "red", lty = 3) +
#   facet_grid(. ~ city, scales = "free_y") +
#   xlim(c(0, 1e+4)) +
#   labs(title = "local significance per meter")
#
# #hist(di$di, xlim = c(0,1), breaks = 100)
# #hist(ls$ls_meter, xlim = c(0, 4e+4), breaks = 1e+6)
# #hist(ls$ls, xlim = c(0, 4e+5), breaks = 10000)
# pDI
# pLS
