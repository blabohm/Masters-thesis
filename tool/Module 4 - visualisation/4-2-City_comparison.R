library(dplyr)
library(sf)
library(ggplot2)
library(plotly)

wd <- "Z:/output/"
cities <- list.files(wd)
out <- tibble()
city <- cities[1]

for (city in cities) {
  print(paste(which(cities == city), "of", length(cities)))
  try({
  out <- wd %>%
  paste0(city, "/detour_index.gpkg") %>%
    read_sf() %>%
    st_drop_geometry() %>%
    mutate(city = city,
           di = case_when(di > 1 ~ 1,
                          #is.na(di) ~ 0,
                          TRUE ~ di)) %>%
    arrange(di) %>%
    mutate(pop_cum = cumsum(population),
           pop_sum = sum(population),
           pop_rel = pop_cum / pop_sum,
           di = round(di, digits = 2)) %>%
    group_by(di) %>%
    summarise(pop = mean(pop_rel), city = first(city)) %>%
    bind_rows(out)
  })}

#out <- read.csv("Z:/MA/di_per_pop_cum.csv") %>% na.omit()
city_boundaries <- gsub("output/", "input/cities.gpkg", wd)
city_info <- read.csv("Z:/city_info.csv") %>%
  tibble() %>%
  mutate(city_code = substr(URAU_COD_1, 1, 5),
         URAU_NAME = ifelse(city_code == "XK003", "Mitrovica", URAU_NAME))
perc_coverage <- read.csv("Z:/MA/percent_OSM_coverage.csv")
city_sf <- read_sf(city_boundaries) %>%
  rename(city_code = URAU_CODE) %>%
  left_join(perc_coverage) %>%
  left_join(city_info, by = "city_code")
city_vec <- filter(city_sf, percent_coverage > .85) %>% pull(city_code)
plot_df <- city_sf %>%
  st_drop_geometry() %>%
  right_join(rename(out, "city_code" = "city"))

plot_sf <- out %>%
  mutate(di = ifelse(is.na(di), 0, di),
         pop_di = di * pop) %>%
  group_by(city) %>%
  summarise(di = mean(di), pop = mean(pop), pop_di = mean(pop_di)) %>%
  rename(city_code = city) %>%
  right_join(city_sf) %>%
  st_as_sf()

ggplot() +
  geom_sf(data = plot_sf, aes(fill = di, color = di))
ggplot() +
  geom_sf(data = plot_sf, aes(fill = pop_di, color = pop_di))

out_cntr <- plot_df %>%
#  group_by(CNTR_CODE, pop) %>%
  # arrange(di) %>%
  # summarise(di = mean(di)) %>%
  # mutate(di = round(di, digits = 1)) %>%
  group_by(CNTR_CODE, di) %>%
  summarise(pop = mean(pop))


out_region <- plot_df %>%
  # group_by(RegionUN, pop) %>%
  # arrange(di) %>%
  # summarise(di = mean(di)) %>%
  # mutate(di = round(di, digits = 1)) %>%
  group_by(RegionUN, di) %>%
  summarise(pop_round = mean(pop))

out %>%
  ggplot(aes(x = di, y = pop, col = city)) + geom_line()

out_cntr %>%
  ggplot(aes(x = di, y = pop, col = CNTR_CODE)) + geom_line()

out_region %>%
  ggplot(aes(x = di, y = pop_round, col = RegionUN)) + geom_line()

plot_ly(data = out, x = ~di, y = ~pop_round, color = ~city) %>%
  add_lines()
out_sav <- out
out <- out_sav
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
