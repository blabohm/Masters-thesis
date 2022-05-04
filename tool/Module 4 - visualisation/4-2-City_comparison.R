library(dplyr)
library(sf)
library(ggplot2)
library(plotly)

wd <- "Z:/output/"
cities <- list.files(wd)
out <- tibble()
#city <- cities[1]

for (city in cities) {
print(city)
  out <- wd %>%
  paste0(city, "/detour_index.gpkg") %>%
    read_sf() %>%
    st_drop_geometry() %>%
    #filter(!is.na(di)) %>%
    mutate(city = city,
           di = case_when(di > 1 ~ 1,
                          #is.na(di) ~ 0,
                          TRUE ~ di)) %>%
    arrange(di) %>%
    mutate(pop_cum = cumsum(population),
           pop_sum = sum(population),
           pop_rel = pop_cum / pop_sum,
           pop_round = round(pop_rel, digits = 2)) %>%
    group_by(pop_round) %>%
    summarise(di = mean(di), city = first(city)) %>%
    bind_rows(out)
}
agg_df <- read_sf("Z:/cities_europe_core/cities_full.gpkg") %>%
  st_drop_geometry() %>%
  mutate(city = substr(URAU_COD_1, 1, 5)) %>%
  select(city, CNTR_CODE, URAU_NAME, RegionEU, RegionUN)

out_agg <- left_join(out, agg_df)

out_cntr <- out_agg %>%
  group_by(CNTR_CODE, pop_round) %>%
  arrange(di, pop_round) %>%
  summarise(di = mean(di))

out_region <- out_agg %>%
  group_by(RegionUN, pop_round) %>%
  arrange(di, pop_round) %>%
  summarise(di = mean(di))

out_region1 <- out_agg %>%
  group_by(RegionEU, pop_round) %>%
  arrange(di, pop_round) %>%
  summarise(di = mean(di))

out %>%
  ggplot(aes(x = di, y = pop_round, col = city)) + geom_line()

out_cntr %>%
  ggplot(aes(x = di, y = pop_round, col = CNTR_CODE)) + geom_line()

out_region %>%
  ggplot(aes(x = di, y = pop_round, col = RegionUN)) + geom_line()

out_region1 %>%
  ggplot(aes(x = di, y = pop_round, col = RegionEU)) + geom_line()

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
