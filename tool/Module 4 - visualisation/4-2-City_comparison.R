library(dplyr)
library(sf)
library(ggplot2)
library(units)
di <- read_sf("C:/project/DE001/detour_index.gpkg") %>%
  st_drop_geometry() %>%
  #filter(!is.na(di)) %>%
  mutate(di_pop = di * population,
         city = "Berlin",
         x_bar = mean(di, na.rm = TRUE))
ls <- read_sf("C:/project/DE001/local_significance.gpkg") %>%
  filter(!is.na(ls)) %>%
  mutate(edge_length = st_length(.)) %>%
  st_drop_geometry() %>%
  mutate(ls_meter = ls / edge_length,
         city = "Berlin",
         x_bar = mean(ls),
         x_bar1 = mean(ls_meter))


di1 <- read_sf("C:/project/DE008/detour_index.gpkg") %>%
  st_drop_geometry() %>%
  filter(!is.na(di)) %>%
  mutate(di_pop = di * population,
         city = "Leipzig",
         x_bar = mean(di))
ls1 <- read_sf("C:/project/DE008/local_significance.gpkg") %>%
  filter(!is.na(ls)) %>%
  mutate(edge_length = st_length(.)) %>%
  st_drop_geometry() %>%
  mutate(ls_meter = ls / edge_length,
         city = "Leipzig",
         x_bar = mean(ls),
         x_bar1 = mean(ls_meter))

diDF <-  di %>%
  select(di, city, x_bar) %>%
  bind_rows(select(di1, di, city, x_bar))

lsDF <-  ls %>%
  select(ls, city, x_bar) %>%
  bind_rows(select(ls1, ls, city, x_bar))

lsmDF <-  ls %>%
  select(ls = ls_meter, city, x_bar) %>%
  bind_rows(select(ls1, ls = ls_meter, city, x_bar1)) %>%
  mutate(ls = as.double(ls))

pDI <- diDF %>%
  ggplot(aes(x = di)) +
  geom_histogram(bins = 100) +
  geom_vline(data = select(diDF, city, x_bar), aes(xintercept = x_bar), col = "red", lty = 3) +
  facet_grid(. ~ city, scales = "free_y") +
  xlim(c(0,1)) +
  labs(title = "detour index")
pLS <- lsDF %>%
  ggplot(aes(x = ls)) +
  geom_histogram(bins = 100) +
  geom_vline(data = select(lsDF, city, x_bar), aes(xintercept = x_bar), col = "red", lty = 3) +
  facet_grid(. ~ city, scales = "free_y") +
  xlim(c(0, 1e+5)) +
  labs(title = "local significance")

pLSM <- lsmDF %>%
  ggplot(aes(x = ls)) +
  geom_histogram(bins = 100) +
 # geom_vline(data = select(lsmDF, city, x_bar1), aes(xintercept = x_bar1), col = "red", lty = 3) +
  facet_grid(. ~ city, scales = "free_y") +
  xlim(c(0, 1e+4)) +
  labs(title = "local significance per meter")

#hist(di$di, xlim = c(0,1), breaks = 100)
#hist(ls$ls_meter, xlim = c(0, 4e+4), breaks = 1e+6)
#hist(ls$ls, xlim = c(0, 4e+5), breaks = 10000)
pDI
pLS
