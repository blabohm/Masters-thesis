library(dplyr)
library(sf)

getwd() %>%
  paste0("/tool/Module 2 - data preparation/functions/") %>%
  list.files(pattern = "2-2[A-Za-z].*\\.R|2_.*\\.R", full.names = TRUE) %>%
  for (file in .) source(file)

wd <- "Z:/output/"
cities <- list.files(wd)
ua_directory <- gsub("output/", "input/UA2018/", wd)
city_boundaries <- gsub("output/", "input/cities.gpkg", wd)
perc_coverage <- tibble()
city_code <- cities[136]

for (city_code in cities[18:832]) {
  print(paste(which(cities == city_code), "of", length(cities)))
  try({
  city_boundary <- boundaryLoader(city_boundaries = city_boundaries,
                                  city_code = city_code)
  UAresidential <- UAresLoader(ua_dir = ua_directory,
                               fua_code = city_boundary$FUA_CODE,
                               boundary = city_boundary) %>%
    filter(Pop2018 > 2)
  osm_dir <- paste0(wd, city_code, "/buildings.gpkg")
  OSMresidential <- read_sf(osm_dir)
  covered <- st_covers(x = UAresidential, y = OSMresidential)
  N <- nrow(UAresidential)
  i <- nrow(UAresidential[lapply(covered, length) > 0,])
  i/N
  perc_coverage <- tibble(city_code = city_code, percent_coverage = i / N) %>%
    bind_rows(perc_coverage)
  })
}
city_info <- read.csv("Z:/city_info.csv") %>%
  tibble() %>%
  mutate(city_code = substr(URAU_COD_1, 1, 5),
        URAU_NAME = ifelse(city_code == "XK003", "Mitrovica", URAU_NAME))

city_sf <- read_sf(city_boundaries) %>%
  rename(city_code = URAU_CODE) %>%
  left_join(perc_coverage) %>%
  left_join(city_info, by = "city_code")

ggplot() +
  geom_sf(data = city_sf, aes(col = percent_coverage, fill = percent_coverage)) +
  labs(title = "OSM coverage of European cities",
       color = "Percent of UA residential \n class covered by OSM") +
  guides(fill = "none") +
  theme(legend.position = "bottom")
