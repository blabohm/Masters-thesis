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
  perc_coverage <- tibble(city_code = city_code, perc_coverage = i / N) %>%
    bind_rows(perc_coverage)
  })
}

plot(perc_coverage)
