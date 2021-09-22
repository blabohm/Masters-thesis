library(dplyr)

in_sf <-
  "E:/citiesEurope/Cities.shp" %>%
  sf::st_read()

in_xlsx <-
  "E:/citiesEurope/Cities.xlsx" %>%
  readxl::read_xlsx()w


glimpse(in_xlsx)
test <-
  merge(in_sf, in_xlsx, by = "FUA_CODE", all = TRUE) %>% 
  filter(Pop2018 > 500000)


sf::write_sf(test, "E:/citiesEurope/Pop500k.gpkg")
