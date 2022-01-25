library(dplyr)
library(sf)
library(igraph)
library(tidygraph)
library(sfnetworks)


res_class <- c(11100, 11210, 11220,
               11230, 11240, 11300)

resLyr <-
  paste0(baseDir, "UA2018/DE001/DE001L1_BERLIN_UA2018_v013.gpkg") %>%
  st_read(layer = lr$name[1], quiet = TRUE) %>%
  filter(code_2018 %in% res_class)

buildEntries <-
  paste0(baseDir, "berlin_cent_core.gpkg") %>%
  st_read(quiet = TRUE) %>%
  st_transform(3035) %>%
  st_intersection(resLyr) %>%
  mutate(population = ifelse(population == 0 & pop_by_area > population,
                             pop_by_area, population),
         building_id = row_number()) %>%
  filter(population > 0) %>%
  select(code_2018, population, identifier)