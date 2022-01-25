library(tidyverse)
library(sf)
library(igraph)

baseDir <- "C:/Berlin/"
res_class <- c(11100, 11210, 11220, 
               11230, 11240, 11300)

highway <- 
  paste0(baseDir, "highway_clean.gpkg") %>% 
  st_read() %>% 
  select(highway) %>% 
  st_transform(3035) %>% 
  distinct() #%>% 
  st_union() %>% 
  st_as_sf()

parkEntries <-
  paste0(baseDir, "park_entries.gpkg") %>% 
  st_read() %>% 
  select(area, identifier) %>% 
  st_transform(3035)

resLyr <- 
  paste0(baseDir, "UA2018/DE001/DE001L1_BERLIN_UA2018_v013.gpkg") %>% 
  st_read(quiet = TRUE) %>% 
  filter(code_2018 %in% res_class)

buildEntries <- 
  paste0(baseDir, "berlin_cent_core.gpkg") %>% 
  st_read() %>% 
  mutate(population = ifelse(population == 0 & pop_by_area > population, 
                             pop_by_area, population)) %>% 
  st_transform(3035) %>% 
  st_intersection(resLyr) %>% 
  select(code_2018, population, identifier) 
  
system.time({
snapTest <- 
  buildEntries %>% 
  st_snap(highway, tolerance = 50)
})
