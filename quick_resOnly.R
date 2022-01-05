library(dplyr)
library(sf)

berlin <- "C:/Berlin/berlin_cent_core.gpkg" %>% 
  st_read() %>% 
  st_transform(3035)
  

# urban atlas (ua) residential classes
res_class <- c(11100, 11210, 11220, 
               11230, 11240, 11300)

# get name of UA land-use layer
ua_file <- "C:/Berlin/UA2018/DE001/DE001L1_BERLIN_UA2018_v013.gpkg"
lr <- st_layers(ua_file)$name[1]

# load UA file and filter residential classes
ua <-
  st_read(ua_file, lr) %>% 
  filter(code_2018 %in% res_class)

berlin %>% 
  st_intersection(ua) %>%
  filter(population > 0) %>%
  select(code_2018, population, identifier) %>% 
  st_write("C:/Berlin/resOnly.gpkg") 
  