# load all available boundaries
# filter conditions:
# geometry_t1 is partially contained in any geometry_t2
# AND FUA_t1 != FUA_geometry_t2
# -> create colmn 'inside' that contains FUA of geometry_t2
# -> filter for FUA_t1 != inside

# dplyr ftw
library(dplyr)

# load data
mt <- 
  read.csv("Z:/UA/master_table.csv")

UA06 <- 
  sf::st_read("C:/LandOeko/ua_boundaries/UA2006_Boundary_aa.gpkg") %>% 
  mutate(FUA06 = substr(FUA_OR_CIT, 1, 5)) %>% 
  select(FUA06) %>% 
#  sf::st_buffer(dist = -1) %>% 
  sf::st_make_valid(.)


UA12 <- sf::st_read("C:/LandOeko/ua_boundaries/UA2012_Boundary_aa.gpkg") %>% 
  mutate(FUA12 = substr(fua_code, 1, 5)) %>% 
  select(FUA12) %>% 
  sf::st_make_valid(.)

UA18 <- sf::st_read("C:/LandOeko/ua_boundaries/UA2018_Boundary_aa.gpkg") %>% 
  mutate(FUA18 = substr(fua_code, 1, 5)) %>% 
  select(FUA18) %>% 
  sf::st_buffer(dist = -1) %>% 
  sf::st_make_valid(.)

UA0612 <-
  sf::st_join(UA06, UA12, join = sf::st_overlaps, largest = TRUE) %>% 
  merge(mt, all.x = TRUE)

UA1218 <-
  sf::st_join(UA12, UA18, join = sf::st_overlaps, largest = TRUE)

uafiltered0612 <- 
  filter(UA0612,
         !is.na(FUA12),
         FUA06 != FUA12)

uafiltered1218 <- 
  filter(UA1218,
         !is.na(FUA18),
         FUA12 != FUA18)


# DELFT NL017 -> NL001
# TURNBRIDGE_WELLS UK040 -> UK001