sabb <- sf::read_sf("E:/Downloads/sabb.gpkg") %>% 
  sf::st_bbox() %>%  
  matrix(ncol = 2) %>% 
  bb2df() %>%
  #df2bb() %>% 
  mutate(cityTag = "South_africa") %>% 
  tibble() %>% 
  df2bb()

dsn <- "E:/south_africa_streets.gpkg"

OSMdownloader(sabb, OSMkey, dsn)

df2bb(sabb)
