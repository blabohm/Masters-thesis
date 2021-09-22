library(dplyr)
library(sf)
library(ggplot2)


cast2cent <- function(gpkg, layer) {
  
  if (grepl("osm_multipolygons", layer)) {
    st_read(gpkg, layer) %>% 
      st_make_valid() %>%
      st_cast("POLYGON") %>% 
      st_centroid() %>%  
      return()
  } else if (grepl("osm_polygons", layer)) {
    st_read(gpkg, layer) %>%
      st_centroid() %>%  
      return()
  } else if (grepl("osm_multilines", layer)) {
    st_read(gpkg, layer) %>%
      st_centroid() %>%  
      return()
  } else if (grepl("osm_lines", layer)) {
    st_read(gpkg, layer) %>%
      st_centroid() %>%  
      return()
  }
  
}


uniteLayerscent <- function(gpkg) {
  
  layernames <-
    gpkg %>% 
    st_layers() %>% 
    .$name
  
  tempdf <- cast2cent(gpkg, layernames[1])
  
  for (layer in layernames[2:length(layernames)]) {
    tempdf <- 
      cast2cent(gpkg, layer) %>% 
      bind_rows(tempdf, .) 
  }
  
  return(tempdf)
}


unionCity <- function(fileDir, cityCode) {
  
  inputList <- 
    list.files(fileDir, 
               pattern = cityCode, 
               full.names = TRUE)
  
  tempList <- uniteLayerscent(inputList[1])
  
  for (gpkg in inputList[2:length(inputList)]) {
    tempList <-
      uniteLayerscent(gpkg) %>% 
      bind_rows(tempList, .)
  }
  distinct(tempList) %>% 
    return()
  
}

leipzigAll <- unionCity("E:/osm_buildings/", "DE008.")
st_write(leipzigAll, "E:/temp/Leipzig_buildings.gpkg")

gpkg <- "E:/UA2018/DE008L2_LEIPZIG_UA2018_v012/Data/DE008L2_LEIPZIG_UA2018_v012.gpkg"
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"


lr <- st_layers(gpkg)

lpzg <- st_read(gpkg, lr$name[1])
lpzg <- 
  lpzg %>% 
  filter(code_2018 %in% c(11100, 11210, 11220, 
                          11230, 11240, 11300)) %>% 
  st_transform(wgs84)

lpzg_singlepart <-
  lpzg %>% select(geom) %>% st_union(by_feature = FALSE, is_coverage = TRUE) %>%
  st_sf() 
  
  st_cast("POLYGON") %>% st_cast("MULITPOLYGON")


lpzg_build <- 
  st_read("E:/temp/Leipzig_buildings.gpkg") %>% 
  st_transform(wgs84)

lpzg_build <- 
  lpzg_build %>% 
  filter(st_intersects(x = ., y = lpzg_singlepart, sparse = FALSE))

st_write(lpzg_build, "E:/temp/Leipzig_buildings_filtered.gpkg", append = FALSE)
