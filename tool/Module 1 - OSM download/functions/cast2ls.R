library(dplyr)
library(sf)
library(ggplot2)


cast2ls <- function(gpkg, layer) {
  
  if (grepl("osm_multipolygons", layer)) {
    st_read(gpkg, layer) %>%
      st_cast("MULTIPOLYGON") %>%
      st_cast("POLYGON") %>%
      st_cast("MULTILINESTRING") %>%
      st_cast("LINESTRING") %>% 
      return()
  } else if (grepl("osm_polygons", layer)) {
    st_read(gpkg, layer) %>%
      st_cast("POLYGON") %>%
      st_cast("MULTILINESTRING") %>%
      st_cast("LINESTRING") %>% 
      return()
  } else if (grepl("osm_multilines", layer)) {
    st_read(gpkg, layer) %>%
      st_cast("MULTILINESTRING") %>%
      st_cast("LINESTRING") %>% 
      return()
  } else if (grepl("osm_lines", layer)) {
    st_read(gpkg, layer) %>%
      st_cast("LINESTRING") %>% 
      return()
  }
  
}


uniteLayers <- function(gpkg) {
  
  layernames <-
    gpkg %>% 
    st_layers() %>% 
    .$name
  
  if (grepl("points", layernames) & length(layernames) == 1) {
    message("...") %>% 
      return()
  } else {
    
    tempdf <- cast2ls(gpkg, layernames[1])
    
    for (layer in layernames[2:length(layernames)]) {
      tempdf <- 
        cast2ls(gpkg, layer) %>% 
        bind_rows(tempdf, .) 
    }
    
    return(tempdf)
  }
}


unionCity <- function(fileDir, cityCode) {
  
  inputList <- 
    list.files(fileDir, 
               pattern = cityCode, 
               full.names = TRUE) %>% 
    grep(".gpkg$", ., value = TRUE)
  
  tempList <- uniteLayers(inputList[1])
  
  for (gpkg in inputList[2:length(inputList)]) {
    try({
      tempList <-
        uniteLayers(gpkg) %>% 
        bind_rows(tempList, .)
    })
  }
  distinct(tempList) %>% 
    return()
  
}

paths_all <- unionCity("E:/osm_paths/", "DE001.")

st_write(paths_all, "E:/temp/paths.gpkg", append = FALSE)

barriers_all <- unionCity("E:/osm_barriers/", "DE001.")

st_write(barriers_all, "E:/temp/barriers.gpkg", append = FALSE)

################################################################################


# leipzig1 <- uniteLayers(input[1])
# leipzig2 <- uniteLayers(input[2])
# 
# leipzig12 <- bind_rows(leipzig1, leipzig2)
# leipzigd <- distinct(leipzig12)


# test <- cast2ls(gpkg = gpkg, layer = layernames[2]) 
# 
# test <- st_read(gpkg, layer) %>%
#   st_cast("MULTILINESTRING") %>%
#   st_cast("LINESTRING") 
# 
# q <- bind_rows(tempdf, test)
#   

# wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
# 
# cast2ls <- function(gpkg) {
#   
#   layernames <-
#     gpkg %>% 
#     st_layers() %>% 
#     .$name
#   
#   tempdf <- st_sfc(crs = wgs84) %>% st_sf()
#   
#   for (layer in layernames) {
#     
#     if (grepl("osm_multipolygons", layer)) {
#       tempdf <-
#         st_read(gpkg, layer) %>%
#         st_cast("MULTIPOLYGON") %>%
#         st_cast("POLYGON") %>%
#         st_cast("MULTILINESTRING") %>%
#         st_cast("LINESTRING") %>% 
#         rbind(tempdf)
#     }
#     if (grepl("osm_polygons", layer)) {
#       tempdf <-
#         st_read(gpkg, layer) %>%
#         st_cast("POLYGON") %>%
#         st_cast("MULTILINESTRING") %>%
#         st_cast("LINESTRING") %>% 
#         rbind(tempdf)
#     }
#     if (grepl("osm_multilines", layer)) {
#       tempdf <-
#         st_read(gpkg, layer) %>%
#         st_cast("MULTILINESTRING") %>%
#         st_cast("LINESTRING") %>% 
#         rbind(tempdf)
#     }
#     if (grepl("osm_lines", layer)) {
#       tempdf <-
#         st_read(gpkg, layer) %>%
#         st_cast("LINESTRING") %>% 
#         rbind(tempdf)
#     }
#     
#   }
#   return(tempdf)
# }
# 
# 
# 
# #
# test <- cast2ls(input[1])
# gpkg <- input[1]
# layer <-layernames[2]
# bind_rows(a, b, c, d) %>%
#   write_sf("E:/temp/DE008.gpkg")
# # a <- st_read(input[1], layer = x[4])
# # b <- st_read(input[1], layer = x[3])
# # c <- st_read(input[1], layer = x[2])
# # d <- st_read(input[1], layer = x[1])
# # 
# # e <- st_union(a, b)
# # f <- st_union(c, d)
# 
# # 
# # st_union(a, b, c, d)
# # 
# # %>% 
# #  %>% 
# #   st_cast("MULTILINESTRING") %>% 
# #   st_cast("LINESTRING")
# 
# 
# layer <- layernames[2]