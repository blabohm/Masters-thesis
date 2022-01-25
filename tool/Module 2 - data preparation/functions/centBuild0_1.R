# library(dplyr)
# library(sf)
# library(ggplot2)



################################################################################
# TESTING AREA
################################################################################
osm_dir <-  "C:/Berlin/osm_buildings_temp/"
ua_dir <- "C:/Berlin/UA2018/"
# osm_outdir <- "Y:/openstreetmap/osm_buildings/"
# 
# osm_dir <-  "Y:/openstreetmap/osm_buildings_temp/"
# ua_dir <- "Z:/UA2018/"
# osm_outdir <- "Y:/openstreetmap/osm_buildings/"
cityCode <- "DE001"

# osm_file <- osm_file <- osm_list[11]
# osm_file <- osm_list[4]

# layername <- layernames[2]

# berlin <- unionCity(osm_dir, ua_dir, cityCode)
# st_write(berlin, "C:/Berlin/berlin_cent.gpkg", append = FALSE)

################################################################################

# FUAlist <-   
#   readxl::read_xlsx("Z:/R_skripts/StädtelisteBenni.xlsx",
#                     sheet = 2) %>% 
#   filter(Select == 1) %>% 
#   transmute(code = substr(Code, 1, 5)) %>% 
#   add_row(code = "DE038") %>% 
#   pull(code)
# 
# for (cityCode in FUAlist) {
#   
#   unionCity(osm_dir = osm_dir, 
#             ua_dir = ua_dir,
#             cityCode = cityCode) %>% 
#     st_write(paste0(osm_outdir, cityCode, ".gpkg"))
#   
# }


################################################################################

unionCity <- function(osm_dir, 
                      ua_dir, 
                      network_dir,
                      cityCode) {
  
  # urban atlas (ua) residential classes
  res_class <- c(11100, 11210, 11220, 
                 11230, 11240, 11300)
  
  # load UA file and filter residential classes
  ua <- loadUAres(ua_dir, cityCode)
  
  # load network
  network <- loadNetwork(network_dir)
  
  osm_list <- 
    list.files(osm_dir, 
               pattern = cityCode, 
               full.names = TRUE)
  
  tempList <- 
    uniteLayers(osm_file = osm_list[1],
                ua = ua,
                network = network,
                res_class = res_class)
  
  for (osm_file in osm_list[2:length(osm_list)]) {
    tempList <-
      uniteLayers(osm_file = osm_file, 
                  ua = ua,
                  network = network,
                  res_class = res_class) %>% 
      bind_rows(tempList, .)
  }
  distinct(tempList) %>% 
    return()
  
}


################################################################################

uniteLayers <- function(osm_file, 
                        ua, 
                        res_class,
                        network) {
  
  layernames <-
    osm_file %>% 
    st_layers() %>% 
    .$name %>% 
    grep("poly", ., 
         ignore.case = TRUE, value = TRUE)
  
  osmBuild <- OSMloader(osm_file, layernames[1])
  
  network_tile <-
    network %>% 
    getNetworkTile()
  
  tempdf <- centPop(osm_file = osm_file, ua = ua, 
                    layername = layernames[1],
                    res_class = res_class,
                    network_tile)
  
  if (length(layernames) > 1) {
    
    tempdf <- 
      centPop(osm_file = osm_file, ua = ua, 
              layername = layernames[2],
              res_class = res_class,
              network_tile) %>% 
      bind_rows(tempdf, .) }
  
  return(tempdf)
}


################################################################################
# CENTROIDS, POPULATION, SNAP
centPop <- function(osm_file, 
                    ua, 
                    layername,
                    res_class,
                    network_tile) {
  
  OSMloader(osm_file, layername) %>% 
    getPop(ua, res_class) %>% 
    snap2Network(network_tile) %>% 
    return()
  
}


################################################################################

loadUAres <- function(ua_dir, city_code, crs = 3035) {
  
  require(dplyr)
  require(sf)
  
  # urban atlas (ua) residential classes
  res_class <- c(11100, 11210, 11220, 
                 11230, 11240, 11300)
  
  # get ua file with right code
  ua_file <- 
    list.files(ua_dir, 
               pattern = cityCode, 
               full.names = TRUE) %>% 
    list.files(pattern = "gpkg$", 
               full.names = TRUE,
               recursive = TRUE)
  
  # get name of UA land-use layer
  lr <- st_layers(ua_file)$name[1]
  
  # load UA file and filter residential classes
  ua <-
    st_read(ua_file, lr, quiet = TRUE) %>% 
    filter(code_2018 %in% res_class) %>% 
    st_transform(crs) %>% 
    select(Pop2018, identifier, code_2018)
  
  return(ua)
}


################################################################################
# GET NETWORK DATA INSIDE OSM BOUNDARY BOX
getNetworkTile <- function(networkLayer, OSMtile) {
  
  OSMtile %>% st_bbox() 
}


################################################################################
# LOAD NETWORK FILE
loadNetwork <- function(network_dir, crs = 3035) {
  
  network_dir %>% 
    st_read() %>% 
    select(highway) %>% 
    st_transform(3035) %>% 
    distinct() %>% 
    return()
  
}


################################################################################
# OSM LOADER

OSMloader <- function(osm_file, layername) {
  # load osm file, add area and join with ua 
  if (grepl("osm_multipolygons", layername)) {
    osm <-
      st_read(osm_file, layername) %>% 
      st_make_valid() %>%
      st_cast("POLYGON") 
  } else if (grepl("osm_polygons", layername)) {
    osm <- 
      st_read(osm_file, layername) 
  }
  osm %>% 
    select(matches("buidling$"))
  return()
}


################################################################################
# GET POP

getPop <- function(osm, ua, res_class) {
  # add area column
  osm <- 
    osm %>% 
    st_make_valid() %>% 
    mutate(area = st_area(.)) %>% 
    st_join(y = ua)
  
  # sum osm areas in each ua polygon (by ua identifier)
  area_df <- 
    osm %>% 
    st_drop_geometry() %>% 
    group_by(identifier) %>% 
    summarise(area_sum = sum(area))
  
  # calculate Population per building by building area
  pop_sf <-
    osm %>% 
    merge(area_df) %>% 
    mutate(population = (area / area_sum * Pop2018) %>% 
             round()) %>% 
    select(population, area, area_sum, 
           Pop2018, identifier, code_2018) %>% 
    st_point_on_surface() 
  
  # calculate population per building by average pop / area / ua class
  pop_df <-
    pop_sf %>% 
    na.omit() %>% 
    st_drop_geometry() %>%
    mutate(pop_per_sqm = population / area) %>% 
    group_by(code_2018) %>% 
    summarise(pop_per_sqm = mean(pop_per_sqm)) 
  
  pop_sf %>% 
    merge(pop_df, all.x = TRUE) %>% 
    mutate(pop_by_area = (area * pop_per_sqm) %>% round()) %>% 
    mutate(population = as.double(population),
           pop_by_area = as.double(pop_by_area)) %>% 
    mutate(population = ifelse(population == 0 & pop_by_area > population, 
                               pop_by_area, population)) %>% 
    select(code_2018, identifier, 
           area, area_sum, pop_per_sqm, 
           pop_by_area, population, Pop2018) %>%  
    return()
}


################################################################################
# SNAP
snap2Network <- function(osm, network) {
  
  
}