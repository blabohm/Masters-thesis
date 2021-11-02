library(dplyr)
library(sf)
library(ggplot2)



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

berlin <- unionCity(osm_dir, ua_dir, cityCode)
st_write(berlin, "C:/Berlin/berlin_cent.gpkg", append = FALSE)

################################################################################

FUAlist <-   
  readxl::read_xlsx("Z:/R_skripts/StädtelisteBenni.xlsx",
                    sheet = 2) %>% 
  filter(Select == 1) %>% 
  transmute(code = substr(Code, 1, 5)) %>% 
  add_row(code = "DE038") %>% 
  pull(code)

for (cityCode in FUAlist) {
  
  unionCity(osm_dir = osm_dir, 
            ua_dir = ua_dir,
            cityCode = cityCode) %>% 
    st_write(paste0(osm_outdir, cityCode, ".gpkg"))
  
}

################################################################################

cast2cent <- function(osm_file, 
                      ua, 
                      layername,
                      res_class) {
  
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
    select(code_2018, identifier, 
           area, area_sum, pop_per_sqm, 
           pop_by_area, population, Pop2018) %>%  
    return()
}

################################################################################

uniteLayerscent <- function(osm_file, 
                            ua, 
                            res_class) {
  
  layernames <-
    osm_file %>% 
    st_layers() %>% 
    .$name %>% 
    grep("poly", ., 
         ignore.case = TRUE, value = TRUE)
  
  tempdf <- cast2cent(osm_file = osm_file, ua = ua, 
                      layername = layernames[1],
                      res_class = res_class)
  
  if (length(layernames) > 1) {
    
    tempdf <- 
      cast2cent(osm_file = osm_file, ua = ua, 
                layername = layernames[2],
                res_class = res_class) %>% 
      bind_rows(tempdf, .) }
  
  return(tempdf)
}

################################################################################

unionCity <- function(osm_dir, 
                      ua_dir, 
                      cityCode) {
  
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
    st_read(ua_file, lr) %>% 
    filter(code_2018 %in% res_class) %>% 
    st_transform(4326) %>% 
    select(Pop2018, identifier, code_2018)
  
  osm_list <- 
    list.files(osm_dir, 
               pattern = cityCode, 
               full.names = TRUE)
  
  tempList <- 
    uniteLayerscent(osm_file = osm_list[1],
                    ua = ua,
                    res_class = res_class)
  
  for (osm_file in osm_list[2:length(osm_list)]) {
    tempList <-
      uniteLayerscent(osm_file = osm_file, 
                      ua = ua,
                      res_class = res_class) %>% 
      bind_rows(tempList, .)
  }
  distinct(tempList) %>% 
    return()
  
}
