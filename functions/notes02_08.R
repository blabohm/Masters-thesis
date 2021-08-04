in_vector <- in_list[6,]

################################################################################
# create function to download OSM files:
dlOSM <- function(in_vector, # list of gpkg files 
                  out_path,  # output directory
                  OSMkey
){
  # load packages
  require(dplyr)
  require(sf)
  # set overpass API to use
  interpreter <- APIselect(api_list = api_list)
  osmdata::set_overpass_url(interpreter) 
  
  # create boundary box (bbox)
  if (grepl("xmax", names(in_vector)) %>% 
      any()) {
  bb <- 
    df2bb(in_vector) %>% 
    round(digits = 5)
  } else {
    wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
    bb <- 
      in_vector$value  %>%
      sf::read_sf(layer = rgdal::ogrListLayers(.)[2]) %>%
      sf::st_transform(., wgs84) %>% 
      sf::st_bbox() %>% 
      matrix(nrow = 2) %>% 
      bb2df()
  }
  ## output destination
  # dsn <-
  #   paste0(out_path,
  #          in_vector$cityTag,
  #          ".gpkg")
  # 
  
  bbList <-
    check.up2(id = in_vector$code,
              bbox = bb,
              fileDirectory = out_path)
  # message
  message(paste("Commencing download of: ", in_vector$cityTag,
                "at: ", interpreter))
  
  # download OSM data for boundary layer
  # check if download is polygons or lines
  if (grepl("way", OSMkey)) {
    
    try({    
      bb %>% 
        ## create OSM query
        osmdata::opq() %>% 
        ## add desired feature to query
        osmdata::add_osm_feature(., 
                                 key = OSMkey) %>% 
        ## download OSM data
        osmdata::osmdata_sf(.)  %>%
        .$osm_lines %>% 
        select(osm_id, building, amenity) %>% 
        sf::st_write(dsn = dsn, layer = OSMkey)
      
      # garbage collector
      gc()
      
    }, silent = T)
  } else {
    
    try({    
      bb %>% 
        ## create OSM query
        osmdata::opq() %>% 
        ## add desired feature to query
        osmdata::add_osm_feature(., 
                                 key = OSMkey) %>% 
        ## download OSM data
        osmdata::osmdata_sf(.)  %>%
        .$osm_polygons %>% 
        select(osm_id, building, amenity) %>% 
        sf::st_write(dsn = dsn, layer = OSMkey)
      
      # garbage collector
      gc()
      
    }, silent = T)
  }
  
  # check if file exists
  if (file.exists(dsn)) {
    message("Download successful.")
  } else ({
    message("Download failed. Proceeding to next area.")
  })
}


################################################################################
