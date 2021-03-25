dlOSM <- function(in_vector, # list of gpkg files 
                  out_path  # output directory
){
  # load packages
  require(dplyr)
  
  # Set overpass API to use 
  api_list <- c('http://overpass-api.de/api/interpreter',
                'https://lz4.overpass-api.de/api/interpreter',
                'https://z.overpass-api.de/api/interpreter',
                'https://overpass.kumi.systems/api/interpreter'
                )
  
  api_to_use <- sample(1:length(api_list), 1)
  
  osmdata::set_overpass_url(api_list[api_to_use]) 
  
  # create boundary box (bbox)
  bb <- 
    matrix(in_vector[2:5] %>% unlist(), 
           nrow = 2, ncol = 2, byrow = T,
           dimnames = list(c('x', 'y'), c('min', 'max'))
    )
  
  # download OSM data for boundary layer
    try({
      buildings <-
      bb %>% 
        ## create boundary box
        sp::bbox() %>%
        ## create OSM query
        osmdata::opq() %>% 
        ## add desired feature to query
        osmdata::add_osm_feature(., 
                                 key = "building") %>% 
        ## download OSM data
        osmdata::osmdata_sp() 
    }, silent = T)
  
  # check if download worked
  if (!exists('buildings')) return(message("Download failed!"))
  
  # select polygons from osm data
  bPolygons <- buildings$osm_polygons
  
  # kill queries
  killLink <- paste0(api_to_use,
                     '/kill_my_queries')
  shell.exec(killLink)
  
  # remove buildings object
  rm (buildings)
  gc()
  
  # convert FIDs from numeric to character
  FIDs <- 
    bPolygons@polygons %>% 
    names() %>% 
    as.character()
  
  bPolygons <- 
    sp::spChFIDs(bPolygons, FIDs)
  ################################################################################
  # kick out most of the columns of @data
  bPolygons@data <-
    bPolygons@data[1]
  ################################################################################
  # generate parameters for write OGR 
  ## output destination
  dsn <-
    paste0(out_path,
           in_vector[1],
           ".gpkg")
  
  ## layer name
  lyr = "buildings"
  
  # write to file
  rgdal::writeOGR(bPolygons,
                  dsn = dsn,
                  layer = lyr,
                  driver = "GPKG",
                  overwrite_layer = T)
  
  Sys.sleep(60)
}



bbList <- 
  bbList %>% 
  mutate(size = (xmax - xmin) * (ymax - ymin))
