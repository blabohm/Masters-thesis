in_vector <- bbList[to_do[23],]
in_vector <- bbList %>% filter(cityTag == "AT001_11")
OSMkey = "building"
OSMkey = "highway"
OSMkey = "barrier"

################################################################################
# create function to download OSM files:
dlOSM <- function(in_vector, # list of gpkg files 
                  out_path,  # output directory
                  OSMkey
){
  # load packages
  require(dplyr)
  
  api_list <- dplyr::tibble(interpreter = 
                             c('http://overpass-api.de/api/interpreter',
                               'https://lz4.overpass-api.de/api/interpreter',
                               'https://z.overpass-api.de/api/interpreter'#,
                               #'https://overpass.kumi.systems/api/interpreter'
                             ))
  
  # set overpass API to use
  interpreter <- APIselect(api_list = api_list)
  osmdata::set_overpass_url(interpreter) 
  
  # create boundary box (bbox)
  bb <- 
    df2bb(in_vector) 
  
  ## output destination
  dsn <-
    paste0(out_path,
           in_vector$cityTag,
           ".gpkg")
  
  # message
  message(paste("Commencing download of: ", in_vector$cityTag,
                "at: ", interpreter))
  
  OSMdownloader(bb, OSMkey, dsn)
  
  # check if file exists
  if (file.exists(dsn)) {
    
    message("Proceeding...")
    
  } else {
    
    message("Nothing to write.")
    
  }
}

################################################################################

includeHighway <- function(OSMsf) {
  
  require(dplyr)
  
  OSMsf %>% 
    select(contains("id"), 
           contains("way"), 
           contains("foot"), 
           contains("side"), 
           contains("cycle")) %>% 
    return()
  
}

includeBuilding <- function(OSMsf) {
  
  require(dplyr)
  
  OSMsf %>% 
    select(contains("id"), 
           contains("build"), 
           contains("amenity"), 
           contains("value")) %>% 
    return()
  
}

includeBarriers <- function(OSMsf) {
  
  require(dplyr)
  
  OSMsf %>% 
    select(contains("id"), 
           contains("barrier"), 
           contains("access")) %>% 
    return()
  
}

exclude <- function(OSMsf) {
  
  require(dplyr)
  
  OSMsf %>% 
    select(- contains("fid"),- 
             contains("wiki"), 
           - contains("object"), 
           - matches("[.]")) %>% 
    return()
}

################################################################################

OSMdownloader <- function(bb, key, dsn) {
  # download OSM data for boundary layer
  OSMtemp <-
    tryCatch({
      bb %>% 
        ## create OSM query
        osmdata::opq() %>% 
        ## add desired feature to query
        osmdata::add_osm_feature(., 
                                 key = OSMkey) %>% 
        ## download OSM data
        osmdata::osmdata_sf(.) 
    }, error = function(e) {
      message("Download failed. Original error message:")
      message(e)
      return(NULL)
    })
  
  if (!is.null(OSMtemp)) tryCatch({
    
    message("Download successful.")
    
    # check for osm key and filter for columns (not all OSM columns work for
    # st_write)
    if (grepl("way", OSMkey)) {
      
      if (!is.null(OSMtemp$osm_lines)) tryCatch({
        OSMtemp$osm_lines %>% 
          includeHighway() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_lines", 
                       append = FALSE, quiet = TRUE)},
        error = function(e) message(e))
      
      if (!is.null(OSMtemp$osm_multilines)) tryCatch({
        OSMtemp$osm_multilines %>% 
          includeHighway() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_multilines", 
                       append = FALSE, quiet = TRUE)},
        error = function(e) message(e))
      
      if (!is.null(OSMtemp$osm_polygons)) tryCatch({
        OSMtemp$osm_polygons %>% 
          includeHighway() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_polygons",
                       append = FALSE, quiet = TRUE)},
        error = function(e) message(e))
      
      if (!is.null(OSMtemp$osm_multipolygons)) tryCatch({
        OSMtemp$osm_multipolygons %>% 
          includeHighway() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_multipolygons",
                       append = FALSE, quiet = TRUE)},
        error = function(e) message(e))
      
      #clean up
      rm(OSMtemp)
      # garbage collector
      gc()
      
    }
    
    if (grepl("build", OSMkey)) {
      
      if (!is.null(OSMtemp$osm_polygons)) tryCatch({
        OSMtemp$osm_polygons %>% 
          includeBuilding() %>% 
          exclude() %>%  
          sf::st_write(dsn = dsn, layer = "osm_polygons",
                       append = FALSE, quiet = TRUE)},
        error = function(e) message(e))
      
      if (!is.null(OSMtemp$osm_multipolygons)) tryCatch({
        OSMtemp$osm_multipolygons %>% 
          includeBuilding() %>% 
          exclude() %>%  
          sf::st_write(dsn = dsn, layer = "osm_multipolygons",
                       append = FALSE, quiet = TRUE)},
        error = function(e) message(e))
      
      #clean up
      rm(OSMtemp)
      # garbage collector
      gc()
      
    }   
    
    if (grepl("barrier", OSMkey)) {
      
      if (!is.null(OSMtemp$osm_points)) tryCatch({
        OSMtemp$osm_points %>%          
          includeBarriers() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_points", 
                       append = FALSE, quiet = TRUE)},
        error = function(e) message(e))
      
      if (!is.null(OSMtemp$osm_lines)) tryCatch({
        OSMtemp$osm_lines %>% 
          includeBarriers() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_lines",
                       append = FALSE, quiet = TRUE)},
        error = function(e) message(e))
      
      if (!is.null(OSMtemp$osm_multilines)) tryCatch({
        OSMtemp$osm_multilines %>% 
          includeBarriers() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_multilines",
                       append = FALSE, quiet = TRUE)},
        error = function(e) message(e))
      
      if (!is.null(OSMtemp$osm_polygons)) tryCatch({
        OSMtemp$osm_polygons %>% 
          includeBarriers() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_polygons",
                       append = FALSE, quiet = TRUE)},
        error = function(e) message(e))
      
      if (!is.null(OSMtemp$osm_multipolygons)) tryCatch({
        OSMtemp$osm_multipolygons %>% 
          includeBarriers() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_multipolygons",
                       append = FALSE, quiet = TRUE)},
        error = function(e) message(e))
      
      #clean up
      rm(OSMtemp)
      # garbage collector
      gc()
      
    }
  }, error = function(e){
    
    message(e)
    message("..")
    
    # if (file.exists(dsn)) {
    #   message("Something went wrong during file creation.")
    #   message("Deleting output.")
    #   unlink(dsn, force = TRUE)
    # }
    
    if (!file.exists(dsn)) {
      message("Area does not seem to contain desired data.")
      message("Writing placeholder file.")
      
      dsn <- gsub("gpkg", "txt", dsn)
      write(bb, dsn, append = FALSE)}
    
  })
}
################################################################################