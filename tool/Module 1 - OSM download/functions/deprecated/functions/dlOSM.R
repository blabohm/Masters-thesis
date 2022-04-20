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
  
  # set overpass API to use
  interpreter <- APIselect(api_list = dplyr::tibble(interpreter = 
                                                      c('http://overpass-api.de/api/interpreter',
                                                        'https://lz4.overpass-api.de/api/interpreter',
                                                        'https://z.overpass-api.de/api/interpreter'#,
                                                        #'https://overpass.kumi.systems/api/interpreter'
                                                      )))
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
      
      #if (!is.null(OSMtemp$osm_points)) {
      #  OSMtemp$osm_points %>% 
      #   # try to keep columns:
      # select(contains("id"), contains("way"), contains("foot"), contains("footway"), 
      #        contains("side"), contains("sidewalk"), contains("cycleway"),
      #        # try to exclude columns:
      #        - contains("fid"),- contains("wiki"), - matches("[.]")) %>% 
      #    sf::st_write(dsn = dsn, layer = "osm_points", append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_lines)) {
        OSMtemp$osm_lines %>% 
          # try to keep columns:
          select(contains("id"), contains("way"), contains("foot"), contains("footway"), 
                 contains("side"), contains("sidewalk"), contains("cycleway"),
                 # try to exclude columns:
                 - contains("fid"),- contains("wiki"), - matches("[.]")) %>%   
          sf::st_write(dsn = dsn, layer = "osm_lines", append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_multilines)) {
        OSMtemp$osm_multilines %>% 
          # try to keep columns:
          select(contains("id"), contains("way"), contains("foot"), contains("footway"), 
                 contains("side"), contains("sidewalk"), contains("cycleway"),
                 # try to exclude columns:
                 - contains("fid"),- contains("wiki"), - matches("[.]")) %>% 
          sf::st_write(dsn = dsn, layer = "osm_multilines", append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_polygons)) {
        OSMtemp$osm_polygons %>% 
          # try to keep columns:
          select(contains("id"), contains("way"), contains("foot"), contains("footway"), 
                 contains("side"), contains("sidewalk"), contains("cycleway"),
                 # try to exclude columns:
                 - contains("fid"),- contains("wiki"), - matches("[.]")) %>% 
          sf::st_write(dsn = dsn, layer = "osm_polygons", append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_multipolygons)) {
        OSMtemp$osm_multipolygons %>% 
          # try to keep columns:
          select(contains("id"), contains("way"), contains("foot"), contains("footway"), 
                 contains("side"), contains("sidewalk"), contains("cycleway"),
                 # try to exclude columns:
                 - contains("fid"),- contains("wiki"), - matches("[.]")) %>% 
          sf::st_write(dsn = dsn, layer = "osm_multipolygons", append = FALSE, quiet = TRUE)}
      
      #clean up
      rm(OSMtemp)
      # garbage collector
      gc()
      
    }
    
    if (grepl("build", OSMkey)) {
      # points only seem to give nodes of building polygons
      #if (!is.null(OSMtemp$osm_points)) {
      #  OSMtemp$osm_points %>% 
      # try to keep columns:
      # select(contains("id"), contains("build"), contains("amenity"),
      #        # try to exclude columns:
      #        - contains("fid"),- contains("wiki"), - matches("[.]")) %>%      
        #    sf::st_write(dsn = dsn, layer = "osm_points", append=FALSE)}
      
      if (!is.null(OSMtemp$osm_lines)) {
        OSMtemp$osm_lines %>% 
          # try to keep columns:
          select(contains("id"), contains("build"), contains("amenity"),
                 # try to exclude columns:
                 - contains("fid"),- contains("wiki"), - matches("[.]")) %>% 
          sf::st_write(dsn = dsn, layer = "osm_lines", append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_multilines)) {
        OSMtemp$osm_multilines %>% 
          # try to keep columns:
          select(contains("id"), contains("build"), contains("amenity"),
                 # try to exclude columns:
                 - contains("fid"),- contains("wiki"), - matches("[.]")) %>% 
          sf::st_write(dsn = dsn, layer = "osm_multilines", append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_polygons)) {
        OSMtemp$osm_polygons %>% 
          # try to keep columns:
          select(contains("id"), contains("build"), contains("amenity"),
                 # try to exclude columns:
                 - contains("fid"),- contains("wiki"), - matches("[.]")) %>% 
          sf::st_write(dsn = dsn, layer = "osm_polygons", append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_multipolygons)) {
        OSMtemp$osm_multipolygons %>% 
          # try to keep columns:
          select(contains("id"), contains("build"), contains("amenity"),
                 # try to exclude columns:
                 - contains("fid"),- contains("wiki"), - matches("[.]")) %>% 
          sf::st_write(dsn = dsn, layer = "osm_multipolygons", append = FALSE, quiet = TRUE)}
      
      #clean up
      rm(OSMtemp)
      # garbage collector
      gc()
      
    }   
    
    if (grepl("barrier", OSMkey)) {
      
      if (!is.null(OSMtemp$osm_points)) {
        OSMtemp$osm_points %>%          
          # try to keep columns:
          select(contains("id"), contains("build"), contains("access"), 
                 # try to exclude columns:
                 - contains("fid"),- contains("wiki"), - matches("[.]")) %>% 
          sf::st_write(dsn = dsn, layer = "osm_points", append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_lines)) {
        OSMtemp$osm_lines %>% 
          # try to keep columns:
          select(contains("id"), contains("build"), contains("access"), 
                 # try to exclude columns:
                 - contains("fid"),- contains("wiki"), - matches("[.]")) %>% 
          sf::st_write(dsn = dsn, layer = "osm_lines", append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_multilines)) {
        OSMtemp$osm_multilines %>% 
          # try to keep columns:
          select(contains("id"), contains("build"), contains("access"), 
                 # try to exclude columns:
                 - contains("fid"),- contains("wiki"), - matches("[.]")) %>% 
          sf::st_write(dsn = dsn, layer = "osm_multilines", append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_polygons)) {
        OSMtemp$osm_polygons %>% 
          # try to keep columns:
          select(contains("id"), contains("build"), contains("access"), 
                 # try to exclude columns:
                 - contains("fid"),- contains("wiki"), - matches("[.]")) %>% 
          sf::st_write(dsn = dsn, layer = "osm_polygons", append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_multipolygons)) {
        OSMtemp$osm_multipolygons %>% 
          # try to keep columns:
          select(contains("id"), contains("build"), contains("access"), 
                 # try to exclude columns:
                 - contains("fid"),- contains("wiki"), - matches("[.]")) %>% 
          sf::st_write(dsn = dsn, layer = "osm_multipolygons", append = FALSE, quiet = TRUE)}
      
      #clean up
      rm(OSMtemp)
      # garbage collector
      gc()
    }
  }, error = function(e){
    
    message(e)
    message("..")
    
    if (file.exists(dsn)) {
      message("Something went wrong during file creation.")
      message("Deleting output.")
      unlink(dsn, force = TRUE)
    }
    
    if (!file.exists(dsn)) {
      message("Area does not seem to contain desired data.")
      message("Writing placeholder file.")
      
      dsn <- gsub("gpkg", "txt", dsn)
      write(bb, dsn, append = FALSE)}
    
  })
  
  # check if file exists
  if (file.exists(dsn)) {
    
    message("Proceeding...")
    
  } else {
    
    message("Nothing to write.")
    
  }
}


################################################################################