in_vector <- in_list[2,]


################################################################################
# create function to download OSM files:
dlOSM2 <- function(in_vector, # list of gpkg files 
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
    bbCity <- 
      df2bb(in_vector) %>% 
      round(digits = 5)
  } else {
    wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
    bbCity <- 
      in_vector$value  %>%
      sf::read_sf(layer = rgdal::ogrListLayers(.)[2]) %>%
      sf::st_transform(., wgs84) %>% 
      sf::st_bbox() %>% 
      matrix(nrow = 2) %>% 
      bb2df()
  }

  bbDlList <- sf::st_sf(sf::st_sfc())
  bbDlList <-
    check.up2(id = in_vector$code,
              bbox = bbCity,
              fileDirectory = out_path,
              boundaryDirectory = in_vector$value)
  
  if (is.null(bbDlList)) {
    return(message("proceeding..."))
  } else {
    
    # message
    message(paste("Commencing download of: ", in_vector$cityTag,
                  "at: ", interpreter))
    
    for (i in 1:nrow(bbDlList)) {
      
      bb <- bbDlList[i,]
      # download OSM data for boundary layer
      dsn <- paste0(out_path, bb$code, ".gpkg")
      # check for osm key
      if (grepl("way", OSMkey)) {
        
        try({
          OSMtemp <-
            bb %>% 
            df2bb() %>% 
            ## create OSM query
            osmdata::opq() %>% 
            ## add desired feature to query
            osmdata::add_osm_feature(., 
                                     key = OSMkey) %>% 
            ## download OSM data
            osmdata::osmdata_sf(.)
          
          if (!is.null(OSMtemp$osm_points)) {
            OSMtemp$osm_points %>% 
              select(osm_id, highway, foot, footway, side, sidewalk, cycleway) %>% 
              sf::st_write(dsn = dsn, layer = "osm_points", append=FALSE)}
          
          if (!is.null(OSMtemp$osm_lines)) {
            OSMtemp$osm_lines %>% 
              select(osm_id, highway, foot, footway, side, sidewalk, cycleway) %>% 
              sf::st_write(dsn = dsn, layer = "osm_lines", append=FALSE)}
          
          if (!is.null(OSMtemp$osm_multilines)) {
            OSMtemp$osm_multilines %>% 
              select(osm_id, highway, foot, footway, side, sidewalk, cycleway) %>% 
              sf::st_write(dsn = dsn, layer = "osm_multilines", append=FALSE)}
          
          if (!is.null(OSMtemp$osm_polygons)) {
            OSMtemp$osm_polygons %>% 
              select(osm_id, highway, foot, footway, side, sidewalk, cycleway) %>% 
              sf::st_write(dsn = dsn, layer = "osm_polygons", append=FALSE)}
          
          if (!is.null(OSMtemp$osm_multipolygons)) {
            OSMtemp$osm_multipolygons %>% 
              select(osm_id, highway, foot, footway, side, sidewalk, cycleway) %>% 
              sf::st_write(dsn = dsn, layer = "osm_multipolygons", append=FALSE)}
          
          #clean up
          rm(OSMtemp)
          # garbage collector
          gc()
          
        }, silent = T)
      } 
      
      if (grepl("build", OSMkey)) {
        
        try({
          OSMtemp <-
            bb %>% 
            df2bb() %>% 
            ## create OSM query
            osmdata::opq() %>% 
            ## add desired feature to query
            osmdata::add_osm_feature(., 
                                     key = OSMkey) %>% 
            ## download OSM data
            osmdata::osmdata_sf(.) 
          
          if (!is.null(OSMtemp$osm_points)) {
            OSMtemp$osm_points %>% 
              select(osm_id, building, amenity) %>% 
              sf::st_write(dsn = dsn, layer = "osm_points", append=FALSE)}
          
          if (!is.null(OSMtemp$osm_lines)) {
            OSMtemp$osm_lines %>% 
              select(osm_id, building, amenity) %>% 
              sf::st_write(dsn = dsn, layer = "osm_lines", append=FALSE)}
          
          if (!is.null(OSMtemp$osm_multilines)) {
            OSMtemp$osm_multilines %>% 
              select(osm_id, building, amenity) %>% 
              sf::st_write(dsn = dsn, layer = "osm_multilines", append=FALSE)}
          
          if (!is.null(OSMtemp$osm_polygons)) {
            OSMtemp$osm_polygons %>% 
              select(osm_id, building, amenity) %>% 
              sf::st_write(dsn = dsn, layer = "osm_polygons", append=FALSE)}
          
          if (!is.null(OSMtemp$osm_multipolygons)) {
            OSMtemp$osm_multipolygons %>% 
              select(osm_id, building, amenity) %>% 
              sf::st_write(dsn = dsn, layer = "osm_multipolygons", append=FALSE)}
          
          #clean up
          rm(OSMtemp)
          # garbage collector
          gc()
          
        }, silent = T)
      } 
      
      if (grepl("barrier", OSMkey)) {
        
        try({
          OSMtemp <-
            bb %>% 
            ## create OSM query
            osmdata::opq() %>% 
            ## add desired feature to query
            osmdata::add_osm_feature(., 
                                     key = OSMkey) %>% 
            ## download OSM data
            osmdata::osmdata_sf(.) 
          
          if (!is.null(OSMtemp$osm_points)) {
            OSMtemp$osm_points %>% 
              select(osm_id, barrier, access) %>% 
              sf::st_write(dsn = dsn, layer = "osm_points")}
          
          if (!is.null(OSMtemp$osm_lines)) {
            OSMtemp$osm_lines %>% 
              select(osm_id, barrier, access) %>% 
              sf::st_write(dsn = dsn, layer = "osm_lines")}
          
          if (!is.null(OSMtemp$osm_multilines)) {
            OSMtemp$osm_multilines %>% 
              select(osm_id, barrier, access) %>% 
              sf::st_write(dsn = dsn, layer = "osm_multilines")}
          
          if (!is.null(OSMtemp$osm_polygons)) {
            OSMtemp$osm_polygons %>% 
              select(osm_id, barrier, access) %>% 
              sf::st_write(dsn = dsn, layer = "osm_polygons")}
          
          if (!is.null(OSMtemp$osm_multipolygons)) {
            OSMtemp$osm_multipolygons %>% 
              select(osm_id, barrier, access) %>% 
              sf::st_write(dsn = dsn, layer = "osm_multipolygons")}
          
          #clean up
          rm(OSMtemp)
          # garbage collector
          gc()
          
        }, silent = T)
      }
      
      # check if file exists
      if (file.exists(dsn)) {
        message("Worked.")
        } else ({
        message("Download failed. Proceeding to next area.")
      })
    }
  }
}


################################################################################
