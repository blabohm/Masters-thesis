in_vector <- bbList[to_do[1],]
OSMkey = "building"
OSMkey = "way"

################################################################################
# create function to download OSM files:
dlOSM <- function(in_vector, # list of gpkg files 
                  out_path,  # output directory
                  OSMkey
){
  # load packages
  require(dplyr)
  
  # set overpass API to use
  interpreter <- APIselect(api_list = api_list)
  osmdata::set_overpass_url(interpreter) 
  
  # create boundary box (bbox)
  bb <- 
    df2bb(in_vector) %>% 
    round(digits = 5)
  
  ## output destination
  dsn <-
    paste0(out_path,
           in_vector$cityTag,
           ".gpkg")
  
  # message
  message(paste("Commencing download of: ", in_vector$cityTag,
                "at: ", interpreter))
  
  # download OSM data for boundary layer
  try({    
    bb %>% 
      ## create OSM query
      osmdata::opq() %>% 
      ## add desired feature to query
      osmdata::add_osm_feature(., 
                               key = OSMkey) %>% 
      ## download OSM data
      osmdata::osmdata_sf(.) %>%
      .$osm_polygons %>% 
      select(osm_id, building, amenity) %>% 
      sf::st_write(dsn = dsn, layer = OSMkey)
    
    # garbage collector
    gc()
    
  }, silent = T)
  
   # write to file
  if (file.exists(dsn)){
    message("Download successful.")
  } else ({
    message("Download failed. Proceeding to next area.")
  })
}

# select polygons from osm data
# bPolygons <- buildings$osm_polygons
# 
# 
# # convert FIDs from numeric to character
# FIDs <- 
#   bPolygons@polygons %>% 
#   names() %>% 
#   as.character()
# 
# bPolygons <- 
#   sp::spChFIDs(bPolygons, FIDs)
# ################################################################################
# # kick out most of the columns of @data
# bPolygons@data <-
#   data.frame(
#     'osm_id' = bPolygons@data[['osm_id']],
#     'building' = bPolygons@data[['building']],
#     'amenity' = bPolygons@data[["amenity"]])
# 
# rgdal::writeOGR(bPolygons,
#                 dsn = dsn,
#                 layer = OSMkey,
#                 driver = "GPKG",
#                 overwrite_layer = T)