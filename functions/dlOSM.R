in_vector <- bbList[to_do[24],]
OSMkey = "building"

dlOSM <- function(in_vector, # list of gpkg files 
                  out_path,  # output directory
                  OSMkey,
                  # list of API links
                  api_list = dplyr::tibble(interpreter = 
                                      c('http://overpass-api.de/api/interpreter',
                                        'https://lz4.overpass-api.de/api/interpreter',
                                        'https://z.overpass-api.de/api/interpreter'#,
                                        #'https://overpass.kumi.systems/api/interpreter'
                                      ))
){
  # load packages
  require(dplyr)
  
#  while (!exists('OSMdownload')){
  # set overpass API url to use
  interpreter <- APIselect(api_list = api_list)
  
  osmdata::set_overpass_url(interpreter) 
  
  
  # create boundary box (bbox)
  bb <- 
    df2bb(in_vector) %>% 
    round(digits = 5)
  
  # message
  message(paste("Commencing download of: ", in_vector$cityTag,
                "\n at: ", interpreter))
  # download OSM data for boundary layer
  try({    
    OSMdownload <-
      bb %>% 
      ## create OSM query
      osmdata::opq() %>% 
      ## add desired feature to query
      osmdata::add_osm_feature(., 
                               key = OSMkey) %>% #<<<<<<<<--------------
    ## download OSM data
    #osmdata::osmdata_xml(filename = "E:/temp_test/test1.xml")
    osmdata::osmdata_sf(.) 
  }, silent = T)
 # }
  
  # # check if download worked
  if (
    !exists('buildings')
  ) return(
    {message(
      paste0("\n Download failed at: ",
             interpreter))

      Sys.sleep(360)
    }
  ) else try({
  

    ################################################################################
    # generate parameters for write OGR 
    ## output destination
    dsn <-
      paste0(out_path,
             in_vector$cityTag,
             ".gpkg")

    # write to file
    OSMdownload$osm_polygons %>% 
      select(osm_id, building, amenity) %>% 
      sf::st_write(dsn = dsn, layer = OSMkey)
    
    # remove buildings object
    rm(OSMdownload)
    gc()})
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