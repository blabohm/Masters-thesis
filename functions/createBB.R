in_file <- in_list$value[1]
bboxList <- bbList


createBB <- function(in_file, bboxList, tSize = .25
){
  require(dplyr)
  
  
  # generate city tag
  cityTagString <-
    in_file %>%
    get_code()
  
  # OSM proj4 string    
  p <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  
  # create boundary box layer
  bb <- 
    # supress warnings of discarded WGS_1984 argument from input
    suppressWarnings({
      
      ## load UA city gpkg, layer 2 for boundary
      in_file %>%
        sf::read_sf(layer = rgdal::ogrListLayers(.)[2]) %>%
        ## transform to OSM projection
        sf::st_transform(., p) %>% 
        sf::as_Spatial(.) %>% 
        ## create boundary box (bbox)
        sp::bbox(.)
    })
  
  # check if bbox is too big
  if (bbSize(bb) < tSize) {
    # if not too big: fill bbox values into bbox list
    bboxList <-
      bb %>% 
      bb2df() %>% 
      data.frame("cityTag" = cityTagString, 
                 .) %>%
      dplyr::bind_rows(bboxList, .) %>% 
      return()
    
  } else {
    bbTemp <- 
      bb2df(bb) %>% 
      mutate(cityTag = cityTagString)
    
    while (bbSize(bbTemp[1,]) > tSize) {
      
      for (i in 1:nrow(bbTemp)) {
        bbTemp <-
          bbTemp[i,] %>% 
          splitBB() %>% 
          dplyr::bind_rows(bbTemp, .)
      }
      
      bbTemp <- 
        bbTemp %>% 
        filter(is.na(cityTag)) %>% 
        mutate(cityTag = paste0(cityTagString, "_", row_number()))
    }
    bboxList <-
      bbTemp %>% 
      bind_rows(bboxList, .) %>% 
      return()
  } 
}
