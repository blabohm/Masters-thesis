in_file <- in_list$value[5]
bboxList <- bbList


createBB <- function(in_file, bboxList, tSize = .8){
  require(dplyr)
  # suppressWarnings({
  
  
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
    # if too big: split bbox into 4 equal parts
    bbSplitDf <-
      bb %>% 
      splitBB() %>% 
      data.frame("cityTag" =  paste0(cityTagString, 
                                     c('_a', '_b', '_c', '_d')),
                 .)
    
    # now check if the resulting bbox is still too big
    if (bbSize(bbSplitDf[1,]) < tSize) {
      # if size is ok, bind to bblist
      bboxList <-
        bbSplitDf %>% 
        dplyr::bind_rows(bboxList, .) %>% 
        return()
    } else {
  
      bbListTemp <- 
        matrix(ncol = 5, nrow = 0, dimnames = dfNames) %>% 
        data.frame() 
      
      #if size is too big, split again, then bind to bblist
      for (i in 1:nrow(bbSplitDf)) {
        bbListTemp <-
          bbSplitDf[i,] %>% 
          splitBB() %>% 
          dplyr::bind_rows(bbListTemp, .)
      }
      
      bboxList <-
        bbListTemp %>% 
        mutate(cityTag = paste0(cityTagString, 
                                c(rep('_a', 4), rep('_b', 4), 
                                  rep('_c', 4),rep('_d', 4)), 
                                rep(1:4, 4))) %>% 
        dplyr::bind_rows(bboxList, .) %>% 
        return()  
    }
  }
  #})
}

