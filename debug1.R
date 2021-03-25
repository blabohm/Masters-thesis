testFile <- 
  in_file %>%
  sf::read_sf(layer = rgdal::ogrListLayers(.)[2]) %>%
  ## transform to OSM projection
  sf::st_transform(., p) #%>%

testBB <- 
  testFile %>%
  sf::st_bbox()

createBB(in_file)


bb1 <- 
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
################################################################################


APIselect <- function(api_list) {
  
  api_list <- 
    api_list %>% 
    mutate(slots = 
             lapply(interpreter, getFreeSlots) %>% 
             unlist()) %>% 
    arrange(desc(slots))
  
  while (sum(api_list$slots) == 0) {
    Sys.sleep(180)
    
    api_list <- 
      api_list %>% 
      mutate(slots = 
               lapply(interpreter, getFreeSlots) %>% 
               unlist()) %>% 
      arrange(desc(slots))
  }
  
  return(api_list$interpreter[1])
}

getFreeSlots <- 
  function(APIlink) {
    APIlink %>% 
      sub('interpreter', 'status', .) %>% 
      httr::GET() %>% 
      httr::content() %>% 
      strsplit("\n") %>% 
      unlist() %>% 
      grep("available", ., 
           value = TRUE) %>% 
      substr(1,1) %>% 
      as.numeric()
  }
################################################################################
lapply(api_list, getFreeSlots)


    interpreter %>% 
    sub('interpreter', 'status', .) %>% 
    httr::GET() 
  
  api_list$slot <-
    test %>% 
    httr::content() %>% 
    strsplit("\n") %>% 
    unlist() %>% 
    grep("available", ., 
         value = TRUE) %>% 
    substr(1,1) %>% 
    as.numeric()
  
