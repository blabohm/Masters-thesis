for (i in in_list$value) {
  
  cT <- get_code(i)
  
  bb1 <- 
    # supress warnings of discarded WGS_1984 argument from input
    suppressWarnings({
      
      ## load UA city gpkg, layer 2 for boundary
      i %>%
        sf::read_sf(layer = rgdal::ogrListLayers(.)[2]) %>%
        ## transform to OSM projection
        sf::st_transform(., p) %>% 
        sf::as_Spatial(.) %>% 
        ## create boundary box (bbox)
        sp::bbox(.)
      
    })
  
  s <- bbSize(bb1) 
  
  print(paste(cT, s))
}

for (i in to_do) {
  print(bbList[i,])
}

test <-
interpreter %>% 
  sub('interpreter', 'status', .) %>% 
  httr::GET() 

test %>% 
  httr::content() %>% 
  strsplit("\n") %>% 
  unlist() %>% 
  grep("available", ., 
       value = TRUE) %>% 
  substr(1,1) %>% 
  as.numeric()



in_list$value[2] %>% 
  sf::read_sf() %>% 
  osmdata::getbb()

