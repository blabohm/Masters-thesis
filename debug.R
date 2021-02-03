

# names for boundary box list
dfNames <- list(NULL, c("cityTag", "xmin", "xmax", "ymin", "ymax"))

# empty boundaty box list
bbList <- 
  matrix(ncol = 5, nrow = 1, dimnames = dfNames) %>% 
  data.frame() 

# function to extract boundary boxes of each city and fill into bbList
createBB <- function(in_file){
  
  # generate city tag
  cityTag <-
    strsplit(in_file, "/")[[1]][4] %>% 
    substr(1,5) 
  
  # OSM proj4 string    
  p <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  
  # create boundary layer
  bb <- 
    ## load UA city gpkg, layer 2 for boundary
    in_file %>%
    sf::read_sf(layer = rgdal::ogrListLayers(.)[2]) %>%
    ## transform to OSM projection
    sf::st_transform(., p) %>% 
    sf::as_Spatial() %>% 
    ## create boundary box (bbox)
    sp::bbox()
  
  # get length of bbox edges
  a <- bb[1, 2] - bb[1, 1]
  b <- bb[2, 2] - bb[2, 1]
  
  # create min, max and mid points for x and y
  xmin <- bb[1, 1]   
  xmax <- bb[1, 2]
  ymin <- bb[2, 1]
  ymax <- bb[2, 2]
  xmid <- bb[1, 1] + (a / 2)   
  ymid <- bb[2, 1] + (b / 2)   
  
  # check if bbox is too big
  if (
    a * b < 1
    # if not too big: fill bbox values into bbox list
  ) bbList <-
    data.frame(cityTag, 
               xmin, xmax, 
               ymin, ymax) %>%
    dplyr::bind_rows(bbList
    # if too big: split bbox into 4 equal parts
    ) else bbList <-
    data.frame('cityTag' = paste0(cityTag, 
                                  c('_a', '_b', '_c', '_d')),
               'xmin' = c(xmin, xmid, xmin, xmid),    # xmin
               'xmax' = c(xmid, xmax, xmid, xmax),    # xmax
               'ymin' = c(ymid, ymid, ymin, ymin),    # ymin
               'ymax' = c(ymax, ymax, ymid, ymid)) %>%# ymax
                 dplyr::bind_rows(bbList)
               
}