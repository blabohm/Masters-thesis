# function to generate a polygon drom a boundarybox
bb2poly <- function(bb) {
  
  require(dplyr)
  if (is.matrix(bb)) {
    bb <- bb2df(bb)
  }
  
  bbPoly <-
    list(rbind(c(bb$xmin, bb$ymin), 
               c(bb$xmax, bb$ymin), 
               c(bb$xmax, bb$ymax), 
               c(bb$xmin, bb$ymax),
               c(bb$xmin, bb$ymin))) %>% 
    sf::st_polygon() %>% 
    sf::st_sfc() %>% 
    sf::st_sf()
  
  return(bbPoly)
}

# check which tiles still need to be downloaded
check.up <- function(id, 
                     bbox,
                     fileDirectory) {
  
  require(dplyr)
  require(ggplot2)
  # list files starting with id
  bbDirectories <-
    list.files(fileDirectory, 
               pattern = paste0("^", id), 
               full.names = TRUE)
  
  # check if city is already downloaded completely
  if (!grepl("_1|_2|_3|_4", bbDirectories) %>% any()) {
    paste0(id, "fully downloaded.") %>% 
      message() %>% 
      return()
  }
  # create empty sf object
  bbDF <- sf::st_sf(sf::st_sfc())
  
  # create polygon of each bbox
  bbTotal <-
    bb2poly(bbox) 
  
  # create plot
  p <-
    ggplot() +
    geom_sf(data = bbTotal, 
            col = "red", fill = "red")
  p
  
  #loop over bbox Directories
  nrows <- length(bbDirectories)
  #for (i in 1:3) {
  for (i in 1:nrows) {
    
    # load files and generate bboxes  
    bbTemp <-
      bbDirectories[i] %>%
      sf::read_sf() %>%
      sf::st_bbox() %>% 
      bb2poly()
    
    # create plot
    p <-
      p + 
      geom_sf(data = bbTemp, 
              col = "green", 
              fill = "green")
    print(p)
    
    # calculate the amount of times that bbTemp X/Y fits into bbTotal X/Y
    # side lengths of bbox:
    a <- bbox[1, 2] - bbox[1, 1]
    b <- bbox[2, 2] - bbox[2, 1]
    
    bbMatrix <-
      bbTemp %>% 
      sf::st_bbox() 
    
    # side lengths of bbTemp
    a1 <- bbMatrix$xmax - bbMatrix$xmin
    b1 <- bbMatrix$ymax - bbMatrix$ymin
    
    # calculate number of tiles in X and Y direction
    X <- (a / a1) %>% round()
    Y <- (b / b1) %>% round()
    
    # return the centroid of bbTemp
    bbDF <- 
      sf::st_centroid(bbTemp) %>%
      sf::st_buffer(dist = .25 * a1) %>% 
      mutate(id = i,
             x = X,
             y = Y) %>% 
      rbind(bbDF, .)
  }
  
  # create grid and check which tiles are not covered
  bbGrid <-
    sf::st_make_grid(bbTotal, n = c(max(bbDF$x),max(bbDF$y))) %>% 
    sf::st_sf() %>% 
    filter(!sf::st_intersects(., sf::st_union(bbDF), sparse = FALSE))
  
  # if city is complete: Union all tiles, write to single file and save rest to
  # backup directory
  if (nrow(bbGrid) == 0) {
    
    # user communication
    paste0(id, " fully downloaded. Writing results to ", 
           fileDirectory, id, ".gpkg") %>% 
      message()
    
    # union files
    lapply(bbDirectories, sf::st_read) %>%   
      do.call(what = rbind, args = .) %>% 
      sf::st_write(., dsn = paste0(fileDirectory, id, ".gpkg"))
    
    # copy to backup directory and removing files
    file.copy(bbDirectories, paste0(fileDirectory, "backup"))
    file.remove(bbDirectories)
    
  } else {
    # gather bboxes in df and pass to download
    outDF <- 
      tibble()
    
    for (i in 1:nrow(bbGrid)) {
      
      outDF <-
        bbGrid$geometry[i] %>% 
        sf::st_bbox() %>% 
        matrix(nrow = 2) %>% 
        bb2df() %>% 
        mutate(code = paste0(id, "_missing_", i)) %>% 
        bind_rows(outDF, .)
      
    }  
  }
  return(outDF)
}
