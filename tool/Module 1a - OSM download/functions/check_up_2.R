################################################################################
# set crs to wgs84
set.WGS <- function(x){
  wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  st_crs(x) <- wgs84 
  x
}


################################################################################
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

################################################################################
# add geometry column to bbox data frame
bb.geomcol <- function(x) {
  
  bbGridTemp <- 
    sf::st_sf(sf::st_sfc())
  
  for (i in 1:nrow(x)) {
    
    bbGridTemp <-
      x[i,] %>% 
      bb2poly() %>% 
      rbind(bbGridTemp, .)
    
  }
  
  bbGridTemp <-
    cbind(bbGridTemp, x) 
  
  bbGridTemp
} 

################################################################################
# for splitting the bboxes of an entire data-frame with columns:
# cityTag, xmin, xmax, ymin, ymax
splitBBdf2 <- function(bb, ...){
  
  splitBBdf2temp <- 
    tibble()
  
  for (x in 1:length(bb)) {
    
    bbTemp <- 
      bb[x] %>% 
      sf::st_bbox() %>% 
      matrix(nrow = 2) %>% 
      bb2df()
    
    splitBBdf2temp <-
      splitBB(bbTemp) %>% 
      bind_rows(splitBBdf2temp, .)
  }
  return(splitBBdf2temp)
}

################################################################################
# check which tiles still need to be downloaded
check.up2 <- function(id, 
                      bbox,
                      fileDirectory,
                      boundaryDirectory) {
  
  require(dplyr)
  require(ggplot2)
  require(sf)
  # list files starting with id
  bbDirectories <-
    list.files(fileDirectory, 
               pattern = paste0("^", id, "*.*.gpkg$"), 
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
    
    # Try to load downloaded files and generate bboxes 
    bbTemp <-
      tryCatch({
        bbDirectories[i] %>%
          sf::read_sf() %>%
          sf::st_bbox() %>% 
          bb2poly()
      }, error = function(cond) {
        message(paste("File does not seem to exist or is broken:", 
                      bbDirectories[i]))
        message("Here's the original error message:")
        message(cond)
        message("Trying to delete file.")
        try(unlink(bbDirectories[i]))
        return(NULL)
      })
    
    if (is.null(bbTemp)) {
      message("Proceeding...") %>% 
        return()
    } else {
    # create plot
    p <-
      p + 
      geom_sf(data = bbTemp, 
              col = "green", 
              fill = "green")
    print(p)
    
    # calculate the amount of times that bbTemp X/Y fits into bbTotal X/Y
    # side lengths of bbox:
    a <- bbox$xmax - bbox$xmin
    b <- bbox$ymax - bbox$ymin
    
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
      bbTemp %>%
      sf::st_buffer(dist = -.1 * a1) %>% 
      mutate(id = i,
             x = X,
             y = Y) %>% 
      rbind(bbDF, .)
    }
  }
  
  # load boundary layer to check if tiles are inside city boundaries
  wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  boundaryTemp <-
    boundaryDirectory %>% 
    st_read(layer = rgdal::ogrListLayers(.)[2], 
            quiet = TRUE) %>% 
    # MUST BE TRANSFORMED!
    sf::st_transform(., wgs84)
  
  # create grid and check which tiles are not covered
  bbGrid <-
    tryCatch({
    sf::st_make_grid(bbTotal, n = c(max(bbDF$x),max(bbDF$y))) %>% 
    sf::st_sf() %>% 
    filter(!sf::st_intersects(., sf::st_union(bbDF), sparse = FALSE)) %>%   
    splitBBdf2(bb = .$geometry) %>% 
    bb.geomcol() %>% 
    set.WGS() %>% 
    # check if splitted bb are inside city boundary
    filter(st_intersects(., boundaryTemp, sparse = FALSE))
  }, error = function(cond) {
    message("Checking for not covered tiles yielded following error:")
    message(cond)
    return(NULL)
  })
  # if city is complete: Union all tiles, write to single file and save rest to
  # backup directory
  if (is.null(bbGrid)) {
    
    # user communication
    paste0(id, " fully downloaded.") %>% 
      message()
    
    # # union files
    # lapply(bbDirectories, sf::st_read) %>%   
    #   do.call(what = rbind, args = .) %>% 
    #   sf::st_write(., dsn = paste0(fileDirectory, id, ".gpkg"))
    # 
    # # copy to backup directory and removing files
    # file.copy(bbDirectories, paste0(fileDirectory, "backup"))
    # file.remove(bbDirectories)
    
    return(NULL)
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

################################################################################

bboxFromUA <-
  function(x) {
    
    x %>% 
      sf::st_read(layer = rgdal::ogrListLayers(.)[2], 
                  quiet = TRUE) %>% 
      sf::st_transform(wgs84) %>% 
      sf::st_bbox() %>% 
      matrix(nrow = 2) %>% 
      bb2df() %>% 
      return()
    
  }
