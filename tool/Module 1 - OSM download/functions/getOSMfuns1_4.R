################################################################################
# GENERAL TOOLS
################################################################################
# convert a boundary box to a data frame
bb2df <- 
  function(BBox){
    data.frame(
      "xmin" = BBox[1, 1],   
      "xmax" = BBox[1, 2],
      "ymin" = BBox[2, 1],
      "ymax" = BBox[2, 2])
  }


################################################################################
# convert a data frame to a boundary box
df2bb <- 
  function(BBox_df){
    matrix(BBox_df %>%
             select(xmin:ymax) %>% 
             unlist(), 
           nrow = 2, ncol = 2, byrow = T,
           dimnames = list(c('x', 'y'), c('min', 'max'))
    )}


################################################################################
# get size of a boundary box
bbSize <-
  function(BBox){
    if (dim(BBox) %>% .[1] == 1) {
      BBox <-
        BBox %>% 
        select(xmin:ymax) %>% 
        df2bb()
    }
    
    # get length of bbox edges
    a <- BBox[1, 2] - BBox[1, 1]
    b <- BBox[2, 2] - BBox[2, 1]
    # calculate area
    A <- a * b
    return(A)
  }

################################################################################

splitBB <- 
  function(BBox){
    
    if (dim(BBox) %>% first() == 1) {
      BBox <-
        BBox %>% 
        select(xmin:ymax) %>% 
        df2bb()
    }
    # get length of bbox edges
    a <- BBox[1, 2] - BBox[1, 1]
    b <- BBox[2, 2] - BBox[2, 1]
    
    # create min, max and mid points for x and y
    xmin <- BBox[1, 1]   
    xmax <- BBox[1, 2]
    ymin <- BBox[2, 1]
    ymax <- BBox[2, 2]
    xmid <- BBox[1, 1] + (a / 2)   
    ymid <- BBox[2, 1] + (b / 2)   
    
    
    # if too big: split bbox into 4 equal parts
    
    data.frame('xmin' = c(xmin, xmid, xmin, xmid),    # xmin
               'xmax' = c(xmid, xmax, xmid, xmax),    # xmax
               'ymin' = c(ymid, ymid, ymin, ymin),    # ymin
               'ymax' = c(ymax, ymax, ymid, ymid)) %>%# ymax
      return()
  }


################################################################################
# for splitting the bboxes of an entire data-frame with columns:
# cityTag, xmin, xmax, ymin, ymax
splitBBdf <- function(bb){
  
  bbTempDf <- tibble()
  
  for (x in 1:nrow(bb)) {
    
    bbTemp <- bb[x, ]
    
    cityTagString <- bbTemp$cityTag
    
    bbTemp <-
      bbTemp %>% 
      splitBB() %>% 
      dplyr::bind_rows(bbTemp, .)
    
    bbTemp <- 
      bbTemp %>% 
      filter(is.na(cityTag)) %>% 
      mutate(cityTag = paste0(cityTagString, "_", row_number()))
    
    bbTempDf <-
      bbTemp %>% 
      bind_rows(bbTempDf, .) 
  }
  return(bbTempDf)
}
################################################################################
# get the 5 character city code from a directory
get_code <-
  function(dir_string){
    dir_string %>% 
      strsplit("/") %>% 
      unlist() %>% 
      grep("[[:alpha:]]{2}\\d{3}\\D", ., value = TRUE) %>% 
      .[1] %>% 
      substr(1, 5)
  }


# for to-do function
get_code0 <-
  function(dir_string){
    dir_string %>% 
      strsplit("/") %>% 
      unlist() %>% 
      grep("[[:alpha:]]{2}\\d{3}\\D", ., value = TRUE) %>% 
      gsub("\\.gpkg|\\.txt", "", .)
  }


################################################################################
# apply get_code function to a list of directories
namify <- 
  function(dir_list) {
    lapply(dir_list, get_code) %>% 
      unlist()
  }


# for to-do function
namify0 <- 
  function(dir_list) {
    lapply(dir_list, get_code0) %>% 
      unlist()
  }


################################################################################
# get number of free slots from an overpass API
getFreeSlots <- 
  function(APIlink, N) {
    try({
      nSlots <-
        APIlink %>% 
        sub('interpreter', 'status', .) %>% 
        httr::GET() %>% 
        httr::content() %>% 
        strsplit("\n") %>% 
        unlist() %>% 
        grep("available", ., 
             value = TRUE) %>% 
        substr(1,1) %>% 
        as.numeric() %>% 
        .[1]
    })
    if (exists("nSlots")) {
      if (length(nSlots == N)) {
      return(nSlots)
        } else {
        return(0)
      }
    } else {
      return(0)
    }
  }

# select API with most free slots or wait if no slots available
APIselect <- function(api_list) {
  
  N <- nrow(api_list)
  
  api_list <- 
    api_list %>% 
    mutate(slots = 
             lapply(interpreter, getFreeSlots, N) %>% 
             unlist()) %>% 
    arrange(desc(slots))
  
  while (sum(api_list$slots, na.rm = T) < 1) {
    
    message("Waiting for API slot.")
    Sys.sleep(30)
    
    api_list <- 
      api_list %>% 
      mutate(slots = 
               lapply(interpreter, getFreeSlots, N) %>% 
               unlist()) %>% 
      arrange(desc(slots))
  }
  
  return(api_list$interpreter[1])
}



# get status of overpass API link

# APIstatus <- 
#   function(APIlink) {
#     require(dplyr, quietly = T)
#     status <-
#       APIlink %>% 
#       sub('interpreter', 'status', .) %>% 
#       httr::GET() %>% 
#       httr::http_status(.) %>%
#       .$category
#     
#     return(status)
#   }
# 
# 
# ################################################################################
# # select a working API
# getAPI <-
#   function(url_list, waitTime) {
#     # set n to 1 to check first position in API list
#     n <- 1
#     # retrieve first API link
#     Link <- url_list[n]
#     # check status
#     status_check <- APIstatus(Link)
#     
#     # keep checking status of all API links until one is not busy anymore
#     while (status_check != "Success") {
#       
#       # kill open queries
#       killLink <- 
#         sub('interpreter', 
#             'kill_my_queries', 
#             Link)
#       
#       httr::GET(killLink)
#       
#       # check next API link
#       n <- n + 1
#       # reset n to one if last element was reached and wait
#       if (n >= length(url_list) + 1) {
#         n <- 1
#         status_check <- "Failure"
#         
#         Sys.sleep(waitTime)
#         
#       } else {
#         
#         Link <- url_list[n]
#         
#         status_check <- APIstatus(Link)
#       }
#     }
#     # return working API link
#     return(Link)
#     
#   }


################################################################################
#get files that have to be downloaded (via file name)
check.up <- function(file_list,
                     out_dir,
                     df_out = F) {
  
  # check if there are files in out dir
  if (
    list.files(out_dir) %>% 
    purrr::is_empty(.)
  ) return(1:length(file_list))
  
  n <-
    file_list %>% 
    #    namify() %>% 
    as_tibble() %>% 
    mutate(key = "x",
           priority = row_number())
  
  m <-
    out_dir %>% 
    list.files() %>% 
    namify0() %>% 
    as_tibble() %>% 
    mutate(key = "x")
  
  df <-
    merge(n, m, by = "value", 
          all.x = TRUE) %>%
    distinct() %>% 
    transmute(interval = value,
              file_list = key.x,
              joined_files = key.y,
              priority = priority) %>% 
    arrange(priority) %>% 
    select(-priority)
  
  
  if (
    df_out == T
  ) return(
    df
  ) else if (
    anyNA(df$joined_files)
  ) return(
    which(df$joined_files %>% is.na())
  ) else message(
    "All fine! :)"
  )
  
}

################################################################################
# FUNCTIONS NEEDED TO CHECK WHICH TILES STILL HAVE TO BE DOWNLOADED
################################################################################
# set crs to wgs84
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"


set.WGS <- function(x){
  wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  sf::st_crs(x) <- wgs84 
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
# CREATE BOUNDARY BOXES AND DOWNLOAD OSM DATA
################################################################################
# function to extract boundary boxes of each city and fill into bbList

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

################################################################################
################################################################################
# create function to download OSM files:
dlOSM <- function(in_vector, # list of gpkg files 
                  out_path,  # output directory
                  OSMkey
){
  # load packages
  require(dplyr)
  
  # set overpass API to use
  interpreter <- APIselect(api_list = dplyr::tibble(interpreter = 
                                                      c('http://overpass-api.de/api/interpreter',
                                                        'https://lz4.overpass-api.de/api/interpreter',
                                                        'https://z.overpass-api.de/api/interpreter'#,
                                                        #'https://overpass.kumi.systems/api/interpreter'
                                                      )))
  osmdata::set_overpass_url(interpreter) 
  
  # create boundary box (bbox)
  bb <- 
    df2bb(in_vector) 
  
  ## output destination
  dsn <-
    paste0(out_path,
           in_vector$cityTag,
           ".gpkg")
  
  # message
  message(paste("Commencing download of: ", in_vector$cityTag,
                "at: ", interpreter))
  
  OSMdownloader(bb, OSMkey, dsn)
  
  # check if file exists
  if (file.exists(dsn)) {
    
    message("Proceeding...")
    
  } else {
    
    message("Nothing to write.")
    
  }
}

################################################################################

# column selector function for HIGHWAY layers
includeHighway <- function(OSMsf) {
  
  require(dplyr)
  
  OSMsf %>% 
    select(contains("id"), 
           contains("way"), 
           contains("foot"), 
           contains("side"), 
           contains("cycle")) %>% 
    return()
  
}

# column selector function for BUILDING layers
includeBuilding <- function(OSMsf) {
  
  require(dplyr)
  
  OSMsf %>% 
    select(contains("id"), 
           contains("build"), 
           contains("amenity"), 
           contains("value")) %>% 
    return()
  
}

# column selector function for BARRIER layers
includeBarriers <- function(OSMsf) {
  
  require(dplyr)
  
  OSMsf %>% 
    select(contains("id"), 
           contains("barrier"), 
           contains("access")) %>% 
    return()
  
}

# function to exclude problematic columns
exclude <- function(OSMsf) {
  
  require(dplyr)
  
  OSMsf %>% 
    select(- contains("fid"),- 
             contains("wiki"), 
           - contains("object"), 
           - matches("[.]")) %>% 
    return()
}

################################################################################
# OSM Downloader function to be called in the dlOSM function
OSMdownloader <- function(bb, key, dsn) {
  # download OSM data for boundary layer
  OSMtemp <-
    tryCatch({
      bb %>% 
        ## create OSM query
        osmdata::opq() %>% 
        ## add desired feature to query
        osmdata::add_osm_feature(., 
                                 key = key) %>% 
        ## download OSM data
        osmdata::osmdata_sf(.) 
    }, error = function(e) {
      message("Download failed. Original error message:")
      message(e)
      return(NULL)
    })
  
  if (!is.null(OSMtemp)) tryCatch({
    
    message("Download successful.")
    
    # check for osm key and filter for columns (not all OSM columns work for
    # st_write)
    if (grepl("way", key)) {
      
      #if (!is.null(OSMtemp$osm_points)) {
      #  OSMtemp$osm_points %>% 
      #    includeBarriers() %>% 
      #    exclude() %>% 
      #    sf::st_write(dsn = dsn, layer = "osm_points", 
      #                 append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_lines)) {
        OSMtemp$osm_lines %>% 
          includeHighway() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_lines", 
                       append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_multilines)) {
        OSMtemp$osm_multilines %>% 
          includeHighway() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_multilines", 
                       append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_polygons)) {
        OSMtemp$osm_polygons %>% 
          includeHighway() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_polygons",
                       append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_multipolygons)) {
        OSMtemp$osm_multipolygons %>% 
          includeHighway() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_multipolygons",
                       append = FALSE, quiet = TRUE)}
      
      #clean up
      rm(OSMtemp)
      # garbage collector
      gc()
      
    }
    
    if (grepl("build", key)) {
      
      # points only seem to give nodes of building polygons
      #if (!is.null(OSMtemp$osm_points)) {
      #  OSMtemp$osm_points %>% 
      #   includeBuilding() %>% 
      #   exclude() %>%      
      #   sf::st_write(dsn = dsn, layer = "osm_points", append=FALSE)}
      
      if (!is.null(OSMtemp$osm_lines)) {
        OSMtemp$osm_lines %>% 
          includeBuilding() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_lines",
                       append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_multilines)) {
        OSMtemp$osm_multilines %>% 
          includeBuilding() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_multilines",
                       append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_polygons)) {
        OSMtemp$osm_polygons %>% 
          includeBuilding() %>% 
          exclude() %>%  
          sf::st_write(dsn = dsn, layer = "osm_polygons",
                       append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_multipolygons)) {
        OSMtemp$osm_multipolygons %>% 
          includeBuilding() %>% 
          exclude() %>%  
          sf::st_write(dsn = dsn, layer = "osm_multipolygons",
                       append = FALSE, quiet = TRUE)}
      
      #clean up
      rm(OSMtemp)
      # garbage collector
      gc()
      
    }   
    
    if (grepl("barrier", key)) {
      
      if (!is.null(OSMtemp$osm_points)) {
        OSMtemp$osm_points %>%          
          includeBarriers() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_points", 
                       append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_lines)) {
        OSMtemp$osm_lines %>% 
          includeBarriers() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_lines",
                       append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_multilines)) {
        OSMtemp$osm_multilines %>% 
          includeBarriers() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_multilines",
                       append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_polygons)) {
        OSMtemp$osm_polygons %>% 
          includeBarriers() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_polygons",
                       append = FALSE, quiet = TRUE)}
      
      if (!is.null(OSMtemp$osm_multipolygons)) {
        OSMtemp$osm_multipolygons %>% 
          includeBarriers() %>% 
          exclude() %>% 
          sf::st_write(dsn = dsn, layer = "osm_multipolygons",
                       append = FALSE, quiet = TRUE)}
      
      #clean up
      rm(OSMtemp)
      # garbage collector
      gc()
      
    }
  }, error = function(e){
    
    message(e)
    message("..")
    
    if (file.exists(dsn)) {
      message("Something went wrong during file creation.")
      message("Deleting output.")
      unlink(dsn, force = TRUE)
    }
    
    if (!file.exists(dsn)) {
      message("Area does not seem to contain desired data.")
      message("Writing placeholder file.")
      
      dsn <- gsub("gpkg", "txt", dsn)
      write(bb, dsn, append = FALSE)}
    
  })
}
################################################################################