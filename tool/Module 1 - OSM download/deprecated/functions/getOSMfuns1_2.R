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
splitBBdf <- function(bb, bboxList, tSize){
  
  for (x in 1:nrow(bb)) {
    
    bbTemp <- bb[x, ]
    
    cityTagString <- bbTemp$cityTag
    
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
      bind_rows(bboxList, .) 
  }
  return(bboxList)
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
      gsub("\\.gpkg", "", .)
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
  function(APIlink) {
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
      if (length(nSlots == nrow(api_list))) {
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
  
  api_list <- 
    api_list %>% 
    mutate(slots = 
             lapply(interpreter, getFreeSlots) %>% 
             unlist()) %>% 
    arrange(desc(slots))
  
  while (sum(api_list$slots, na.rm = T) < 1) {
    
    message("Waiting for API slot.")
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
#get files that have to be downloaded
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
  # check if download is polygons or lines
  if (grepl("way", OSMkey)) {
  
  try({    
    bb %>% 
      ## create OSM query
      osmdata::opq() %>% 
      ## add desired feature to query
      osmdata::add_osm_feature(., 
                               key = OSMkey) %>% 
      ## download OSM data
      osmdata::osmdata_sf(.)  %>%
      .$osm_lines %>% 
      select(osm_id, building, amenity) %>% 
      sf::st_write(dsn = dsn, layer = OSMkey)
    
    # garbage collector
    gc()
    
  }, silent = T)
    } else {
  
  try({    
    bb %>% 
      ## create OSM query
      osmdata::opq() %>% 
      ## add desired feature to query
      osmdata::add_osm_feature(., 
                               key = OSMkey) %>% 
      ## download OSM data
      osmdata::osmdata_sf(.)  %>%
      .$osm_polygons %>% 
      select(osm_id, building, amenity) %>% 
      sf::st_write(dsn = dsn, layer = OSMkey)
    
    # garbage collector
    gc()
    
  }, silent = T)
      }
  
  # check if file exists
  if (file.exists(dsn)){
    message("Download successful.")
  } else ({
    message("Download failed. Proceeding to next area.")
  })
}


################################################################################
################################################################################
################################################################################