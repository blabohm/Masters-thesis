
################################################################################
#get files that have to be downloaded
check.up <- function(file_list,
                     out_dir,
                     df_out = F) {
  
  # check if there are files in out dir
  if (
    list.files(out_dir) %>% 
    purrr::is_empty()
  ) return(1:length(file_list))
  
  
  n <-
    lapply(file_list, function(in_file){
      
      f_name <-
        strsplit(in_file, "/")[[1]][4] %>% 
        substr(1,5)
      
      return(f_name)
    }) %>% 
    unlist() %>% 
    as_tibble() %>% 
    mutate(key = "x")
  
  
  m <-
    out_dir %>% 
    list.files() %>% 
    lapply(., 
           function(x) {
             strsplit(x, "\\.")[[1]][1]}) %>% 
    unlist(.) %>% 
    as_tibble() %>% 
    mutate(key = "x")
  
  df <-
    merge(n, m, by = "value", all = T) %>%
    transmute(interval = value,
              file_list = key.x,
              joined_files = key.y)
  
  
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
    ) %>% return(
      ) else bbList <-
    data.frame('cityTag' = paste0(cityTag, 
                                  c('_a', '_b', '_c', '_d')),
               'xmin' = c(xmin, xmid, xmin, xmid),    # xmin
               'xmax' = c(xmid, xmax, xmid, xmax),    # xmax
               'ymin' = c(ymid, ymid, ymin, ymin),    # ymin
               'ymax' = c(ymax, ymax, ymid, ymid)) %>%# ymax
    dplyr::bind_rows(bbList) %>% 
    return()
  
}


################################################################################
# create function to download OSM files:
dlOSM <- function(in_file, # list of gpkg files 
                  out_path  # output directory
){
  # load packages
  require(dplyr)
  
  # randomly set overpass API to use 
  api_list <- c('http://overpass-api.de/api/interpreter',
                'https://lz4.overpass-api.de/api/interpreter',
                'https://z.overpass-api.de/api/interpreter',
                'https://overpass.kumi.systems/api/interpreter')
  
  api_to_use <- sample(1:length(api_list), 1)
  
  osmdata::set_overpass_url(api_list[api_to_use]) 
  
  # input path (e.g.: 'C:/UrbanAtlas/') containing the folders with UA data 
  # named after the cities (e.g. 'AL001L1_TIRANA_013' etc.)
  # in_path <- "E:/UA2012/"
  
  # output path for the OSM data
  # out_path <- "E:/temp/"
  
  # OSM proj4 string  
  pOSM <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  pUA <-
  
  # create boundary layer
  boundary <- 
    ## load city gpkg, layer 2 for boundary
    in_file %>%
    # list.files(recursive = T,
    #            pattern = ".gpkg$",
    #            full.names = T) %>%
    # .[1] %>%
    #readOGR(layer = ogrListLayers(.)[2]) %>%
    sf::read_sf(layer = rgdal::ogrListLayers(.)[2]) %>%
    ## transform to OSM projection
    sf::st_transform(., pOSM) %>% 
    sf::as_Spatial()

    # download OSM data for boundary layer
  buildings <-
  try({
      boundary %>% 
      ## create boundary box
      sp::bbox() %>%
      ## create OSM query
      osmdata::opq() %>% 
      ## add desired feature to query
      osmdata::add_osm_feature(., 
                               key = "building") %>% 
      ## download OSM data
      osmdata::osmdata_sp() 
  })
  
  # check if download worked
  if (is.element(buildings))
  
  # select polygons from osm data
  p1 <- buildings$osm_polygons
  
  
  killLink <- 'http://overpass-api.de/api/kill_my_queries'
  shell.exec(killLink)
  
  # remove buildings object
  rm (buildings)
  gc()
  
  # convert FIDs from numeric to character
  FIDs <- 
    p1@polygons %>% 
    names() %>% 
    as.character()
  
  p1 <- 
    sp::spChFIDs(p1, FIDs)
  
  # kick out most of the columns of @data
  p1@data <-
    p1@data[1]
  
  # generate parameters for write OGR 
  ## output destination
  dsn <-
    strsplit(in_file, "/")[[1]][4] %>% 
    substr(1,5) %>% 
    paste0(out_path,
           .,
           ".gpkg")
  
  ## layer name
  lyr = "buildings"
  
  # write to file
  rgdal::writeOGR(p1,
                  dsn = dsn,
                  layer = lyr,
                  driver = "GPKG",
                  overwrite_layer = T)
  
  Sys.sleep(5)
}

################################################################################
