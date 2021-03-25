
################################################################################

# create variables for directories
set.up <-
  function(base_directory, # directory containing UA .zip files
           target_layer,
           out_name,
           year = c("2006", "2012", "2018",
                    "2006-2012", "2012-2018") # years of interest
  ) {
    
    # names for set up data frame
    dfNames <- c("indir", "tmpdir_glob", "outdir", "year", "layer", "dir")
    
    # generate empty data frame
    setUPdf <- 
      tibble()
    
    # determine layer:
    if (target_layer == "land-use") lyr <- 1
    if (target_layer == "boundary") lyr <- 2
    if (target_layer == "core") lyr <- 3
    
    # fill data frame with directories
    for (i in year) {
      
      # UA or UAC (UA_change)?
      if (
        nchar(i) < 8
      ) u <- "UA" else u <- "UAC"
      
      # Input directory containing UA .zip files 
      indir <- 
        paste0(base_directory, u, i, "/")
      
      # Temp directory used for unzipping the UA-files
      tmpdir <- 
        paste0(base_directory,
               "temp/")
      if (
        !dir.exists(tmpdir)
      ) dir.create(tmpdir)
      
      # Directory for storing the results
      outdir <-
        paste0(base_directory,
               "Results/")
      if (
        !dir.exists(outdir)
      ) dir.create(outdir)
      
      # output destination
      dsn <- paste0(outdir, u, i, "_", out_name)
      
      # add directories to data frame
      setUPdf <- 
        matrix(c(indir, tmpdir, outdir, i, lyr, dsn), 
               nrow = 1, ncol = length(dfNames),
               dimnames = list(NULL, dfNames)
               ) %>% 
        as.data.frame() %>% 
        bind_rows(setUPdf, .)
    }
    return(setUPdf)
  }

################################################################################

# empty temp directory
clean.up <-
  function(directory){
    require(dplyr)
    directory %>% 
      unlink(recursive = T)
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

################################################################################
# apply get_code function to a list of directories
namify <- 
  function(dir_list) {
    lapply(dir_list, get_code) %>% 
      unlist()
  }

################################################################################

combineSHP <- function(input_directory,
                       output_directory,
                       layer,
                       year) {
  
  z_list <-
    input_directory %>% 
    list.files(full.names = T) %>% 
    as_tibble() %>% 
    filter(!grepl(".zip$", .$value)) %>% 
    mutate(uaname = namify(value)) %>% 
    filter(uaname %in% UA_2006_comp$code)
  
  if (layer == 1) {
    z_list <-
      z_list %>% 
      mutate(city_files = value %>%
               list.files(recursive = T,
                          pattern = ".shp$",
                          full.names = TRUE) %>%
               tibble() %>% 
               filter(!grepl("oundary",.)) %>% 
               .$.)
    
    for (shp in z_list$city_files) {
      #require dplyr for pipes
      require(dplyr)
      
      #get name of desired layer
      lyr <- namify(shp)
      
      sf::read_sf(shp) %>% 
        select(city_code = contains("FUA_OR"),
               class = contains("CODE20"),
               Population = contains("Pop")) %>% 
        sf::write_sf(obj = .,
                     dsn = output_directory, 
                     layer = lyr,
                     append = TRUE)
    }
    
  } else {
    z_list <-
      z_list %>% 
      mutate(city_files = value %>%
               list.files(recursive = T,
                          pattern = ".shp$",
                          full.names = TRUE) %>%
               tibble() %>% 
               filter(grepl("oundary",.)) %>% 
               .$.)
    
    for (shp in z_list$city_files) {
      #require dplyr for pipes
      require(dplyr)
      
      #get name of desired layer
      lyr <- namify(shp)
      
      sf::read_sf(shp) %>% 
        sf::write_sf(obj = .,
                     dsn = output_directory, 
                     layer = lyr,
                     append = TRUE)
    }
  }
}

################################################################################
combineGPKG <- function(input_directory,
                       output_directory,
                       layer,
                       year) {
  
  z_list <-
    input_directory %>% 
    list.files(full.names = T) %>% 
    as_tibble() %>% 
    filter(!grepl(".zip$", .$value)) %>% 
    mutate(uaname = namify(value)) %>% 
    filter(uaname %in% UA_2006_comp$code) %>% 
    mutate(city_files = value %>% 
             list.files(recursive = T,
                        pattern = ".gpkg$",
                        full.names = TRUE))
  
  if (layer > 1) {
    
    for (gpkg in z_list$city_files) {
      #require dplyr for pipes
      require(dplyr)
      
      #get name of desired layer
      lyr <- sf::st_layers(gpkg)$name[layer]
      
      sf::read_sf(gpkg, layer = lyr) %>% 
        sf::write_sf(obj = .,
                     dsn = output_directory, 
                     layer = lyr,
                     append = TRUE)
    }
  } else {
    
  for (gpkg in z_list$city_files) {
    #require dplyr for pipes
    require(dplyr)
    
    #get name of desired layer
    lyr <- sf::st_layers(gpkg)$name[layer]

    sf::read_sf(gpkg, layer = lyr
    ) %>% 
      select(city_code = contains("FUA_OR") | contains("fua_code"),
             class = contains("code") && contains("20"),
             Population = contains("Pop.")) %>% 
      sf::write_sf(obj = .,
                   dsn = output_directory, 
                   layer = lyr,
                   append = TRUE)
  }
  }
}