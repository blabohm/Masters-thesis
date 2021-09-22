id <- in_list$code[1]
fileDirectory <- out_path

createOutput <- function(id, fileDirectory) { 
  
  idDirectories <-
    list.files(fileDirectory, 
               pattern = paste0("^", id), 
               full.names = TRUE)
  
  tileDirectories <- 
    idDirectories %>% 
    grep("*.*.gpkg$", ., value = TRUE) %>% 
    .[!grepl(paste0(id, ".gpkg"), .)]
  
  # user communication
  paste0(id, " fully downloaded. Writing results to ", 
         fileDirectory, id, ".gpkg") %>% 
    message()
  
  # read each layer
  tiles <- 
    sf::st_sf(sf::st_sfc()) %>% 
    set.WGS()
  
  for(td in tileDirectories) {
    
    # union files
    lyrs <- 
      td %>% 
      sf::st_layers() %>% 
      .$name
    
    for (lyr in lyrs) {   
      tiles <-
        sf::st_read(td, layer = lyr, quiet = TRUE) %>% 
        bind_rows(tiles, .)
      
    }
    
   
  }
    # write each layer
    tiles %>%   
      sf::st_write(., dsn = paste0(fileDirectory, id, ".gpkg"),
                   layer = lyr, append = FALSE)
  
    rm(tiles)
    
    gc()
    
  backupDir <- paste0(fileDirectory, "backup")
  if (!dir.exists(backupDir)) dir.create(backupDir)
  
  # copy to backup directory and removing files
  file.copy(idDirectories, backupDir)
  file.remove(idDirectories)
  
}

createOutput(in_list$code[1], out_path)
