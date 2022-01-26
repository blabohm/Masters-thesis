# function that uses simple dplyr::bind_rows functionality to
# combine multiple vector files

combinator <- function(file_list) {
  
  # load packages
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  
  # make sure output is empty
  outSF <- NULL
  
  # iterate through files
  for (file in file_list) {
    if (!is.null(outSF)) {
      
      outSF <- 
        st_read(file, quiet = TRUE) %>% 
        bind_rows(outSF)
      
    } else {
      outSF <- st_read(file, quiet = TRUE)
    } 
  }
  return(outSF)
}
