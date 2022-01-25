snapPTLsf <- function(points_sf, lines_sf, crs = 3035) {
  
  x <- 
    points_sf %>% 
    as_Spatial() 
  
  y <- 
    lines_sf %>% 
    as_Spatial()
  
  output <- 
    pkgcond::suppress_conditions({
      pkgcond::suppress_messages({
        pkgcond::suppress_warnings({
          snapPointsToLines(points = x, lines = y, withAttrs = TRUE) %>% 
            st_as_sf() %>% 
            st_transform(crs)
        })})})
  return(output)
}