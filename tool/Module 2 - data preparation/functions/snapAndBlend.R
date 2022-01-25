snapAndBlend <- function(cs, boundary, buildEntries, 
                         parkEntries, net_clean) {
  
  #network_blend <- NULL
  # 1. tile boundaries (3km?)
  cityGrid <-
    boundary %>% 
    st_transform(3035) %>% 
    st_make_grid(cellsize = cs) %>% 
    st_as_sf() %>% 
    filter(st_intersects(., boundary, sparse = FALSE))
  
  #ncore <- detectCores() - 1
  #cl <- makeCluster(ncore)
  #registerDoParallel(cl)
  
  tmp <- "C:/Berlin/tiles/"
  
  for (i in 1:nrow(cityGrid)) {
    
    # require(dplyr)
    # require(sf)
    # require(igraph)
    # require(tidygraph)
    # require(sfnetworks)
    # source("C:/r_toolbox/snapPointsToLines.R")
    # 
    
    paste(i, "of", nrow(cityGrid)) %>% 
      message()
    
    # 2. tile buildings, parks & network (boundary tile + 100m (-> 3.1km))
    # intersection
    build_tile <-
      buildEntries %>% 
      st_transform(3035) %>% 
      st_intersection(cityGrid$x[i]) %>% 
      st_cast("POINT")
    
    park_tile <- 
      parkEntries %>% 
      st_transform(3035) %>% 
      st_intersection(cityGrid$x[i]) %>% 
      st_cast("POINT")
    
    gb <- st_buffer(cityGrid$x[i], 100)
    
    net_tile <- 
      net_clean %>% 
      activate("edges") %>% 
      st_as_sf() %>% 
      st_transform(3035) %>%
      st_filter(gb, .predicate = st_intersects) %>% 
      st_cast("LINESTRING")
    
    # 3. snap_points_to_lines 
    if (nrow(build_tile) > 0) {
      build_snap <- snapPTLsf(build_tile, net_tile)
    } else build_snap <- NULL
    
    if (nrow(park_tile) > 0) {
      park_snap <- snapPTLsf(park_tile, net_tile)
    } else park_snap <- NULL
    
    # 4. blend
    if (is.null(build_snap)) {nodes <- park_snap
    } else if (is.null(park_snap)) {nodes <- build_snap}  else {
      nodes <- bind_rows(build_snap, park_snap)}
    
    nodes <-
      nodes %>% 
      select(matches("identifier"), 
             matches("code_2018"),
             matches("population"),
             matches("area")) 
    
    rownames(nodes) <- 1:nrow(nodes)
    
    tile_blend <-
      as_sfnetwork(net_tile) %>%
      st_network_blend(y = nodes)
    
    # 5. Join city network
    #if (is.null(network_blend)) network_blend <- tile_blend else {
    #  network_blend <- st_network_join(network_blend, tile_blend) }
    # 5. Write results to temp file
    tile_blend %>%
      activate("edges") %>%
      st_as_sf() %>%
      st_write(paste0(tmp, "\\edges", i, ".gpkg"), quiet = TRUE)
    
    tile_blend %>%
      activate("nodes") %>%
      st_as_sf() %>%
      st_write(paste0(tmp, "\\nodes", i, ".gpkg"), quiet = TRUE)
    
    rm(tile_blend, nodes, net_tile, park_tile, park_snap, 
       build_tile, build_snap)
    
    gc()
  }
  
  # return(network_blend)
}