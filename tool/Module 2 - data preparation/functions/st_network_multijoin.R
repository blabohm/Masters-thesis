
st_network_multijoin <- function(park_entries, distance, network) {
# sophisticated way of converting to neighborhood
b <- st_coordinates(park_entries)
xmin <- b[which.min(b[,1]),] %>% st_point()
xmax <- b[which.max(b[,1]),] %>% st_point()
ymin <- b[which.min(b[,2]),] %>% st_point()
ymax <- b[which.min(b[,2]),] %>% st_point()
p <- st_sfc(xmin, xmax, ymin, ymax, crs = 3035)

bn_clip <- p %>% 
  st_geometry() %>% 
  st_combine() %>% 
  st_cast("POLYGON") %>%
  st_buffer(distance) %>% 
  st_filter(activate(network, "nodes"), ., .pred = st_intersects)

nbh1 <- convert(bn_clip, to_spatial_neighborhood, p[1], distance) 
nbh2 <- convert(bn_clip, to_spatial_neighborhood, p[2], distance) 
nbh3 <- convert(bn_clip, to_spatial_neighborhood, p[3], distance) 
nbh4 <- convert(bn_clip, to_spatial_neighborhood, p[4], distance) 


nbh5 <- st_network_join_working(nbh1, nbh2)
nbh6 <- st_network_join_working(nbh3, nbh4)
nbh <- st_network_join_working(nbh5, nbh6) %>% 
  convert(to_undirected) %>% 
  filter(group_components() == 1) %>% 
  distinct()

rm(nbh1, nbh2, nbh3, nbh4, nbh5, nbh6)

return(nbh)}
