park_id <-
  parkEntries %>% 
  pull("identifier") %>% 
  unique()

i <- 321

# PARK ENTRIES
pe <-
  berlin_network %>% 
  activate("nodes") %>% 
  #filter(identifier == park_id[i]) %>% 
  filter(!is.na(area)) %>% 
  st_as_sf() %>% 
  select(area) 

p <- pe %>% 
  st_geometry() %>% 
  st_combine() %>% 
  st_centroid()
  
# distance for service area size (capped roughly between 500 m and 1800 m)
s <- pe$area[1]
#s <- 53177440
#s <- 117
d <- round(s * (s ** -.5) + 500) 

nbh <- berlin_network %>% 
  convert(to_spatial_neighborhood, p, d)

be <-
  berlin_network %>% 
  activate("nodes") %>% 
  filter(!is.na(population)) %>%
  st_as_sf()

# 2) COST MATRIX
od_cost <- 
  berlin_network %>% 
  activate("edges") %>% 
  st_network_cost(x = .,
                  from = be$geom,
                  to = pe$geom) #%>% 
  apply(., 1, function(x) which(x == min(x))[1])


# 3) CALCULATE INDICES
be_index <-
  be %>% 
  mutate(nearest_entry = od_cost,
         area = s,
         euklid_dis = mapply(function(x, y) st_distance(x,y), 
                             .$geom, pe$geom[nearest_entry])) %>%
  arrange(nearest_entry) %>% 
  mutate(sPath = sapply(unique(nearest_entry), 
                        function(i) st_network_paths(x = nbh,
                                                     from = pe$geom[i],
                                                     to = .$geom[.$nearest_entry == i]) %>%
                          pull(edge_paths)) %>% 
           unlist(recursive = FALSE),
         net_dist = sPath %>% 
           sapply(function(x) nbh %>% 
                    activate("edges") %>% 
                    slice(unlist(x)) %>% 
                    pull(weight) %>% 
                    sum(na.rm = TRUE) %>% 
                    round(2))) %>% 
  mutate(loc_sig = population * area / net_dist,
         di = euklid_dis / net_dist)

# bring local significance down to edge level
lsDF <- 
  be_index %>% 
  st_drop_geometry() %>% 
  select(sPath, loc_sig) %>% 
  mutate(sPath = sapply(sPath, function(x) unlist(x) %>% 
                          paste(collapse = " "))) %>% 
  tidyr::separate_rows(sPath, sep = " ") %>% 
  group_by(sPath) %>% 
  summarise(loc_sig = sum(loc_sig, na.rm = TRUE)) %>% 
  mutate(sPath = as.numeric(sPath)) %>% 
  na.omit()

nbh_index <-
  nbh %>% 
  activate("edges") %>% 
  mutate(sPath = .tidygraph_edge_index,
         ls_total = 0) %>% 
  left_join(lsDF, by = "sPath") %>% 
  mutate(loc_sig = ifelse(is.na(loc_sig), 0, loc_sig),
         ls_total = loc_sig + ls_total) %>% 
  activate("nodes") %>% 
  left_join(st_drop_geometry(be_index))


nbh_test <-
nbh_index %>% 
  activate("nodes") %>% 
  st_as_sf()
  left_join(st_drop_geometry(be_index))

  
  
  
    node_out <-
      be_index %>% 
      select(node_id, di_temp, di_count_temp) %>% 
      mutate(di_temp = ifelse(is.na(di_temp), 0, di_temp),
             di_count_temp = ifelse(is.na(di_count_temp), 0, di_count_temp)) %>% 
      st_drop_geometry() %>% 
      left_join(node_out, ., by = "node_id") %>% 
      mutate(detour_index = di_temp + detour_index,
             di_count = di_count_temp + di_count) %>% 
      select(node_id, detour_index, di_count)
      
    edge_out <-
      nbh_index %>% 
      left_join(edge_out, ., by = "edge_id") %>% 
      mutate(local_significance = ls_temp + local_significance) %>% 
      select(edge_id, local_significance)
    
    # activate("nodes") %>% 
    # select(node_id, detour_index, di_count) %>% 
    # left_join(st_drop_geometry(be_index), by = "node_id") %>% 
    # mutate(di_temp = ifelse(is.na(di_temp), 0, di_temp),
    #        detour_index = di_temp + detour_index,
    #        di_count_temp = ifelse(is.na(di_count_temp), 0, di_count_temp),
    #        di_count = di_count_temp + di_count)
    
    be_index %>% 
      mutate(sPath = sapply(unique(nearest_entry), 
                            function(i) st_network_paths(x = nbh,
                                                         from = pe$geom[i],
                                                         to = .$geom[.$nearest_entry == i]) %>%
                              pull(edge_paths)) %>% 
               unlist(recursive = FALSE))