library(dplyr)
library(sf)
library(igraph)
library(tidygraph)
library(sfnetworks)
#library(foreach)
library(doParallel)

for (i in list.files("C:/r_toolbox", full.names = TRUE)) source(i)

################################################################################
# CITY LEVEL
################################################################################

# READ UA DATA AND FILTER FOREST + URBAN GREEN CLASSES
baseDir <- "C:/Berlin/"

################################################################################
# READ UA GPKG
gpkg <- paste0(baseDir, "UA2018/DE001/DE001L1_BERLIN_UA2018_v013.gpkg")

lr <- st_layers(gpkg)
land_use <- st_read(gpkg, lr$name[1], quiet = TRUE) %>% 
  st_transform(3035)
boundary <- st_read(gpkg, lr$name[3], quiet = TRUE) %>% 
  st_transform(3035)


################################################################################
# NETWORK
################################################################################

net_clean <- 
  paste0(baseDir, "network_clean.gpkg") %>% 
  st_read(quiet = TRUE) %>% 
  select(highway) %>% 
  st_transform(3035) %>%
  distinct() %>%
  st_cast("LINESTRING") %>% 
  as_sfnetwork() %>% 
  filter(group_components() == 1) %>% 
  activate("edges") %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_geometry() %>%
  lapply(function(x) round(x, 0)) %>%
  st_sfc(crs = 3035) %>% 
  as_sfnetwork() %>% 
  convert(to_spatial_smooth) %>%
  convert(to_spatial_subdivision) 

net_clean <- 
  st_read("C:/Berlin/network_berlin_clean_sf1.gpkg", quiet = TRUE) %>% 
  as_sfnetwork()

################################################################################
# PARK ENTRIES 
################################################################################

parkEntries <-
  paste0(baseDir, "park_entries.gpkg") %>% 
  st_read(quiet = TRUE) %>% 
  select(area, identifier) %>% 
  st_transform(3035) %>% 
  st_cast("POINT")


################################################################################
# BUILDING ENTRIES
################################################################################

res_class <- c(11100, 11210, 11220, 
               11230, 11240, 11300)

resLyr <- 
  paste0(baseDir, "UA2018/DE001/DE001L1_BERLIN_UA2018_v013.gpkg") %>% 
  st_read(layer = lr$name[1], quiet = TRUE) %>% 
  filter(code_2018 %in% res_class)

buildEntries <- 
  paste0(baseDir, "berlin_cent_core.gpkg") %>% 
  st_read(quiet = TRUE) %>% 
  st_transform(3035) %>% 
  st_intersection(resLyr) %>%
  mutate(population = ifelse(population == 0 & pop_by_area > population, 
                             pop_by_area, population),
         building_id = row_number()) %>% 
  filter(population > 0) %>% 
  select(code_2018, population, identifier) 


################################################################################
# SNAP & BLEND
# Snapping function for sf objects





# network_rdy <- snapAndBlend(cs = 5000, boundary, buildEntries, 
#                             parkEntries, net_clean)

# 6. combine files back into sfnetwork object
nodes <- 
  list.files("C:/Berlin/tiles/", 
             pattern = "node.+.gpkg$", full.names = TRUE) %>% 
  combinator() %>% 
  distinct()

edges <- 
  list.files("C:/Berlin/tiles/", 
             pattern = "edge.+.gpkg$", full.names = TRUE) %>% 
  
  network_combinator()  

# st_write(nodes, "C:/Berlin/berlin_nodes1.gpkg")
# 
# edges %>% 
#   activate("edges") %>% 
#   st_as_sf() %>% 
#   st_write("C:/Berlin/berlin_edges1.gpkg")
################################################################################
# SETTING UP NETWORK FROM FILE
nodes <- 
  st_read("C:/Berlin/berlin_nodes1.gpkg", quiet = TRUE) %>% 
  distinct() %>% 
  mutate(node_id = row_number())

edges <- 
  st_read("C:/Berlin/berlin_edges1.gpkg", quiet = TRUE) %>% 
  distinct() %>% 
  mutate(edge_id = row_number())

network <-
  edges %>% 
  as_sfnetwork() %>% 
  st_join(nodes) %>% 
  convert(to_undirected) %>% 
  activate("edges") %>% 
  mutate(weight = edge_length())

# edges <- berlin_network_pre %>% 
#   activate("edges") %>% 
#   st_as_sf()
# 
# nodes <- berlin_network_pre %>% 
#   activate("nodes") %>% 
#   st_as_sf()
#  
# berlin_network <-
#   sfnetwork(edges = edges, 
#             nodes = nodes, 
#             node_key = "node_id",
#             force = TRUE,
#             directed = FALSE) %>% 
#   activate("edges") %>% 
#   mutate(weight = edge_length())
# 
# rm(berlin_nodes, berlin_edges)
# gc()

# network <-
#   network %>% 
#   activate("edges") %>% 
#   mutate(local_significance = 0) %>% 
#   activate("nodes") %>% 
#   
#rm(berlin_network_pre, nodes, edges, land_use)

# sf objects to write output
node_out <-
  network %>% 
  activate("nodes") %>% 
  st_as_sf() %>% 
  mutate(detour_index = 0,
         di_count = 0) %>% 
  select(node_id, detour_index, di_count)

edge_out <-
  network %>% 
  activate("edges") %>% 
  st_as_sf() %>% 
  mutate(local_significance = 0) %>% 
  select(edge_id, local_significance)

################################################################################
# PARK / SERVICE AREA LEVEL
################################################################################

# 1) SERVICE AREA
#park_id <- "76213-DE001L1"
# park_id <-
#   parkEntries %>% 
#   pull("identifier") %>% 
#   unique()

park_id <-
  network %>% 
  activate("nodes") %>% 
  filter(!is.na(area)) %>% 
  pull(identifier) %>% 
  unique()

N <- 1:length(park_id)

for (i in N[c(6, 9, 10, 11, 12, 13, 14, 17, 18)]) {
  
  # user communication
  paste(i, "of", max(N)) %>% message()
  # PARK ENTRIES
  pe <-
    network %>% 
    activate("nodes") %>% 
    filter(identifier == park_id[i]) %>% 
    st_as_sf() %>% 
    select(area) %>% 
    distinct()
  
  # park area
  s <- pe$area[1]
  
  # distance for service area size (capped roughly between 500 m and 1800 m)
  #s <- 53177440
  #s <- 117
  d <- round(s * (s ** -.5) + 500) 
  
  # convert network to service area
  nbh <- st_network_multijoin(pe, d, network)
  
  # 2) COST MATRIX
  #building entries
  be <-
    nbh %>% 
    activate("nodes") %>% 
    filter(!is.na(population)) %>%
    st_as_sf() %>% 
    select(-detour_index, -di_count)
  
  if (nrow(be) == 0) message("No buildings in service area.") else {
    od_cost <- 
      st_network_cost(x = nbh,
                      from = be$geom,
                      to = pe$geom) %>% 
      apply(., 1, function(x) which(x == min(x))[1])
    
    ##############################
    if (length(od_cost %>% unique()) < 2) message("Write workaround for single entry!") else {
      
      ##############################
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
                 unlist(recursive = FALSE)) %>% 
        mutate(net_dist = sPath %>% 
                 sapply(function(x) nbh %>% 
                          activate("edges") %>% 
                          slice(unlist(x)) %>% 
                          pull(weight) %>% 
                          sum(na.rm = TRUE) %>% 
                          round(2))) %>% 
        mutate(ls_temp = population * area / net_dist,
               di_temp = euklid_dis / net_dist,
               di_count_temp = 1)
      
      # 4) ATTACH INDICES TO OUTPUT SF OBJECTS
      # sum local significance (building-entry_n - park-entry_n) for each edge
      ls_df <- 
        be_index %>% 
        st_drop_geometry() %>% 
        select(sPath, ls_temp) %>% 
        mutate(sPath = sapply(sPath, function(x) unlist(x) %>% 
                                paste(collapse = " "))) %>% 
        tidyr::separate_rows(sPath, sep = " ") %>% 
        group_by(sPath) %>% 
        summarise(ls_temp = sum(ls_temp, na.rm = TRUE)) %>% 
        mutate(sPath = as.numeric(sPath)) %>% 
        na.omit()
      
      # link local significance to edge id
      nbh_index <-
        nbh %>% 
        activate("edges") %>% 
        mutate(sPath = row_number()) %>% 
        select(edge_id, sPath, -local_significance) %>% 
        left_join(ls_df, by = "sPath") %>% 
        mutate(ls_temp = ifelse(is.na(ls_temp), 0, ls_temp)) %>%
        st_as_sf() %>% 
        st_drop_geometry() %>% 
        select(edge_id, ls_temp)
      
      node_out_test <-
        be_index %>% 
        select(node_id, di_temp, di_count_temp) %>% 
        mutate(di_temp = ifelse(is.na(di_temp), 0, di_temp),
               di_count_temp = ifelse(is.na(di_count_temp), 0, di_count_temp)) %>% 
        st_drop_geometry() %>% 
        left_join(node_out, ., by = "node_id") %>% 
        mutate(detour_index = di_temp + detour_index,
               di_count = di_count_temp + di_count) %>% 
        select(node_id, detour_index, di_count)
      
      edge_out_test <-
        nbh_index %>% 
        left_join(edge_out, ., by = "edge_id") %>% 
        mutate(local_significance = ls_temp + local_significance) %>% 
        select(edge_id, local_significance)
    }
  }
}

node_out_test %>% na.omit() %>% distinct()

node_out %>% 
  mutate(detour_index = detour_index / di_count) %>% 
  st_write("C:/Berlin/berlin_node_out1.gpkg")

edge_out %>% na.omit() %>% distinct()

st_write(edge_out, "C:/Berlin/berlin_edge_out2.gpkg")
