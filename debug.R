tile <- be_tiles[1]


beDir %>%
  st_read(quiet = TRUE) %>%
  st_point_on_surface() %>%
  st_write("D:/Berlin/buildings_cent.gpkg")


st_collection_extract(lwgeom::st_split(net_tile, nodes), "LINESTRING")




edges <- edges %>%
  mutate(edgeID = c(1:n()))

nodes <- edges %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(edgeID = L1) %>%
  group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2)) %>%
  mutate(xy = paste(.$X, .$Y)) %>%
  mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
  select(-xy)

source_nodes <- nodes %>%
  filter(start_end == 'start') %>%
  pull(nodeID)

target_nodes <- nodes %>%
  filter(start_end == 'end') %>%
  pull(nodeID)

edges = edges %>%
  mutate(from = source_nodes, to = target_nodes)

nodes <- nodes %>%
  distinct(nodeID, .keep_all = TRUE) %>%
  select(-c(edgeID, start_end)) %>%
  st_as_sf(coords = c('X', 'Y')) %>%
  st_set_crs(st_crs(edges)) #%>%

nodes <- nodes %>%
  st_join(nodes)



as_sfnetwork(edges) %>%
  activate("nodes") %>%
  st_join(nodes) %>%
  plot(add = TRUE)

sfnetwork(nodes = nodes, edges = edges$geom, directed = FALSE)

nodes2 <- as_sfnetwork(edges) %>%
  activate("nodes") %>%
  st_join(nodes) %>%
  st_as_sf()


test <-
  as_sfnetwork(edges) %>%
  activate("nodes") %>%
  st_join(distinct(nodes)) %>%
  st_as_sf()

nodes %>% filter(!(ID %in% test$ID))


gs <- st_read(gs_entries)

st_geometry(gs) <- gs %>%
  st_geometry() %>%
  lapply(function(x) round(x, 0)) %>%
  st_sfc(crs = 3035)

gs1 <- distinct(gs)

st_write(gs1, "D:/Berlin/green_space_entries2.gpkg")


system.time({

  if (nrow(build_tile) > 0) {
    build_snap <- st_snap_points(points = build_tile, lines = net_tile)
  } else build_snap <- NULL
  if (nrow(gs_tile) > 0) {
    gs_snap <- st_snap_points(points = gs_tile, lines = net_tile)
  } else gs_snap <- NULL
  # Make sure object is not empty
  if (is.null(build_snap)) {nodes <- gs_snap
  } else if (is.null(gs_snap)) {nodes <- build_snap}  else {
    nodes <- dplyr::bind_rows(build_snap, gs_snap) %>%
      dplyr::distinct()}

  tile_blend1 <-
    as_sfnetwork(net_tile) %>%
    st_network_blend(y = nodes)
})

system.time({

  nodes1 <- bind_rows(build_tile, gs_tile)

  tile_blend <-
    as_sfnetwork(net_tile) %>%
    st_network_blend(y = nodes1)
})


system.time({

  # Snap buildings and green space entries to network
  if (nrow(build_tile) > 0) {
    build_snap <- st_snap_points(points = build_tile, lines = net_tile)
  } else build_snap <- NULL
  if (nrow(gs_tile) > 0) {
    gs_snap <- st_snap_points(points = gs_tile, lines = net_tile)
  } else gs_snap <- NULL
  # Make sure object is not empty
  if (is.null(build_snap)) {nodes <- gs_snap
  } else if (is.null(gs_snap)) {nodes <- build_snap}  else {
    nodes <- dplyr::bind_rows(build_snap, gs_snap) %>%
      dplyr::distinct()}
  # Blend buildings and green space entries to network
  edges <- sf::st_collection_extract(lwgeom::st_split(net_tile, sf::st_buffer(nodes, 1e-5)),
                                     "LINESTRING")
  st_geometry(edges) <- edges %>%
    st_geometry() %>%
    lapply(function(x) round(x, 0)) %>%
    st_sfc(crs = 3035)

  st_geometry(nodes) <- nodes %>%
    st_geometry() %>%
    lapply(function(x) round(x, 0)) %>%
    st_sfc(crs = 3035)

  edges <- distinct(edges)
  # st_write(edges, edge_out, quiet = TRUE)

  nodes_net <- as_sfnetwork(edges) %>%
    activate("nodes") %>%
    st_join(distinct(nodes)) %>%
    st_as_sf() #%>%
  #st_write(node_out, quiet = TRUE)

  nodes %>% filter(!(ID %in% nodes_net$ID)) %>% st_write()
  nodes_net %>% filter(geom %in% missed_nodes$geometry)


})

n_ls %>% filter(!is.na(population) | !is.na(area)) %>%
  distinct()


nodes %>% distinct()

n_sb <- tile_blend1 %>% activate("nodes") %>% st_as_sf()
e_sb <- tile_blend1 %>% activate("edges") %>% st_as_sf()

n_b <- tile_blend %>% activate("nodes") %>% st_as_sf()
e_b <- tile_blend %>% activate("edges") %>% st_as_sf()

n_ls <- tile_blend2 %>% activate("nodes") %>% st_as_sf()
e_ls <- tile_blend2 %>% activate("edges") %>% st_as_sf()
