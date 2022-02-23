################################################################################
# MODULE 2 - DATA PREPARATION
# PART 4 - NETWORK BLENDING
# 4c - OPERATOR FUNCTIONS
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################
#
# FUNCTIONS:
# 1. sfc2bb
#    -> Return a boundary box polygon for intersection
# 2. snapPointsToLines
#    -> Snap points to lines while preserving attributes
# 3. snapPTLsf
#    -> wrapper around snapPointsToLines for sf objects
################################################################################
# 6. CONVERT TO BUILDING CENTROID
#    -> POINT ON SURFACE
#    -> SNAP BUILDING CENTROIDS TO NETWORK
# 8. OUTPUT TO TEMP
################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################

drive <- "Z"
city_boundary <- paste0(drive, ":/Berlin/cities.gpkg")
city_code <- "DE001"
build_entries <- paste0(drive, ":/Berlin/buildings_cent.gpkg")
gs_entries <- paste0(drive, ":/Berlin/green_space_entries2.gpkg")
network <- paste0(drive, ":/Berlin/network_clean1.gpkg")
output_dir <- paste0(drive, ":/Berlin/net_blend/")
cellsize = 1000
crs = 3035

snapAndBlend <- function(city_boundary, city_code, build_entries, gs_entries, network,
                         output_dir, cellsize = 3000, crs = 3035)
{
  # Load required packages
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(doParallel, quietly = TRUE)
  # Subset city boundaries
  bound_query <- paste0('SELECT * FROM \'cities\' WHERE URAU_CODE = \'',
                        city_code, '\'')
  city_boundary <- city_boundary %>%
    st_read(query = bound_query, quiet = TRUE) %>%
    st_transform(crs)
  # Tile boundaries
  cityGrid <- city_boundary %>%
    st_buffer(1000) %>%
    st_make_grid(cellsize = cellsize) %>%
    st_as_sf() %>%
    st_filter(city_boundary, .pred = st_intersects)
  # Set up parallel processing
  ncore <- round(detectCores() * .75)
  cl <- makeCluster(ncore, outfile = "")
  registerDoParallel(cl)
  # Iterate through city tiles
  foreach(i = 1:nrow(cityGrid)) %dopar% {
    #foreach(i = todo) %dopar% {
    require(dplyr)
    require(sf)
    getwd() %>%
      paste0("/tool/Module 2 - data preparation/functions/") %>%
      list.files(pattern = "2-4[A-Za-z].*\\.R|2_.*\\.R", full.names = TRUE) %>%
      for (file in .) source(file)
    # output directories:
    edge_out <- paste0(output_dir, "edges", i, ".gpkg")
    node_out <- paste0(output_dir, "nodes", i, ".gpkg")
    # User communication
    message(paste(i, "of", nrow(cityGrid)))
    # Intersect input with grid
    gridBox <- cityGrid[i,] %>%
      sf::st_geometry() %>%
      sf::st_as_text()
    # Buildings
    build_tile <- tryCatch({build_entries %>%
        sf::st_read(wkt_filter = gridBox, quiet = TRUE) %>%
        sf::st_cast("POINT")}, error = function(e) return(NULL))
    # Green space entries
    gs_tile <- tryCatch({gs_entries %>%
        sf::st_read(wkt_filter = gridBox, quiet = TRUE) %>%
        sf::st_cast("POINT")}, error = function(e) return(NULL))
    # Network (with buffer to avoid errors at edges)
    gridBox1 <- cityGrid[i,] %>%
      sf::st_buffer(100, nQuadSegs = 1) %>%
      sf::st_geometry() %>%
      sf::st_as_text()
    net_tile <- network %>%
      sf::st_read(wkt_filter = gridBox1, quiet = TRUE) %>%
      sf::st_cast("LINESTRING")
    # Snap buildings and green space entries to network
    if (!is.null(build_tile)) {
      if (nrow(build_tile) > 0) {
        build_snap <- st_snap_points(points = build_tile, lines = net_tile)
      } else build_snap <- NULL
    } else build_snap <- NULL
    if (!is.null(gs_tile)) {
      if (nrow(gs_tile) > 0) {
        gs_snap <- st_snap_points(points = gs_tile, lines = net_tile)
      } else gs_snap <- NULL
    } else gs_snap <- NULL
    # Make sure object is not empty
    if (is.null(build_snap) & is.null(gs_snap)) {
      bind_rows(build_tile, gs_tile) %>%
        st_write(node_out, quiet = TRUE, append = TRUE)
      st_write(net_tile, edge_out, quiet = TRUE, append = TRUE)
    } else {
      if (is.null(build_snap)) {
        nodes <- mutate(gs_snap, population = NA, ID = NA)
      } else if (is.null(gs_snap)) {
        nodes <- mutate(build_snap, city_code = NA,
                        class = NA, identifier = NA,
                        area = NA)}  else {
                          nodes <- dplyr::bind_rows(build_snap, gs_snap) %>%
                            dplyr::distinct()}
      # Blend buildings and green space entries to network
      edges <- sf::st_collection_extract(
        lwgeom::st_split(
          net_tile,
          sf::st_buffer(nodes, 1e-5)),
        "LINESTRING")
      # make sure edges and node geometries are at exact same places
      st_geometry(edges) <- edges %>%
        st_geometry() %>%
        lapply(function(x) round(x, 0)) %>%
        st_sfc(crs = 3035)
      edges <- distinct(edges)
      st_geometry(nodes) <- nodes %>%
        st_geometry() %>%
        lapply(function(x) round(x, 0)) %>%
        st_sfc(crs = 3035)
      nodes <- distinct(nodes)
      # Write output
      st_write(nodes, node_out, quiet = TRUE, append = TRUE)
      st_write(edges, edge_out, quiet = TRUE, append = TRUE)
    }
  }
  stopCluster(cl)
}


################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################

no <- list.files(output_dir, pattern = "node", full.names = TRUE)

system.time({
  for (f in ef) {
    st_read(f, quiet = TRUE) %>%
      st_write("Z:/Berlin/nodes.gpkg", quiet = TRUE, append = TRUE)
  }})
# function that uses simple dplyr::bind_rows functionality to
# combine multiple vector files
ef <- list.files(output_dir, pattern = "edge", full.names = TRUE)

edges <- network_combinator(ef)
edges %>%
  activate("edges") %>%
  st_as_sf() %>%
  st_write("Z:/edges_clean.gpkg")

network_combinator <- function(file_list) {

  # load packages
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  # make sure output is empty
  outSF <- NULL
  n <- 1
  # iterate through files
  for (file in file_list) {

    message(n, " of ", length(file_list))
    n <- n + 1

    if (!is.null(outSF)) {

      outSF <-
        st_read(file, quiet = TRUE) %>%
        filter(!st_is_empty(.)) %>%
        as_sfnetwork() %>%
        st_network_join(outSF)

    } else {
      outSF <-
        st_read(file, quiet = TRUE) %>%
        filter(!st_is_empty(.)) %>%
        as_sfnetwork()

    }
  }
  return(outSF)
}


################################################################################

