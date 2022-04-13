################################################################################
# MODULE 2 - DATA PREPARATION
# PART 4 - NETWORK BLENDING
# 4c - OPERATOR FUNCTIONS
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################
#
# FUNCTIONS:
# 1. snapAndBlend
# 2.networkCombinator
#
################################################################################
# 6. CONVERT TO BUILDING CENTROID
#    -> POINT ON SURFACE
#    -> SNAP BUILDING CENTROIDS TO NETWORK
# 8. OUTPUT TO TEMP
################################################################################
# 1. SNAPPING AND BLENDING NETWORK COMPONENTS TOGETHER
# REQUIRED SETTINGS:
# city_boundary: Setting description
# city_code: Setting description
# build_entries: Setting description
# gs_entries: Setting description
# network: Setting description
# output_dir: Setting description
# OPTIONAL SETTINGS:
# cellsize: Setting description - DEFAULT values
# crs: Setting description - DEFAULT values
################################################################################
#
# drive <- "D:/temp"
# city_boundary <- paste0(drive, "/cities.gpkg")
# city_code <- "DE001"
# build_entries <- paste0(drive, "/building_entries.gpkg")
# gs_entries <- paste0(drive, "/green_space_entries.gpkg")
# network <- paste0(drive, "/network_clean.gpkg")
# output_dir <- paste0(drive, "/net_blend/")
# cellsize = 3000
# crs = 3035
# perc_core = .5

snapAndBlend <- function(city_code, city_boundary, build_entries, gs_entries,
                         network, output_dir,
                         cellsize = 3000, crs = 3035, perc_core = .75)
{
  # Load required packages
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)
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
  ncore <- round(detectCores() * perc_core)
  cl <- makeCluster(ncore, outfile = "")
  registerDoParallel(cl)
  # Iterate through city tiles
  foreach(i = 1:nrow(cityGrid)) %dopar% {
    #foreach(i = todo) %dopar% {
    require(dplyr)
    require(sf)
    require(sfnetworks)
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
    # Exception handling buildings and green space entries to network
    if (is.null(build_tile)) {build_tile <- NULL
    } else if (nrow(build_tile) == 0) build_tile <- NULL
    if (is.null(gs_tile)) {gs_tile <- NULL
    } else if (nrow(gs_tile) == 0) gs_tile <- NULL

    # Make sure object is not empty
    if (is.null(build_tile) & is.null(gs_tile)) {
      bind_rows(build_tile, gs_tile) %>%
        st_write(node_out, quiet = TRUE, append = TRUE)
      st_write(net_tile, edge_out, quiet = TRUE, append = TRUE)
    } else {
      if (is.null(build_tile)) {
        nodes <- mutate(gs_tile, population = NA, ID = NA)
      } else if (is.null(gs_tile)) {
        nodes <- mutate(build_tile, city_code = NA,
                        class = NA, identifier = NA,
                        area = NA)}  else {
                          nodes <- dplyr::bind_rows(build_tile, gs_tile) %>%
                            dplyr::distinct()}
      # Blend buildings and green space entries to network
      # edges <- sf::st_collection_extract(
      #   lwgeom::st_split(
      #     net_tile,
      #     sf::st_buffer(nodes, 1e-5)),
      #   "LINESTRING")
      # make sure edges and node geometries are at exact same places
      # st_geometry(edges) <- edges %>%
      #   st_geometry() %>%
      #   lapply(function(x) round(x, 0)) %>%
      #   st_sfc(crs = 3035)
      # edges <- distinct(edges)
      # st_geometry(nodes) <- nodes %>%
      #   st_geometry() %>%
      #   lapply(function(x) round(x, 0)) %>%
      #   st_sfc(crs = 3035)
      # nodes <- distinct(nodes)
      net <- net_tile %>%
        as_sfnetwork() %>%
        st_network_blend(nodes)
      edges <- net %>%
        activate("edges") %>%
        st_as_sf() %>%
        select(-matches("from|to")) %>%
        distinct()
      nodes <- net %>%
        activate("nodes") %>%
        st_as_sf() %>%
        select(-matches("city_code|class")) %>%
        distinct()
      # Write output
      st_write(nodes, node_out, quiet = TRUE, append = FALSE)
      st_write(edges, edge_out, quiet = TRUE, append = FALSE)
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
#file_list <- list.files("Z:/Berlin/net_blend/", pattern = "edges", full.names = TRUE)
#combinator(file_list, output_dir = "D:/Berlin")

networkCombinator <- function(file_list, out = "file", output_dir = NULL)
{
  # load packages
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  # check output consistency
  output_dir <- outputChecker(directory = output_dir, file_name = "edges.gpkg")
  # set up progress bar
  pb <- txtProgressBar(min = 0, max = length(file_list),
                       initial = 0, style = 3)
  stepi <- 0
  outSF <- NULL
  # iterate through files
  for (file in file_list) {
    # for progress bar
    stepi <- stepi + 1
    setTxtProgressBar(pb, stepi)
    # Read input in file_list and execute a network join
    tmp <- file %>%
      st_read(quiet = TRUE) %>%
      filter(!st_is_empty(.)) %>%
      as_sfnetwork()
    if (!is.null(outSF)) {
      outSF <- tmp %>%
        st_network_join(outSF, .)
    } else {
      outSF <- tmp
    }
  }
  if (out == "file") {
    outSF %>%
      activate("edges") %>%
      st_as_sf() %>%
      remove_overlap() %>%
      mutate(edge_id = row_number()) %>%
      st_write(paste0(output_dir, "/edges.gpkg"))
  } else outSF %>%
    activate("edges") %>%
    st_as_sf() %>%
    remove_overlap() %>%
    mutate(edge_id = row_number()) %>%
    return()
}


################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################
# file_list <- list.files("D:/Berlin/net_blend/", pattern = "nodes", full.names = TRUE)
# file_list <- nodes
#file <- file_list[2]

#combinator(file_list, output_dir = "D:/Berlin")

combinator <- function(file_list, out = "file", output_dir = NULL)
{
  # load packages
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  # check output consistency
  output_dir <- outputChecker(directory = output_dir, file_name = "nodes.gpkg")
  # set up progress bar
  pb <- txtProgressBar(min = 0, max = length(file_list),
                       initial = 0, style = 3)
  stepi <- 0
  # iterate through files
  for (file in file_list) {
    # for progress bar
    stepi <- stepi + 1
    setTxtProgressBar(pb, stepi)
    # Read input in file_list and execute a network join
    tmp <- tryCatch({file %>%
      st_read(quiet = TRUE) %>%
      filter(!st_is_empty(.))}, error = function(e) return(tibble()))
    # Check for content
    if (nrow(tmp) > 0) {
      tmp %>%
        select(identifier, area, population, ID) %>%
        mutate(identifier = as.character(identifier),
               area = as.double(area),
               population = as.double(population),
               ID = as.character(ID)) %>%
        st_write(output_dir, layer = "nodes",
                 append = TRUE, quiet = TRUE)
    } else rm(tmp)
  }
  if (out != "file") {
    out <- st_read(output_dir)
    unlink(output_dir)
    out
  } else { message("\n done!")
    output_dir
  }
}


################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################
outputChecker <- function(directory, file_name, output_type = "file")
{
  if (is.null(directory) & output_type == "file") {
    directory <- readline(prompt = "Please enter output directory.")
  }
  if (!dir.exists(directory)) {
    a <- readline(prompt = "Output directory does not exist. Shall I create it? (y/n)")
    if (a == "y") dir.create(directory) else stop("Specify different output directory.")
  }
  if (output_type == "file" & !is.null(directory)) {
    tmpOut <- paste0(directory, "/", file_name)
  } else tmpOut <- paste0(getwd(), "/", file_name)
  if (file.exists(tmpOut)) {
    b <- readline(prompt = "Output file exists. Remove? (y/n)")
    if (b == "y") unlink(tmpOut) else stop("Specify different output directory.")
  }
  tmpOut
}

#4.1.1
remove_overlap <- function(network)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  network <- distinct( select(network, geom) )
  eb <- st_buffer(network, 1e-5)
  network$covers <- c(st_covers(eb, network))
  covering <- filter(network, lengths(covers) > 1)
  notCovering <- filter(network, lengths(covers) < 2)
  if (nrow(covering) < 1) return( select(network, geom) )
  st_difference(st_union(covering), st_union(notCovering)) %>%
    st_cast("MULTILINESTRING", do_split = TRUE, warn = FALSE) %>%
    st_cast("LINESTRING", do_split = TRUE, warn = FALSE) %>%
    st_sf() %>%
    rename(geom = geometry) %>%
    bind_rows(notCovering, .) %>%
    distinct() %>%
    select(geom) %>%
    return()
}

################################################################################
# END OF DOCUMENT
################################################################################
