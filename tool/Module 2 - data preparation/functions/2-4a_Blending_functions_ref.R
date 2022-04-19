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

snapAndBlend <- function(city_code, city_boundaries, build_entries, gs_entries,
                         network, output_dir,
                         cellsize = 3000, crs = 3035, perc_core = .75)
{
  require(doParallel, quietly = TRUE)
  cityGrid <- mkCityGrid(city_code, city_boundaries, cellsize)
  ncore <- round(detectCores() * perc_core)
  cl <- makeCluster(ncore, outfile = "")
  registerDoParallel(cl)
  foreach(i = 1:nrow(cityGrid), packages = c(dplyr, sf, sfnetworks)) %dopar% {
    getwd() %>%
      paste0("/tool/Module 2 - data preparation/functions/") %>%
      list.files(pattern = "2-4[A-Za-z].*\\.R|2_.*\\.R", full.names = TRUE) %>%
      for (file in .) source(file)
    edge_out <- paste0(output_dir, "edges", i, ".gpkg")
    node_out <- paste0(output_dir, "nodes", i, ".gpkg")
    gridBox <- getGb(cityGrid, i)
    build_tile <- getNodes(build_entries, gridBox)
    gs_tile <- getNodes(gs_entries, gridBox)
    net_tile <- getEdges(network, cityGrid, i)
    nodes <- mergeNodes(build_tile, gs_tile, node_out)
    if (is.null(nodes)) {write_sf(net_tile, edge_out, append = FALSE)
      try(next)}
    net <- blend(net_tile, nodes)
    writeOutput(net, edge_out, node_out)
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
