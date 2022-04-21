################################################################################
# MODULE 'NUMBER' - MODULE NAME
# PART 'NUMBER' - SUBMODULE NAME
# PART 'NUMBER' + letter - FUNCTIONS THEME
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################
#
# FUNCTIONS:
# 1. FUNCTION NAME
#    -> Function description
#
################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################
# ID <- "76213-DE001L1" # v-park
# ID <- "81210-DE001L1" # t-feld
# perc_core <- .75
# d <- 500
# folder <- "D:/temp/"
################################################################################
# Urban atlas identifiers of green spaces
# Network node file path
# Network edge file path
# Where to store index .csv files (directory)?
# Percent of computational core to be used
# Distance

calcIndices <- function(green_space_IDs, in_directory, out_directory,
                        perc_core = .75, d = 500)
{
  # create output directory if it doesn't exist
  if (!dir.exists(in_directory)) dir.create(in_directory)
  # Set up parallel processing
  require(doParallel)
  ncore <- round(detectCores() * perc_core)
  cl <- makeCluster(ncore, outfile = "")
  registerDoParallel(cl)
  foreach(ID = green_space_IDs) %dopar% {
  #for (ID in green_space_IDs) {
    # Load required packages for each core
    require(sf, quietly = TRUE)
    require(sfnetworks, quietly = TRUE)
    require(tidygraph, quietly = TRUE)
    require(dplyr, quietly = TRUE)
    getwd() %>%
      paste0("/tool/Module 3 - index building/functions/") %>%
      list.files(pattern = "3-1[A-Za-z].*\\.R", full.names = TRUE) %>%
      for (file in .) source(file)
    # 1
    gs_entries <- load_gs_entries(ID, in_directory)
    # 2
    build_entries <- load_build_entries(in_directory, gs_entries, d)
    # 3
    network <- load_network(in_directory, gs_entries)
    if (nrow(build_entries) == 0) next
    # 4
    out <- add_params(build_entries, gs_entries, network)
    if (nrow(out) < 1) next
    # 5
    write_output(out, network, out_directory, ID)
  }
  parallel::stopCluster(cl)
}


################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################
# drive <- "D:/temp/"
# building_polygons <- paste0(drive, "/buildings.gpkg")
# index_dir <- paste0(drive, "/indices/")
################################################################################

gatherDI <- function(building_polygons, index_dir, output_dir)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  #building_polygons
  di_files <- list.files(index_dir,
                         pattern = "nodes[1-9]*.*csv$",
                         full.names = TRUE)

  for (i in di_files) {
    di_values <- read.csv(i)
    if (nrow(di_values) > 0) if (unique(di_values$di) != Inf) {
      if (i != first(di_files)) tmpDf <- bind_rows(tmpDf, di_values) else tmpDf <- di_values
    }
  }
  di_out <-
    tmpDf %>%
    group_by(ID) %>%
    summarise(di = mean(di))

  building_polygons %>%
    st_read(quiet = TRUE) %>%
    left_join(di_out) %>%
    st_write(output_dir)
}


################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################
# drive <- "D:/temp/"
# edges <- paste0(drive, "/edges.gpkg")
# index_dir <- paste0(drive, "/indices/")
################################################################################

gatherLS <- function(edges, index_dir, output_dir)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  ls_files <- list.files(index_dir,
                         pattern = "edges[1-9]*.*csv$",
                         full.names = TRUE)

  for (i in ls_files) {
    require(dplyr, quietly = TRUE)
    require(sf, quietly = TRUE)
    ls_values <- read.csv(i)
    if (nrow(ls_values) > 0) if (unique(ls_values$ls) != Inf) {
      if (i != first(ls_files)) tmpDf <- bind_rows(tmpDf, ls_values) else tmpDf <- ls_values
    }
  }
  ls_out <-
    tmpDf %>%
    group_by(edge_id) %>%
    summarise(ls = mean(ls))

  edges %>%
    st_read(quiet = TRUE) %>%
    left_join(ls_out) %>%
    st_write(output_dir)
}


################################################################################
# END OF DOCUMENT
################################################################################

