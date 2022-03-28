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
ID <- "76213-DE001L1" # v-park
ID <- "81210-DE001L1" # t-feld
drive <- "D:/temp/"
output <- paste0(drive, "indices")
nodes <- paste0(drive, "nodes.gpkg")
edges <- paste0(drive, "edges.gpkg")
perc_core <- .5
d <- 500
################################################################################

calcIndices <- function(
  # Urban atlas identifiers of green spaces
  green_space_IDs,
  # Network node file path
  nodes,
  # Network edge file path
  edges,
  # Where to store index .csv files (directory)?
  output,
  # Percent of computational core to be used
  perc_core = .75,
  # Distance
  d = 500)
{
  # create output directory if it doesn't exist
  if (!dir.exists(output)) dir.create(output)
  # Set up parallel processing
  # require(doParallel)
  # ncore <- round(detectCores() * perc_core)
  # cl <- makeCluster(ncore, outfile = "")
  # registerDoParallel(cl)
  # foreach(i = gs_IDs) %dopar% {
  for (ID in gs_IDs) {

    # Load required packages for each core
    require(dplyr, quietly = TRUE)
    require(sf, quietly = TRUE)
    require(sfnetworks, quietly = TRUE)
    require(tidygraph, quietly = TRUE)

    message(which(gs_IDs == ID), " of ", length(gs_IDs))
    # Read green space entries
    gs_query <- paste0("SELECT identifier, area, geom
                       FROM nodes
                       WHERE identifier = \'",
                       ID, "\'")
    gs_entries <- nodes %>%
      st_read(query = gs_query, quiet = TRUE) %>%
      distinct()
    # convert network to service area
    gs_filter <- gs_entries$geom %>%
      st_buffer(d) %>%
      st_union() %>%
      st_as_text()
    # load network
    network <- st_read(edges, wkt_filter = gs_filter, quiet = TRUE)
    # convert to sf_network
    nbh <- network %>%
      remove_overlap() %>%
      mutate(weight = st_length(.)) %>%
      as_sfnetwork() %>%
      convert(to_undirected)
    # 2) COST MATRIX
    #building entries
    b_entries <- nodes %>%
      st_read(query = "SELECT * FROM nodes WHERE population is not null",
              wkt_filter = gs_filter, quiet = TRUE) %>%
      filter(population > 0) %>%
      distinct()
    # OD cost matrix
    if (nrow(b_entries) == 0) message("No buildings in service area.") else {
      od_cost <- nbh %>%
        st_network_cost(x = .,
                        from = b_entries$geom,
                        to = gs_entries$geom) %>%
        apply(., 1, function(x) which(x == min(x))[1])
      # lengths of street segments
      dist_df <- nbh %>%
        activate("edges") %>%
        pull(weight)
      # 3) CALCULATE INDICES
      be_index <- b_entries %>%
        mutate(nearest_entry = gs_entries$geom[od_cost],
               ne_id = od_cost,
               area = gs_entries$area[1]) %>%
        mutate(euklid_dis = round(st_distance(nearest_entry, geom,
                                              by_element = TRUE))) %>%
        arrange(ne_id)
      # check if there are more than 1 park entries
      if (length(unique(be_index$ne_id)) == 2 &
          length(be_index$ne_id[be_index$ne_id == unique(be_index$ne_id)[1]]) == 2 &
          length(be_index$ne_id[be_index$ne_id == unique(be_index$ne_id)[2]]) == 2) {
        be_index <- be_index %>%
          mutate(sPath = unlist(
            list(
            sapply(unique(ne_id), function(i) {
              pull(
                st_network_paths(x = nbh,
                                 from = gs_entries$geom[i],
                                 to = .$geom[.$ne_id == i]),
                edge_paths)
            })
            ), recursive = FALSE))
        # check if there are more than 1 park entries
        } else if (length(unique(be_index$ne_id)) > 1) {
        be_index <- be_index %>%
          mutate(sPath = unlist(
              sapply(unique(ne_id), function(i) {
                pull(
                  st_network_paths(x = nbh,
                                   from = gs_entries$geom[i],
                                   to = .$geom[.$ne_id == i]),
                  edge_paths)
              }), recursive = FALSE))
      } else {
        be_index <- be_index %>%
          mutate(sPath = sapply(unique(ne_id), function(i) {
            pull(
              st_network_paths(x = nbh,
                               from = gs_entries$geom[i],
                               to = .$geom[.$ne_id == i]),
              edge_paths)}
          ))
        }
      # calculate shortest path lengths
      be_index <- be_index %>%
        st_drop_geometry() %>%
        mutate(net_dist = sapply(sPath, function(x) sum(dist_df[x],
                                                        na.rm = TRUE))) %>%
        filter(net_dist < d & net_dist > 0) %>%
        mutate(ls = population * area / net_dist,
               di = euklid_dis / net_dist)
      # check if any building entries were in service area
      if (nrow(be_index) < 1) message("No buildings in service area.") else{
        # output files
        node_out <- paste0(output, "/nodes_", ID, ".csv")
        edge_out <- paste0(output, "/edges_", ID, ".csv")
        # 4) WRITE OUTPUT
        be_index %>%
          select(ID, di) %>%
          filter(!is.infinite(di)) %>%
          mutate(di = round(di, 2)) %>%
          write.csv(node_out, row.names = FALSE)
        # link local significance to edge id
        be_index %>%
          select(sPath, ls) %>%
          mutate(sPath = sapply(sPath, function(x) unlist(x) %>%
                                  paste(collapse = " "))) %>%
          tidyr::separate_rows(sPath, sep = " ") %>%
          group_by(sPath) %>%
          summarise(ls = round(sum(ls, na.rm = TRUE))) %>%
          mutate(sPath = as.numeric(sPath),
                 edge_id = network$edge_id[sPath]) %>%
          na.omit() %>%
          select(-sPath) %>%
          write.csv(edge_out, row.names = FALSE)
      }
    }
    #parallel::stopCluster(cl)
  }
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

  ls_files <- list.files(di_directory,
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
    st_write(ouput_dir)
}


################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################
#e <- edges
#e <- distinct(out)
#e <- st_read("D:/Berlin/indices/edges_76213-DE001L11.gpkg")

remove_overlap <- function(e)
{
  e <- distinct( select(e, geom) )
  eb <- st_buffer(e, 1e-5)
  e$covers <- c(st_covers(eb, e))

  covering <- filter(e, lengths(covers) > 1)
  notCovering <- filter(e, lengths(covers) < 2)

  if (nrow(covering) < 1) return( select(e, geom) )

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

