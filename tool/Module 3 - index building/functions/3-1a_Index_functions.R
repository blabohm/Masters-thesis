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
i <- "76213-DE001L1" # v-park
i <- "81210-DE001L1" # t-feld
i <- "109682-DE001L1"
drive <- "D:/"
output_dir <- paste0(drive, "Berlin/indices")
node_directory <- paste0(drive, "Berlin/nodes.gpkg")
edge_directory <- paste0(drive, "Berlin/edges.gpkg")


calcIndices <- function(green_space_IDs,
                        node_directory, edge_directory, output_directory)
{
  #  for (i in gs_IDs) {
  # Set up parallel processing
  require(doParallel)
  ncore <- round(detectCores() * .75)
  cl <- makeCluster(ncore, outfile = "")
  registerDoParallel(cl)
  foreach(i = gs_IDs) %dopar% {

    require(dplyr, quietly = TRUE)
    require(sf, quietly = TRUE)
    require(sfnetworks, quietly = TRUE)
    require(tidygraph, quietly = TRUE)

    message(i)
    # Read green space entries
    gs_query <- paste0("SELECT identifier, area, geom
                     FROM nodes
                     WHERE identifier = \'",
                     i, "\'")
    gs_entries <- node_directory %>%
      st_read(query = gs_query, quiet = TRUE) %>%
      distinct()
    # Distance
    d <- 500
    # convert network to service area
    gs_filter <- gs_entries$geom %>%
      st_buffer(d) %>%
      st_union() %>%
      st_as_text()
    # load network
    edges <- edge_directory %>%
      st_read(wkt_filter = gs_filter, quiet = TRUE) %>%
      mutate(weight = st_length(.))
    # convert to st_network
    nbh <- edges %>%
      as_sfnetwork() %>%
      convert(to_undirected)
    # 2) COST MATRIX
    #building entries
    b_entries <- node_directory %>%
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
      if (length(unique(be_index$ne_id)) > 1) {
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
          ))}
      # calculate shortest path lengths
      be_index <- be_index %>%
        st_drop_geometry() %>%
        mutate(net_dist = sapply(sPath, function(x) sum(dist_df[x],
                                                        na.rm = TRUE))) %>%
        filter(net_dist < d) %>%
        mutate(ls_temp = population * area / net_dist,
               di_temp = euklid_dis / net_dist,
               di_count_temp = 1)
      # check if any building entries were in service area
      if (nrow(be_index) < 1) message("No buildings in service area.") else{

        # output files
        node_out <- paste0(output_dir, "/nodes_", i, ".csv")
        edge_out <- paste0(output_dir, "/edges_", i, ".csv")
        # 4) WRITE OUTPUT
        be_index %>%
          select(ID, di = di_temp, di_count = di_count_temp) %>%
          filter(!is.infinite(di)) %>%
          mutate(di = round(di, 2)) %>%
          write.csv(node_out, row.names = FALSE)
        # link local significance to edge id
        be_index %>%
          select(sPath, ls_temp) %>%
          mutate(sPath = sapply(sPath, function(x) unlist(x) %>%
                                  paste(collapse = " "))) %>%
          tidyr::separate_rows(sPath, sep = " ") %>%
          group_by(sPath) %>%
          summarise(ls = round(sum(ls_temp, na.rm = TRUE))) %>%
          mutate(sPath = as.numeric(sPath),
                 edge_id = edges$edge_id[sPath]) %>%
          na.omit() %>%
          select(-sPath) %>%
          write.csv(edge_out, row.names = FALSE)
      }
    }
    parallel::stopCluster(cl)
  }
}


################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################
# GET BBOX

sfc2bb <- function(sfc_object, crs = 3035)
{
  require(sf)
  require(dplyr)
  bb <- st_bbox(sfc_object)

  list(rbind(c(bb$xmin, bb$ymin),
             c(bb$xmax, bb$ymin),
             c(bb$xmax, bb$ymax),
             c(bb$xmin, bb$ymax),
             c(bb$xmin, bb$ymin))) %>%
    sf::st_polygon() %>%
    sf::st_sfc() %>%
    sf::st_sf(crs = crs) %>%
    return()
}


################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################
building_polygons <- "D:/Berlin/buildings.gpkg"
di_directory <- "D:/Berlin/indices/"
di_files <- list.files(di_directory,
                       pattern = "nodes[1-9]*.*csv$",
                       full.names = TRUE)

# gs_IDs <- list.files(di_directory, pattern = "nodes[1-9]*.*gpkg$") %>%
#   gsub("nodes_|.gpkg", "", .) %>%
#   tibble(ID = .)
#
# st_write(gs_IDs, building_polygons, layer = "IDs", append = TRUE)

i <- di_files[1]

tmpDf <- NULL

for (i in di_files) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  di_values <- read.csv(i)
  if (nrow(di_values) > 0) if (unique(di_values$di) != Inf) {
    if (!is.null(tmpDf)) tmpDf <- bind_rows(tmpDf, di_values) else tmpDf <- di_values
  }
}
di_out <-
  tmpDf %>%
  group_by(ID) %>%
  summarise(di = mean(di))

bPoly <- st_read(building_polygons, quiet = TRUE)

bPoly1 <- left_join(bPoly, di_out)

st_write(bPoly1, "D:/Berlin/node_out_test.gpkg")
################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################
edges <- "D:/Berlin/edges.gpkg"
ls_directory <- "D:/Berlin/indices/"
ls_files <- list.files(di_directory,
                       pattern = "edges[1-9]*.*csv$",
                       full.names = TRUE)

# gs_IDs <- list.files(di_directory, pattern = "nodes[1-9]*.*gpkg$") %>%
#   gsub("nodes_|.gpkg", "", .) %>%
#   tibble(ID = .)
#
# st_write(gs_IDs, building_polygons, layer = "IDs", append = TRUE)

i <- di_files[1]

tmpDf <- NULL

for (i in ls_files) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  ls_values <- read.csv(i)
  if (nrow(ls_values) > 0) if (unique(ls_values$ls) != Inf) {
    if (!is.null(tmpDf)) tmpDf <- bind_rows(tmpDf, ls_values) else tmpDf <- ls_values
  }
}
ls_out <-
  tmpDf %>%
  group_by(edge_id) %>%
  summarise(ls = mean(ls))

ls_edges <- st_read(edges, quiet = TRUE)

ls_edges1 <- left_join(ls_edges, ls_out)

st_write(ls_edges1, "D:/Berlin/edge_out_test.gpkg")

################################################################################

