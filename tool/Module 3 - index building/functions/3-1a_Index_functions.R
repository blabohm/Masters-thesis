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
i <- "76213-DE001L1"
drive <- "D:/"
output_dir <- paste0(drive, "Berlin/indices")
node_directory <- paste0(drive, "Berlin/nodes.gpkg")
edge_directory <- paste0(drive, "Berlin/edges.gpkg")

for (i in gs_IDs) {

  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)
  require(tidygraph, quietly = TRUE)

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

  if (nrow(b_entries) == 0) message("No buildings in service area.") else {
    od_cost <- nbh %>%
      st_network_cost(x = .,
                      from = b_entries$geom,
                      to = gs_entries$geom) %>%
      apply(., 1, function(x) which(x == min(x))[1])

    #if (length(od_cost %>% unique()) < 2) message("Write workaround for single entry!") else {

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
      arrange(ne_id) %>%
      mutate(sPath = sapply(unique(ne_id),
                            function(i) st_network_paths(x = nbh,
                                                         from = gs_entries$geom[i],
                                                         to = .$geom[.$ne_id == i]) %>%
                              pull(edge_paths)) %>%
               unlist(recursive = FALSE)) %>%
      mutate(net_dist = sapply(sPath, function(x) sum(dist_df[x],
                                                      na.rm = TRUE))) %>%
      filter(net_dist < d) %>%
      mutate(ls_temp = population * area / net_dist,
             di_temp = euklid_dis / net_dist,
             di_count_temp = 1)

    # 4) ATTACH INDICES TO OUTPUT SF OBJECTS
    # link local significance to edge id
    ls_df <- be_index %>%
      st_drop_geometry() %>%
      select(sPath, ls_temp) %>%
      mutate(sPath = sapply(sPath, function(x) unlist(x) %>%
                              paste(collapse = " "))) %>%
      tidyr::separate_rows(sPath, sep = " ") %>%
      group_by(sPath) %>%
      summarise(ls_temp = sum(ls_temp, na.rm = TRUE)) %>%
      mutate(sPath = as.numeric(sPath)) %>%
      na.omit() %>%
      transmute(ls = ls_temp, geom = edges$geom[sPath]) %>%
      st_as_sf(crs = 3035)


    node_out <- paste0(output_dir, "/nodes_", i, ".gpkg")
    edge_out <- paste0(output_dir, "/edges_", i, ".gpkg")
    be_index %>%
      select(ID, di = di_temp, di_count = di_count_temp) %>%
      st_write(node_out, quiet = TRUE)
    st_write(ls_df, edge_out, quiet = TRUE)

    #}
  }
}


################################################################################
