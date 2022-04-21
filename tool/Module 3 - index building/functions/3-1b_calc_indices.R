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

# 1
# Load required packages for each core
# Get directory of nodes.gpkg

load_gs_entries <- function(ID, folder)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  nodes <- list.files(folder, pattern = "nodes.gpkg", full.names = TRUE)[1]
  gs_query <- paste0("SELECT identifier, area, geom
                     FROM nodes
                     WHERE identifier = \'",
                     ID, "\'")
  gs_entries <- nodes %>%
    read_sf(query = gs_query) %>%
    distinct()
  return(gs_entries)
}

################################################################################
# 2
load_build_entries <- function(folder, gs_entries, d = 500)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  nodes <- list.files(folder, pattern = "nodes.gpkg$", full.names = TRUE)[1]
  gs_filter <- gs_entries$geom %>%
    st_buffer(d) %>%
    st_union() %>%
    st_as_text()
  b_entries <- nodes %>%
    read_sf(query = "SELECT * FROM nodes WHERE population is not null",
            wkt_filter = gs_filter) %>%
    filter(population > 0) %>%
    distinct()
  return(b_entries)
}

################################################################################
# 3
load_network <- function(folder, gs_entries, d = 500)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  edges <- list.files(folder, pattern = "edges.gpkg$", full.names = TRUE)[1]
  gs_filter <- gs_entries$geom %>%
    st_buffer(d) %>%
    st_union() %>%
    st_as_text()
  edges %>%
    st_read(wkt_filter = gs_filter, quiet = TRUE) %>%
    return()
}

################################################################################
# 4
add_params <- function(build_entries, gs_entries, network)
{
  require(dplyr, quietly = TRUE)
  # 4.1
  sf_network <- make_sf_network(network)
  # 4.2
  odc_matrix <- calc_OD_cost(build_entries, gs_entries, sf_network)
  # 4.3
  build_entries %>%
    add_nearest_entry(., gs_entries, odc_matrix) %>%
    # 4.4
    add_euklid_dist(.) %>%
    # 4.5
    add_shortest_path(., gs_entries, sf_network) %>%
    # 4.6
    add_net_dist(., sf_network) %>%
    # 4.7
    add_indices(., sf_network) %>%
    return()
}


################################################################################
# 5
write_output <- function(out, network, out_dir, ID)
{
  if (nrow(out) < 1) message("No buildings in service area.") else{
    #out_dir <- paste0(folder, "/indices/")
    if (!dir.exists(out_dir)) dir.create(out_dir)
    write_di(out, out_dir, ID)
    write_ls(out, network, out_dir, ID)
  }
}

