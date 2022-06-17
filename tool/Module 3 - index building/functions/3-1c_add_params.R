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
# 4.1
make_sf_network <- function(network)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)
  require(tidygraph, quietly = TRUE)
  network %>%
    mutate(weight = st_length(.)) %>%
    as_sfnetwork() %>%
    convert(to_undirected) %>%
    return(.)
}




# 4.2
calc_OD_cost <- function(build_entries, gs_entries, sf_network)
{
  require(dplyr, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)
  sf_network %>%
    st_network_cost(x = .,
                    from = build_entries$geom,
                    to = gs_entries$geom) %>%
    apply(., 1, function(x) which(x == min(x))[1]) %>%
    return()
}


# 4.3
add_nearest_entry <- function(build_entries, gs_entries, odc_matrix)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  build_entries %>%
    mutate(nearest_entry = gs_entries$geom[odc_matrix],
           ne_id = odc_matrix,
           area = gs_entries$area[1]) %>%
    return()
}


# 4.4
add_euklid_dist <- function(build_entries)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  build_entries %>%
    mutate(euklid_dis = st_distance(nearest_entry, geom, by_element = TRUE)) %>%
    arrange(ne_id) %>%
    return()
}

# 4.5
add_shortest_path <- function(build_entries, gs_entries, sf_network)
{
  require(dplyr, quietly = TRUE)
  if (length(unique(build_entries$ne_id)) == 2 &
      length(build_entries$ne_id[build_entries$ne_id ==
                                 unique(build_entries$ne_id)[1]]) == 2 &
      length(build_entries$ne_id[build_entries$ne_id ==
                                 unique(build_entries$ne_id)[2]]) == 2) {
    build_entries %>%
      s_path1(gs_entries, sf_network) %>%
      return()
  } else if (length(unique(build_entries$ne_id)) > 1) {
    build_entries %>%
      s_path2(gs_entries, sf_network) %>%
      return()
  } else {
    build_entries %>%
      s_path3(gs_entries, sf_network) %>%
      return()
  }
}


# 4.6
add_net_dist <- function(build_entries, sf_network, d = 500)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)
  dist_df <- sf_network %>%
    activate("edges") %>%
    pull(weight)
  build_entries %>%
    st_drop_geometry() %>%
    mutate(net_dist = sapply(sPath, function(x) sum(dist_df[x],
                                                    na.rm = TRUE))) %>%
    filter(net_dist < d & net_dist > 0) %>%
    return()
}


# 4.7
add_indices <- function(build_entries, sf_network)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)
  build_entries %>%
    mutate(ls = (population * area) / (net_dist ^ 2),
           di = euklid_dis / net_dist) %>%
    return()
}

