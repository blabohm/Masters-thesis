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
#4.5.1
s_path1 <- function(build_entries, gs_entries, sf_network)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)
  build_entries %>%
    mutate(sPath = unlist(
      list(
        sapply(unique(ne_id), function(i) {
          pull(
            st_network_paths(x = sf_network,
                             from = gs_entries$geom[i],
                             to = .$geom[.$ne_id == i]),
            edge_paths)
        })
      ), recursive = FALSE)) %>%
    return()
}


#4.5.2
s_path2 <- function(build_entries, gs_entries, sf_network)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)
  build_entries %>%
    mutate(sPath = unlist(
      sapply(unique(ne_id), function(i) {
        pull(
          st_network_paths(x = sf_network,
                           from = gs_entries$geom[i],
                           to = .$geom[.$ne_id == i]),
          edge_paths)
      }), recursive = FALSE))
}


#4.5.3
s_path3 <- function(build_entries, gs_entries, sf_network)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)
  build_entries %>%
    mutate(sPath = sapply(unique(ne_id), function(i) {
      pull(
        st_network_paths(x = sf_network,
                         from = gs_entries$geom[i],
                         to = .$geom[.$ne_id == i]),
        edge_paths)}
    ))
}

