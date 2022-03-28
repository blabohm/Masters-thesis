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
load_gs_entries <- function(ID, folder)
{

}

# 2
load_build_entries <- function(folder, gs_entries)
{

}

# 3
load_network <- function(folder, gs_entries)
{

}

# 4
add_params <- function(build_entries, gs_entries, network)
{

}

# 4.1
make_sf_network <- function(network)
{

}

# 4.2
calc_OD_cost <- function(build_entries, gs_entries, sf_network)
{

}

# 4.3
add_nearest_entry <- function(build_entries, gs_entries, odc_matrix)
{

}

# 4.4
add_euklid_dist <- function(build_entries, gs_entries)
{

}

# 4.5
add_shortest_path <- function(build_entries, gs_entries, sf_network)
{

}

# 4.6
add_net_dist <- function(build_entries, sf_network)
{
  # dist_df
}


# 5
write_output <- function(out, network)
{

}

# 5.1
write_di <- function(out, network, folder)
{

}

# 5.2
write_ls <- function(out, network, folder)
{

}
