################################################################################
# MODULE 2 - DATA PREPARATION
# PART 3 - NETWORK BLENDING
################################################################################

# INPUT:
# - CLEANED NETWORK (CITY LEVEL)
# - PARK ENTRIES (CITY LEVEL)
# - BUILDING TILES WITH POP DATA AND BUILDING ID
# OUTPUT:
# - BLENDED NETWORK, PARK ENTRIES, BUILDING ENTRIES

################################################################################
# OVERVIEW:
# CONVERT TO BUILDING CENTROID
# READ OSM NETWORK
# OUTPUT TO TEMP
################################################################################
# drive <- "D"
# cityBound <- paste0(drive, ":/Berlin/cities.gpkg")
# city_code <- "DE001"
# beDir <- paste0(drive, ":/Berlin/buildings_cent.gpkg")
# gsDir <- paste0(drive, ":/Berlin/green_space_entries2.gpkg")
# network <- paste0(drive, ":/Berlin/network_clean1.gpkg")
# outDir <- paste0(drive, ":/Berlin/net_blend/")
# netDir <- paste0(drive, ":/Berlin/network_clean1.gpkg")
#
# networkBlend(boundary_directory = cityBound,
#              network_directory = netDir,
#              green_space_directory = gsDir,
#              build_entry_directory = beDir,
#              output_directory = outDir)

networkBlend <- function(city_boundaries,
                         city_code,
                         network_directory,
                         green_space_directory,
                         building_directory,
                         output_directory)
{
  # LOAD PACKAGES AND FUNCTIONS
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  require(sf, quietly = TRUE)
  getwd() %>%
    paste0("/tool/Module 2 - data preparation/functions/") %>%
    list.files(pattern = "2-4[A-Za-z].*\\.R|2_.*\\.R", full.names = TRUE) %>%
    for (file in .) source(file)
  if (!dir.exists(output_directory)) dir.create(output_directory)

  # SNAP AND BLEND BUILDING AND PARK ENTRIES TO NETWORK
  snapAndBlend(city_code = city_code,
               city_boundary = city_boundaries,
               build_entries = building_directory,
               gs_entries = green_space_directory,
               network = network_directory,
               output_dir = output_directory)
  # Combine ouput
  nodes <- list.files(output_directory, pattern = "node", full.names = TRUE)
  edges <- list.files(output_directory, pattern = "edge", full.names = TRUE)
  networkCombinator(file_list = edges, output_dir = output_directory, out = "sf") %>%
    mutate(edge_id = row_number()) %>%
    st_write(paste0(output_directory, "edges.gpkg"),
             append = FALSE, quiet = TRUE)
  combinator(nodes, output_dir = output_directory)
  unlink(nodes)
  unlink(edges)
}


################################################################################
# END OF DOCUMENT
################################################################################
