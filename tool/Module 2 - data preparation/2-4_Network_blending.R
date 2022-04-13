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

networkBlend <- function(city_code, input_directory, output_directory,
                         city_boundaries = paste0(input_directory, "cities.gpkg"),
                         network_directory = paste0(output_directory, "/network_clean.gpkg"),
                         green_space_directory = paste0(output_directory, "/green_space_entries.gpkg"),
                         building_directory = paste0(output_directory, "/buildings.gpkg"),
                         blend_out = paste0(outputDir, "/net_blend/")
                         )
{
  # LOAD PACKAGES AND FUNCTIONS
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  require(sf, quietly = TRUE)
  getwd() %>%
    paste0("/tool/Module 2 - data preparation/functions/") %>%
    list.files(pattern = "2-4[A-Za-z].*\\.R|2_.*\\.R", full.names = TRUE) %>%
    for (file in .) source(file)
  if (!dir.exists(blend_out)) dir.create(blend_out)

  # SNAP AND BLEND BUILDING AND PARK ENTRIES TO NETWORK
  snapAndBlend(city_code = city_code,
               city_boundary = city_boundaries,
               build_entries = building_directory,
               gs_entries = green_space_directory,
               network = network_directory,
               output_dir = blend_out)
  # Combine ouput
  nodes <- list.files(blend_out, pattern = "node", full.names = TRUE)
  edges <- list.files(blend_out, pattern = "edge", full.names = TRUE)
  networkCombinator(file_list = edges, output_dir = blend_out, out = "sf") %>%
    mutate(edge_id = row_number()) %>%
    st_write(paste0(blend_out, "edges.gpkg"),
             append = FALSE, quiet = TRUE)
  combinator(nodes, output_dir = blend_out)
  unlink(nodes)
  unlink(edges)
}


################################################################################
# END OF DOCUMENT
################################################################################
