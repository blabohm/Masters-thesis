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
netDir <- "C:/Berlin/network_clean1.gpkg"
gsDir <- "C:/Berlin/green_space_entries.gpkg"
beDir <- "C:/Berlin/buildings.gpkg"
outDir <- "C:/Berlin/net_blend/"
cityBound <- "C:/Berlin/cities.gpkg"
city_code <- "DE001"

network_blend(network_dir = netDir,
              green_space_dir = gsDir,
              build_entry_dir = beDir,
              city_boundaries = cityBound,
              output_dir = outDir)

network_blend <- function(boundary_dir, network_dir, green_space_dir, build_entry_dir,
                          output_dir)
{
  # LOAD PACKAGES AND FUNCTIONS
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  require(sf, quietly = TRUE)
  getwd() %>%
    paste0("/tool/Module 2 - data preparation/functions/") %>%
    list.files(pattern = "2-4[A-Za-z].*\\.R|2_.*\\.R", full.names = TRUE) %>%
    for (file in .) source(file)
  if (!dir.exists(output_dir)) dir.create(output_dir)
  # SNAP AND BLEND BUILDING AND PARK ENTRIES TO NETWORK
  snapAndBlend(city_boundary = boundary_dir,
               build_entries = build_entry_dir,
               gs_entries = green_space_dir,
               network = network_dir,
               output_dir = outDir)
  network_combinator(edges)
  combinator(nodes)
}

