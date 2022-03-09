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
drive <- "D"
cityBound <- paste0(drive, ":/Berlin/cities.gpkg")
city_code <- "DE001"
beDir <- paste0(drive, ":/Berlin/buildings_cent.gpkg")
gsDir <- paste0(drive, ":/Berlin/green_space_entries2.gpkg")
network <- paste0(drive, ":/Berlin/network_clean1.gpkg")
outDir <- paste0(drive, ":/Berlin/net_blend/")
netDir <- paste0(drive, ":/Berlin/network_clean1.gpkg")

network_blend(boundary_dir = cityBound,
              network_dir = netDir,
              green_space_dir = gsDir,
              build_entry_dir = beDir,
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
  nodes <- list.files(output_dir, pattern = "node", full.names = TRUE)
  edges <- list.files(output_dir, pattern = "edge", full.names = TRUE)
  network_combinator(edges, output_dir = output_dir, out = "sf") %>%
    mutate(edge_id = row_number()) %>%
    st_write(paste0(output_dir, "edges.gpkg", append = FALSE))
  combinator(nodes, output_dir = output_dir)
  unlink(nodes)
  unlink(edges)
}


################################################################################
# END OF DOCUMENT
################################################################################
