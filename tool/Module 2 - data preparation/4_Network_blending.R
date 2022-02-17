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
beDir <- "D:/Berlin/buildings.gpkg"
outDir <- "C:/Berlin/net_blend/"
cityBound <- "E:/citiesEurope/Cities.shp"
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
# READ OSM NETWORK
#OSMnetwork <- st_read(network_dir, quiet = TRUE)
#gsEntries <- st_read(green_space_dir, quiet = TRUE)
#bEntries <- build_entry_dir %>%
#  st_read(quiet = TRUE) %>%
#  st_point_on_surface()
#city_boundary <- boundaryLoader(city_boundaries = city_boundaries,
#                                city_code = city_code, code_string = "URAU_CO")
# SNAP AND BLEND BUILDING AND PARK ENTRIES TO NETWORK
snapAndBlend(city_boundary = boundary_dir,
             build_entries = build_entry_dir,
             gs_entries = green_space_dir,
             network = network_dir,
             output_dir = outDir) # 65
}

