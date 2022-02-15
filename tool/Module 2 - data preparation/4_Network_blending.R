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
net_dir <- "D:/Berlin/network_clean1.gpkg"
gs_dir <- "D:/Berlin/green_space_entries.gpkg"
be_dir <- "D:/Berlin/popTiles/"
tmpDir <- "D:/Berlin/popTilesSnapped/"
# LOAD PACKAGES AND FUNCTIONS
require(dplyr, quietly = TRUE)
require(sf, quietly = TRUE)
getwd() %>%
  paste0("/tool/Module 2 - data preparation/functions/") %>%
  list.files(pattern = "2-4[A-Za-z].*\\.R", full.names = TRUE) %>%
  for (file in .) source(file)
# READ OSM NETWORK
OSMnetwork <- st_read(net_dir, quiet = TRUE)
gsEntries <- st_read(gs_dir, quiet = TRUE)


# CONVERT TO BUILDING CENTROID AND SNAP TO NEAREST NETWORK LINES
#    -> POINT ON SURFACE
#    -> SNAP BUILDING CENTROIDS TO NETWORK
# OUTPUT TO TEMP
be_tiles <- list.files(be_dir, pattern = ".gpkg$", full.names = TRUE)
for (tile in be_tiles) {
  inTile <- tile %>%
    st_read(quiet = TRUE)
  bbox <- sfc2bb(inTile) %>%
    st_buffer(100)
  net_tile <- OSMnetwork %>%
    st_filter(bbox, .pred = st_intersects)

  st_snap_points(inTile, net_tile, max_dist = 200)

  #snapPTLsf(points_sf = inTile, lines_sf = net_tile, max_distance = 200) %>%
  #  st_write(tmpDir, quiet = TRUE) #16:12
}
