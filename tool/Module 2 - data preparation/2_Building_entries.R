################################################################################
# MODULE 2 - DATA PREPARATION
# PART 2 - BUILDING ENTRIES
################################################################################

# INPUT:
# - OSM BUILDING TILE
# - UA 2018
# - OSM NETWORK

# OUTPUT:
# - BUILDING ENTRIES (TILE) SNAPPED TO NETWORK

################################################################################
# STEPS:
# 1. READ UA 2018 DATA
# 2. READ OSM BUILDING TILE
# 3. FILTER OSM BUILDINGS FOR INSIDE UA RESIDENTIAL POLYGONS
# 4. ADD POPULATION FROM URBAN ATLAS
# 5. ADD BUILDING ID
# 6. CONVERT TO BUILDING CENTROID
# 7. READ OSM NETWORK
# 8. OUTPUT TO TEMP
################################################################################

# LOAD PACKAGES AND FUNCTIONS
library(dplyr)
source()

################################################################################

# 1. READ UA 2018 DATA
#    -> FILTER FOR RESIDENTIAL AREAS
UAresidential <- loadUAres()

# 2. READ OSM BUILDING TILE
#    -> UNITE LAYERS IF NECESSARY
OSMbuildings <-
  OSMloader(osm_file = osm_directory) %>%

# 3. FILTER OSM BUILDINGS FOR INSIDE UA RESIDENTIAL POLYGONS
  OSMfilter(osm_buildings = ., ua_res = UAresidential)

# 4. ADD POPULATION FROM URBAN ATLAS
#    -> 1. FROM UA POLYGON
#    -> 2. FROM AVERAGE POP / AREA IN TILE (IF NO POP INFO AT POLYGON)

# 5. ADD BUILDING ID
#    -> LETTER FOR TILE NUMBER + NROW()
# 6. CONVERT TO BUILDING CENTROID
#    -> POINT ON SURFACE
# 7. READ OSM NETWORK
#    -> SNAP BUILDING CENTROIDS TO NETWORK
# 8. OUTPUT TO TEMP