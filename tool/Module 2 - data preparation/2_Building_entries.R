################################################################################
# MODULE 2 - DATA PREPARATION
# PART 2 - BUILDING ENTRIES
################################################################################

# INPUT:
# - OSM BUILDING TILE
# - UA 2018 LAND-USE DATA
# - OSM NETWORK

# OUTPUT:
# - BUILDING ENTRIES (TILE) SNAPPED TO NETWORK

################################################################################
# OVERVIEW:
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
getwd() %>%
  paste0("/tool/Module 2 - data preparation/functions/") %>%
  list.files(pattern = "2[A-Za-z].*\\.R", full.names = TRUE) %>%
  for (file in .) source(file)

################################################################################
# INPUT VALUES FOR TESTING CODE
# DATA DIRECTORIES FOR UA AND OSM DATA
osm_dir <-  "D:/Berlin/osm_buildings_temp/"
ua_dir <- "D:/Berlin/UA2018/"
# FUA CITY CODE
cityCode <- "DE001"
osm_file <- list.files(osm_dir, pattern = cityCode, full.names = TRUE)[12]

################################################################################

# 1. READ UA 2018 DATA
#    -> FILTER FOR RESIDENTIAL AREAS
UAresidential <- UAresLoader(ua_dir)

# 2. READ OSM BUILDING TILE
#    -> CHECK IF OSM LAYER IS INSIDE CITY BOUNDARIES
#    -> UNITE LAYERS IF NECESSARY
OSMbuildings <- osm_file %>%
  OSMloader()# %>%

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