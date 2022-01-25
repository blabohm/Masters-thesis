################################################################################
# MODULE 2 - DATA PREPARATION
# PART 2 - BUILDING ENTRIES
# LOADER FUNCTIONS
################################################################################

# INPUT:
# - OSM BUILDING TILE
# - UA 2018
# - OSM NETWORK

# OUTPUT:
# - BUILDING ENTRIES (TILE) SNAPPED TO NETWORK

################################################################################
# STEPS:
# 1. READ OSM BUILDING TILE
#    -> UNITE LAYERS IF NECESSARY
# 2. READ UA 2018 DATA
#    -> FILTER FOR RESIDENTIAL AREAS
# 3. ADD POPULATION FROM URBAN ATLAS
#    -> 1. FROM UA POLYGON
#    -> 2. FROM AVERAGE POP / AREA IN TILE (IF NO POP INFO AT POLYGON)
# 4. ADD BUILDING ID
#    -> LETTER FOR TILE NUMBER + NROW()
# 5. CONVERT TO BUILDING CENTROID
#    -> POINT ON SURFACE
# 6. READ OSM NETWORK
#    -> SNAP BUILDING CENTROID TO NETWORK
################################################################################

# LOAD PACKAGES AND FUNCTIONS