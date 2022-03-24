################################################################################
# MODULE 2 - DATA PREPARATION
# PART 2 - BUILDING PREPARATION
################################################################################
# INPUT:
# - OSM BUILDING TILE
# - UA 2018 LAND-USE DATA
# - OSM NETWORK
#
# OUTPUT:
# - BUILDINGS POLYGONS (TILE)
################################################################################
# OVERVIEW:
# 1. READ UA 2018 DATA
# 2. READ OSM BUILDING TILE
# 3. FILTER OSM BUILDINGS FOR INSIDE UA RESIDENTIAL POLYGONS
# 4. ADD POPULATION FROM URBAN ATLAS
# 5. ADD BUILDING ID
#
################################################################################
# INPUT VALUES FOR TESTING CODE
# DATA DIRECTORIES FOR UA AND OSM DATA
# osmDir <-  "E:/osm_buildings/"
# uaDir <- "C:/Berlin/UA2018/"
# cityCode <- "DE001"
# cityBound <- "E:/citiesEurope/Cities.shp"
# outDir <- "C:/Berlin/buildings.gpkg"
#
# buildingPrep(osm_directory = osmDir,
#              ua_directory = uaDir,
#              city_code = cityCode,
#              city_boundaries = cityBound,
#              out_dir = outDir)
################################################################################

buildingPrep <- function(city_boundaries,
                         city_code,
                         ua_directory,
                         osm_directory,
                         output_directory)
{
  # LOAD PACKAGES AND FUNCTIONS
  require(dplyr, quietly = TRUE)
  getwd() %>%
    paste0("/tool/Module 2 - data preparation/functions/") %>%
    list.files(pattern = "2-2[A-Za-z].*\\.R|2_.*\\.R", full.names = TRUE) %>%
    for (file in .) source(file)
  # Load city boundary
  city_boundary <- city_boundaries %>%
    boundaryLoader(city_boundaries = .,
                   city_code = city_code)
  # GET DIRECTORIES OF OSM FILES INSIDE UA BOUNDARY
  osm_file_list <- osm_directory %>%
    list.files(pattern = city_code, full.names = TRUE) #%>%
    #proximity_checker(city_boundary = city_boundary,
    #                  osm_file = .)
  # 1. READ UA 2018 DATA
  #    -> FILTER FOR RESIDENTIAL AREAS
  UAresidential <- UAresLoader(ua_dir = ua_directory,
                               city_code = city_code,
                               boundary = city_boundary)
  # 2. READ OSM BUILDING TILE
  #    -> CHECK IF OSM LAYER IS INSIDE CITY BOUNDARIES
  #    -> UNITE LAYERS IF NECESSARY
  for (osm_file in osm_file_list) {
    osmTmp <- osm_file %>%
      OSMloader(boundary = city_boundary)
    if (nrow(osmTmp) > 0) osmTmp %>%
      # 3. FILTER OSM BUILDINGS FOR INSIDE UA RESIDENTIAL POLYGONS
      OSMfilter(ua_residential = UAresidential) %>%
      # 4. ADD POPULATION FROM URBAN ATLAS
      #    -> 1. FROM UA POLYGON
      #    -> 2. FROM AVERAGE POP / AREA IN TILE (IF NO POP INFO AT POLYGON)
      OSMpop() %>%
      # 5. ADD BUILDING ID
      #    -> IDENTIFIER + NUMBER
      OSMbuildID() %>%
      st_write(output_directory, layer = "osm_buildings", quiet = TRUE, append = TRUE)
  }
  # Building entries
  output_directory %>%
    st_read(quiet = TRUE) %>%
    st_point_on_surface() %>%
    roundGeometry() %>%
    st_write(gsub("buildings", "building_entries", output_directory),
             quiet = TRUE, append = FALSE)
}
