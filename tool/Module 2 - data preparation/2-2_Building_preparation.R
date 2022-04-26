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
# city_code = cityCode
# input_directory = inputDir
# output_directory = outputDir
# city_boundaries = paste0(input_directory, "/cities.gpkg")
# osm_directory = paste0(input_directory, "/osm_buildings/")
# ua_directory = paste0(input_directory, "/UA2018/")
# building_out = paste0(output_directory, "/buildings.gpkg")

################################################################################

buildingPrep <- function(city_code, input_directory, output_directory,
                         city_boundaries = paste0(input_directory, "/cities.gpkg"),
                         osm_directory = paste0(input_directory, "/osm_buildings/"),
                         ua_directory = paste0(input_directory, "/UA2018/"),
                         building_out = paste0(output_directory, "/buildings.gpkg")
                         )
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
    list.files(pattern = city_code, full.names = TRUE)
  # 1. READ UA 2018 DATA
  #    -> FILTER FOR RESIDENTIAL AREAS
  UAresidential <- UAresLoader(ua_dir = ua_directory,
                               fua_code = city_boundary$FUA_CODE,
                               boundary = city_boundary)
  # 2. READ OSM BUILDING TILE
  #    -> CHECK IF OSM LAYER IS INSIDE CITY BOUNDARIES
  #    -> UNITE LAYERS IF NECESSARY
  for (osm_file in osm_file_list) {
    osmTmp <- osm_file %>%
      OSMloader(boundary = city_boundary)
    if (!is.null(osmTmp)) if (nrow(osmTmp) > 0) osmTmp %>%
      # 3. FILTER OSM BUILDINGS FOR INSIDE UA RESIDENTIAL POLYGONS
      OSMfilter(ua_residential = UAresidential) %>%
      # 4. ADD POPULATION FROM URBAN ATLAS
      #    -> 1. FROM UA POLYGON
      #    -> 2. FROM AVERAGE POP / AREA IN TILE (IF NO POP INFO AT POLYGON)
      OSMpop() %>%
      # 5. ADD BUILDING ID
      #    -> IDENTIFIER + NUMBER
      OSMbuildID() %>%
      st_write(building_out, layer = "osm_buildings", quiet = TRUE, append = TRUE)
  }
  # Building entries
  building_out %>%
    st_read(quiet = TRUE) %>%
    st_point_on_surface() %>%
    roundGeometry() %>%
    st_write(gsub("buildings", "building_entries", building_out),
             quiet = TRUE, append = FALSE)
}
