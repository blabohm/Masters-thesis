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
osm_dir <-  "E:/osm_buildings/"
city_bound <- "E:/citiesEurope/Cities.shp"
ua_dir <- "C:/Berlin/UA2018/"
net_dir <- "C:/Berlin/network_clean.gpkg"
# FUA CITY CODE
cityCode <- "DE001"
outDir <- "C:/Berlin/popTiles/"

buildingPrep(osm_directory = osm_dir, ua_directory = ua_dir,
             city_code = cityCode, out_dir = outDir,
             city_boundary = city_bound)
################################################################################

proximity_checker(city_boundary = city_bound,
                  osm_file = "E:/osm_buildings/DE001_16_2.gpkg",
                  city_code = cityCode)

buildingPrep <- function(osm_directory, ua_directory, city_code,
                         out_dir, city_boundaries)
{
  # LOAD PACKAGES AND FUNCTIONS
  require(dplyr, quietly = TRUE)
  getwd() %>%
    paste0("/tool/Module 2 - data preparation/functions/") %>%
    list.files(pattern = "2-2[A-Za-z].*\\.R", full.names = TRUE) %>%
    for (file in .) source(file)
  # Load city boundary
  cityBound <- cityBoundLoader(city_boundaries = city_boundary, city_code = city_code,
                               code_string = "URAU_CO")
  # GET DIRECTORIES OF OSM FILES INSIDE UA BOUNDARY
  osm_file_list <-
    list.files(osm_directory, pattern = city_code, full.names = TRUE) %>%
    proximity_checker(city_boundary = cityBound, city_code = city_code,
                      osm_file = .)
  # 1. READ UA 2018 DATA
  #    -> FILTER FOR RESIDENTIAL AREAS
  UAresidential <- UAresLoader(ua_directory)
  # 2. READ OSM BUILDING TILE
  #    -> CHECK IF OSM LAYER IS INSIDE CITY BOUNDARIES
  #    -> UNITE LAYERS IF NECESSARY
  for(osm_file in osm_file_list$tile_dir) {
    tmpDir <- paste0(out_dir, strsplit(osm_file, "/")[[1]] %>% last())
    osm_file %>%
      OSMloader() %>%
      # 3. FILTER OSM BUILDINGS FOR INSIDE UA RESIDENTIAL POLYGONS
      OSMfilter(ua_residential = UAresidential, city_boundaries = cityBound) %>%
      # 4. ADD POPULATION FROM URBAN ATLAS
      #    -> 1. FROM UA POLYGON
      #    -> 2. FROM AVERAGE POP / AREA IN TILE (IF NO POP INFO AT POLYGON)
      OSMpop() %>%
      # 5. ADD BUILDING ID
      #    -> IDENTIFIER + NUMBER
      OSMbuildID() %>%
      st_write(tmpDir, quiet = TRUE)
    }
  out_dir %>%
    return()
}
