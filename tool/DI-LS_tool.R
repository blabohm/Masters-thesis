################################################################################
# DETOUR INDEX 6 LOCAL SIGNIFICANCE TOOL
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################
# DESCRIPTION
# This tool is designed to calculate two indices that can be used to assess the
# accessibility of public green spaces (GS) in urban areas. The first index goes
# by the name of detour index (DI) and puts the network distance and the
# euclidean distance from a residential building entry to the nearest GS entries
# into relation. It can assume values between 0 and 1. The closer to 1 the
# value, the more direct the way to the next public green spaces is. If the DI
# is closer to 0, the inhabitants of the building have to take larger detours
# to reach the next GS.
# The local significance index (LS) takes into account the area of a green
# space, the network distance from building entry to GS entry and the
# number of persons inhabiting the building. The LS values are higher, the
# larger the GS area, the higher the number of people accessing it and the
# closer the network distance between building and GS entries.
# Together, these indices can be used to assess where inhabitants of a city have
# barriers on their way to public green spaces, and which GS have a potential to
# be overused.
#
################################################################################
# INPUT DATA
# - urban atlas (UA) data of a cities FUA (including population values for
#   residential areas)
# - polygon of the area of interest (if not provided, UA core will be used)
# - URAU code (?)
################################################################################
# MODULE 1 - OSM DOWNLOAD
# Download OpenStreetMap (OSM) data covering the city polygon
#
################################################################################
# MODULE 2 - DATA PREPARATION

# Load packages and data preparation functions
require(dplyr, quietly = TRUE)
require(sf, quietly = TRUE)
getwd() %>%
  paste0("/tool/Module 2 - data preparation/") %>%
  list.files(pattern = "2-[1-9].*\\.R", full.names = TRUE) %>%
  for (file in .) source(file)


inputDir <- "Z:/_MA/temp/"
################################################################################
# 2.1 - NETWORK CLEANING
#
cityBound <- paste0(inputDir, "cities.gpkg")
ccList <- cityBound %>%
  st_read(query = "SELECT * FROM cities", quiet = TRUE)# %>%
#filter(grepl("001", URAU_CODE))
# URAU city code
# Berlin
#cityCode <- "DE001"
# Leipzig
cityCode <- "DE008"
#cityCode <- ccList$URAU_CODE[1]
outputDir <- gsub("temp/", paste0(cityCode, "/"), inputDir)
if (!dir.exists(outputDir)) dir.create(outputDir)
# Input data
<<<<<<< Updated upstream
netTileDir <- paste0(drive, "/osm_paths/")
netDir <- paste0(drive, "/network_clean.gpkg")
=======
netTileDir <- paste0(inputDir, "/osm_paths/")
netDir <- paste0(outputDir, "/network_clean.gpkg")
>>>>>>> Stashed changes
# Run function
networkPrep(city_code = cityCode,
            network_tile_dir = netTileDir,
            city_boundaries = cityBound,
            output_directory = netDir)


################################################################################
# 2.2 - BUILDING PREPARATION
#
# Input data
<<<<<<< Updated upstream
osmDir <- paste0(drive, "/osm_buildings/")
uaDirectory <- paste0(drive, "/UA2018/")
buildOut <- paste0(drive, "/buildings.gpkg")
=======
osmDir <- paste0(inputDir, "/osm_buildings/")
uaDirectory <- paste0(inputDir, "/UA2018/") # !! make sure this is correct!!
buildOut <- paste0(outputDir, "/buildings.gpkg")
>>>>>>> Stashed changes
# Run function
buildingPrep(city_code = cityCode,
             osm_directory = osmDir,
             ua_directory = uaDirectory,
             city_boundaries = cityBound,
             output_directory = buildOut)


################################################################################
# 2.3 - GREEN SPACE ENTRY DETECTION
#
<<<<<<< Updated upstream
gsOut <- paste0(drive, "/green_space_entries.gpkg")
=======
gsOut <- paste0(outputDir, "/green_space_entries.gpkg")
>>>>>>> Stashed changes
# Run function
greenSpacePrep(city_code = cityCode,
               city_boundaries = cityBound,
               ua_directory = uaDirectory,
               network_directory = netDir,
               output_directory = gsOut)

################################################################################
################################################################################
# 2.4 - NETWORK BLENDING
#
# Input data
<<<<<<< Updated upstream
buildEntries <- paste0(drive, "/building_entries.gpkg")
blendOut <- paste0(drive, "/net_blend/")
# Run function
networkBlend(city_code = city_code,
=======
buildEntries <- paste0(outputDir, "/building_entries.gpkg")
blendOut <- paste0(outputDir, "/net_blend/")
# Run function
networkBlend(city_code = cityCode,
>>>>>>> Stashed changes
             city_boundaries = cityBound,
             network_directory = netDir,
             green_space_directory = gsOut,
             building_directory = buildEntries,
             output_directory = blendOut)

################################################################################
################################################################################
# MODULE 3 - INDEX BUILDING
#
################################################################################
# Input data
<<<<<<< Updated upstream
nodeDir <- paste0(blendOut, "nodes.gpkg")
edgeDir <- paste0(blendOut, "edges.gpkg")
indexDir <- paste0(drive, "/indices/")
# Run function
getIndices(working_directory)
=======
getwd() %>%
  paste0("/tool/Module 3 - index building") %>%
  list.files(pattern = "3-[1-9].*\\.R", full.names = TRUE) %>%
  for (file in .) source(file)
nodeDir <- paste0(outputDir, "nodes.gpkg")
edgeDir <- paste0(outputDir, "edges.gpkg")
indexDir <- paste0(outputDir, "/indices/")
# Run function
getIndices(outputDir)
>>>>>>> Stashed changes

################################################################################
# Clean up

################################################################################
# END OF DOCUMENT

