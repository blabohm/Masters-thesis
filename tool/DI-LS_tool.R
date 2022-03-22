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

drive <- "D:/temp/"

################################################################################
# 2.1 - NETWORK CLEANING
#
cityBound <- paste0(drive, "cities.gpkg")
ccList <- cityBound %>%
  st_read(query = "SELECT URAU_CODE FROM cities", quiet = TRUE) %>%
  filter(grepl("001", URAU_CODE))
# URAU city code
cityCode <- "DE001"
#cityCode <- ccList$URAU_CODE[1]
# Input data
netTileDir <- paste0(drive, "osm_paths/")
netDir <- paste0(drive, "network_clean.gpkg")
# Run function
networkPrep(city_code = cityCode,
            network_tile_dir = netTileDir,
            city_boundaries = cityBound,
            output_directory = netDir)


################################################################################
# 2.2 - BUILDING PREPARATION
#
# Input data
osmDir <- paste0(drive, "osm_buildings/")
uaDirectory <- paste0(drive, "UA2018/")
buildOut <- paste0(drive, "buildings.gpkg")
# Run function
buildingPrep(city_code = cityCode,
             osm_directory = osmDir,
             ua_directory = uaDirectory,
             city_boundaries = cityBound,
             output_directory = buildOut)


################################################################################
# 2.3 - GREEN SPACE ENTRY DETECTION
#
gsOut <- paste0(drive, "green_space_entries.gpkg")
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
blendOut <- paste0(drive, "net_blend/")
# Run function
networkBlend(boundary_directory = cityBound,
             network_directory = netDir,
             green_space_directory = gsOut,
             building_directory = buildOut,
             output_directory = blendOut)

################################################################################
################################################################################
# MODULE 3 - INDEX BUILDING
#
################################################################################
# Input data
nodeDir <- paste0(blendOut, "nodes.gpkg")
edgeDir <- paste0(blendOut, "edges.gpkg")
# Run function
getIndices(node_directory = nodeDir,
           edge_directory = edgeDir)

################################################################################
# Clean up

################################################################################
# END OF DOCUMENT

