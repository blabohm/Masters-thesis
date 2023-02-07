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
inputDir <- "Z:/MA_data/input/"
out <- gsub("input/", "output/", inputDir)
if (!dir.exists(out)) dir.create(out)

require(dplyr, quietly = TRUE)
require(sf, quietly = TRUE)

cityBound <- paste0(inputDir, "cities.gpkg")
# read_sf("Z:/cities_europe_kernel/cities_boundary.shp") %>%
#   select(URAU_CODE = URAU_COD_1,
#          FUA_CODE, SelectBenn) %>%
#   mutate(URAU_CODE = substr(URAU_CODE, 1, 5),
#          FUA_CODE = substr(FUA_CODE, 1, 5)) %>%
#   write_sf(cityBound, layer = "cities")
done <- list.files(out)
ccList <- cityBound %>%
  read_sf(query = "SELECT URAU_CODE, SelectBenn FROM cities") %>%
  arrange(desc(SelectBenn)) %>%
  filter(!(URAU_CODE %in% done))

# cityCode <- ccList$URAU_CODE[1]

# - urban atlas (UA) data of a cities FUA (including population values for
#   residential areas)
# - polygon of the area of interest (if not provided, UA core will be used)
# - URAU code (?)
# URAU city code
# Berlin
#cityCode <- "DE001"
# Leipzig
#cityCode <- "DE008"

for (cityCode in ccList$URAU_CODE) {

  outputDir <- paste0(out, cityCode, "/")
  if (!dir.exists(outputDir)) dir.create(outputDir)
  ################################################################################
  # MODULE 1 - OSM DOWNLOAD
  # Download OpenStreetMap (OSM) data covering the city polygon
  ################################################################################
  getwd() %>%
    paste0("/tool/Module 1 - OSM download/") %>%
    list.files(pattern = "1-[1-9].*\\.R", full.names = TRUE) %>%
    for (file in .) source(file)

  download_OSM(city_code = cityCode,
               input_directory = inputDir)
  ################################################################################
  # MODULE 2 - DATA PREPARATION
  ################################################################################
  getwd() %>%
    paste0("/tool/Module 2 - data preparation/") %>%
    list.files(pattern = "2-[1-9].*\\.R", full.names = TRUE) %>%
    for (file in .) source(file)
  ################################################################################
  # 2.1 - NETWORK CLEANING
  #
  # Run function
  networkPrep(city_code = cityCode,
              input_directory = inputDir,
              output_directory = outputDir)
  ################################################################################
  # 2.2 - BUILDING PREPARATION
  #
  # Run function
  buildingPrep(city_code = cityCode,
               input_directory = inputDir,
               output_directory = outputDir)
  ################################################################################
  # 2.3 - GREEN SPACE ENTRY DETECTION
  #
  # Run function
  greenSpacePrep(city_code = cityCode,
                 input_directory = inputDir,
                 output_directory = outputDir)
  ################################################################################
  # 2.4 - NETWORK BLENDING
  #
  # Run function
  try({networkBlend(city_code = cityCode,
               input_directory = inputDir,
               output_directory = outputDir)})
  ################################################################################
  # MODULE 3 - INDEX BUILDING
  #
  ################################################################################
  # Input data
  getwd() %>%
    paste0("/tool/Module 3 - index building") %>%
    list.files(pattern = "3-[1-9].*\\.R", full.names = TRUE) %>%
    for (file in .) source(file)
  # Run function
  getIndices(working_directory = outputDir)

  ################################################################################
  # Clean up
}
################################################################################
# END OF DOCUMENT

