################################################################################
# MODULE 1 - OSM DOWNLOAD
################################################################################

# INPUT:
# - INPUT NAME

# OUTPUT:
# - OUTPUT NAME

################################################################################
# OVERVIEW:
# 1. DESCRIPTION OF STEPS
################################################################################

# LOAD PACKAGES AND FUNCTIONS
library(dplyr)

################################################################################
# INPUT VALUES FOR TESTING CODE
city_code = cityCode
input_directory = inputDir
output_directory = outputDir
city_boundaries = paste0(input_directory, "/cities.gpkg")
OSM_out = paste0(input_directory, "/osm_paths/")
# DATA DIRECTORIES

# FUA CITY CODE


################################################################################
download_OSM <- function(city_code, input_directory, output_directory,
                         city_boundaries = paste0(input_directory, "/cities.gpkg"))
{
  # LOAD PACKAGES AND FUNCTIONS
  library(dplyr)
  getwd() %>%
    paste0("/tool/Module 1 - OSM download/functions/") %>%
    list.files(pattern = "1[A-Za-z].*\\.R", full.names = TRUE) %>%
    for (file in .) source(file)

  city_boundary <- boundaryLoader(city_boundaries, city_code, buffer_dist = 1000)
  dlOSM(city_boundary, input_directory)
}

  # 1. STEP
#    -> DESCRIPTION OF STEPS


  ################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################
