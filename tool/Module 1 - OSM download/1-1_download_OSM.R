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
getwd() %>%
  paste0("/tool/Module 1 - OSM download/") %>%
  list.files(pattern = "1[A-Za-z].*\\.R", full.names = TRUE) %>%
  for (file in .) source(file)

################################################################################
# INPUT VALUES FOR TESTING CODE

# DATA DIRECTORIES

# FUA CITY CODE


################################################################################
download_OSM <- function(city_code, input_directory, output_directory,
                         city_boundaries = paste0(input_directory, ) )
# 1. STEP
#    -> DESCRIPTION OF STEPS