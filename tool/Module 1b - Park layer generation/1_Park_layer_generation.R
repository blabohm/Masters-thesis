################################################################################
# MODULE 1a - PARK LAYER GENERATION
################################################################################

# INPUT:
# - DIRECTORY OF URBAN ATLAS GPKG FILES

# OUTPUT:
# - SF OBJECT OR FILE DIRECTORY WITH ALL PARK POLYGONS PRESENT IN UA DIRECTORY

################################################################################
# OVERVIEW:
# 1. DESCRIPTION OF STEPS
################################################################################

# LOAD PACKAGES AND FUNCTIONS
library(dplyr)
getwd() %>%
  paste0("/tool/Module") %>%
  list.files(pattern = "1-1[A-Za-z].*\\.R", full.names = TRUE) %>%
  for (file in .) source(file)

################################################################################
# INPUT VALUES FOR TESTING CODE

# DATA DIRECTORIES

# FUA CITY CODE


################################################################################

# 1. STEP
#    -> DESCRIPTION OF STEPS
