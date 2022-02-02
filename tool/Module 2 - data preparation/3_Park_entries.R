################################################################################
# MODULE 2 - DATA PREPARATION
# PART 3 - PARK ENTRIES
################################################################################

# INPUT:
# - PARK POLYGON (E.G. FROM URBAN ATLAS)
# - NETWORK

# OUTPUT:
# - PARK ENTRIES

################################################################################
# OVERVIEW:
# LOAD PARK POLYGONS
# LOAD NETWORK (IF NECESSARY)
# DETECT PARK ENTRIES
################################################################################

# LOAD PACKAGES AND FUNCTIONS
library(dplyr)
getwd() %>%
  paste0("/tool/Module 2 - data preparation/functions/") %>%
  list.files(pattern = "2-3[A-Za-z].*\\.R", full.names = TRUE) %>%
  for (file in .) source(file)

################################################################################
# INPUT VALUES FOR TESTING CODE

# DATA DIRECTORIES

# FUA CITY CODE


################################################################################

# 1. STEP
#    -> DESCRIPTION OF STEPS