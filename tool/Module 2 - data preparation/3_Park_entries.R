################################################################################
# MODULE 2 - DATA PREPARATION
# PART 3 - PARK ENTRIES
################################################################################

# INPUT:
# - UA GREEN SPACE LAYER FOR ALL CITIES CREATED IN MODULE 1b
# OR
# - UA FILE DIRECTORY FOR ALL CITIES (EXECUTES MODULE 1b)
# - NETWORK

# OUTPUT:
# - PARK ENTRIES

################################################################################
# OVERVIEW:
# LOAD PARK POLYGONS OR EXECUTE MODULE 1b
# LOAD NETWORK (IF NECESSARY)
# DETECT PARK ENTRIES
################################################################################
# INPUT VALUES FOR TESTING CODE
gsDir <- "E:/"
# DATA DIRECTORIES

# FUA CITY CODE
################################################################################
# LOAD PACKAGES AND FUNCTIONS
library(dplyr)
getwd() %>%
  paste0("/tool/Module 2 - data preparation/functions/") %>%
  list.files(pattern = "2-3[A-Za-z].*\\.R", full.names = TRUE) %>%
  for (file in .) source(file)

################################################################################

# 1. STEP
#    -> DESCRIPTION OF STEPS