################################################################################
# MODULE 2 - DATA PREPARATION
# PART 3 - PARK ENTRIES
################################################################################

# INPUT:
# - UA BOUNDARY BOX LAYER FOR ALL CITIES CREATED IN MODULE 1b
# OR
# - UA FILE DIRECTORY FOR ALL CITIES (EXECUTES MODULE 1b)
# - NETWORK

# OUTPUT:
# - PARK ENTRIES

################################################################################
# OVERVIEW:
# LOAD CITY POLYGON LAYER
#   -> Check for proximity to other cities
#   -> Load green spaces of resulting cities
# LOAD NETWORK (IF NECESSARY)
# DETECT PARK ENTRIES
################################################################################
# INPUT VALUES FOR TESTING CODE
# DATA DIRECTORIES
cityBoundary_file <- "E:/citiesEurope/Cities.shp"
network_file <- "C:/Berlin/network_clean1.gpkg"
uaDirectory <- "E:/UA2018"
# FUA CITY CODE
cityCode <- "DE001"

################################################################################
greenSpacePrep <- function(cityBoundary_file, cityCode,
                                    uaDirectory, network_file)
{
  # LOAD PACKAGES AND FUNCTIONS
  require(dplyr, quietly = TRUE)
  getwd() %>%
    paste0("/tool/Module 2 - data preparation/functions/") %>%
    list.files(pattern = "2-3_[A-Za-z].*\\.R", full.names = TRUE) %>%
    for (file in .) source(file)

  ################################################################################
  # 1. STEP
  #    -> DESCRIPTION OF STEPS
  # Load city boundaries matching urau code (to avoid matching multiple cities)
  # User communication

  codeListUA <- proximity_checker(city_boundaries = cityBoundary_file,
                                  city_code = cityCode)

  greenSpaces <- UAgreen_space(code_list = codeListUA,
                               ua_directory = uaDirectory,
                               city_boundaries = cityBoundary_file,
                               city_code = cityCode,
                               output = "sf")
  findGSentries(green_spaces = greenSpaces,
                network = network_file) %>%
    return()
}

# output <- greenSpacePrep(cityBoundary_file = cityBoundary_file,
#                                   cityCode = cityCode,
#                                   uaDirectory = uaDirectory,
#                                   network_file = network_file)
