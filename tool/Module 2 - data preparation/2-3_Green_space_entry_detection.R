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
# cityBound <- "E:/citiesEurope/Cities.shp"
# networkFile <- "C:/Berlin/network_clean1.gpkg"
# uaDirectory <- "E:/UA2018"
# # URAU CITY CODE
# cityCode <- "DE001"
# greenSpacePrep(city_code = cityCode,
#                city_boundaries = cityBound,
#                ua_directory = uaDirectory,
#                network_file = networkFile)
################################################################################
greenSpacePrep <- function(city_boundaries,
                           city_code,
                           ua_directory,
                           network_directory,
                           output_directory)
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

  codeListUA <- proximity_checker1(city_boundaries = city_boundaries,
                                  city_code = city_code)

  greenSpaces <- UAgreen_space(code_list = codeListUA,
                               ua_directory = ua_directory,
                               city_boundaries = city_boundaries,
                               city_code = city_code,
                               output = "sf")
  findGSentries(green_spaces = greenSpaces,
                network = network_directory) %>%
    st_write(output_directory, quiet = TRUE)}

# output <- greenSpacePrep(cityBoundary_file = cityBound,
#                                   cityCode = cityCode,
#                                   uaDirectory = uaDirectory,
#                                   network_file = networkFile)
