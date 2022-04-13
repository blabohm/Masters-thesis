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
# drive <- "D:/temp"
# city_boundaries <- paste0(drive, "/cities.gpkg")
# city_code <- "DE001"
# network_directory <- paste0(drive, "/network_clean.gpkg")
# ua_directory <- paste0(drive, "/UA2018")
# output_directory <- paste0(drive, "green_space_entries.gpkg")
################################################################################
greenSpacePrep <- function(city_code, input_directory, output_directory,
                           city_boundaries = paste0(input_directory, "/cities.gpkg"),
                           ua_directory = paste0(input_directory, "/UA2018/"),
                           network_directory = paste0(output_directory, "/network_clean.gpkg"),
                           green_space_out = paste0(output_directory, "/green_space_entries.gpkg")
                           )
{
  # LOAD PACKAGES AND FUNCTIONS
  require(dplyr, quietly = TRUE)
  getwd() %>%
    paste0("/tool/Module 2 - data preparation/functions/") %>%
    list.files(pattern = "2-3_[A-Za-z].*\\.R|2_.*\\.R", full.names = TRUE) %>%
    for (file in .) source(file)

  ################################################################################
  # 1. STEP
  #    -> DESCRIPTION OF STEPS
  # Load city boundaries matching urau code (to avoid matching multiple cities)
  codeListUA <- proximity_checker1(city_boundaries = city_boundaries,
                                   city_code = city_code)

  # Load all urban atlas green spaces in city area + 1 km
  UAgreen_space(code_list = codeListUA,
                ua_directory = ua_directory,
                city_boundaries = city_boundaries,
                city_code = city_code,
                output = "sf") %>%
    # detect entry points
    findGSentries(green_spaces = .,
                  network = network_directory) %>%
    roundGeometry() %>%
    st_write(green_space_out, quiet = TRUE, append = FALSE)
}

# output <- greenSpacePrep(cityBoundary_file = cityBound,
#                                   cityCode = cityCode,
#                                   uaDirectory = uaDirectory,
#                                   network_file = networkFile)
