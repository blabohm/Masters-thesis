################################################################################
# MODULE 'NUMBER' - MODULE NAME
# PART 'NUMBER' - SUBMODULE NAME
# PART 'NUMBER' + letter - FUNCTIONS THEME
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################
#
# FUNCTIONS:
# 1. FUNCTION NAME
#    -> Function description
#
################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################
city_boundaries <- "E:/citiesEurope/Cities.shp"
city_code <- "DE001"

proximity_checker <- function(city_boundaries, city_code, crs = 3035)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  if (class(city_boundaries) == "character")  {
    cityBound <- st_read(city_boundaries, quiet = TRUE) } else {
      cityBound <- city_boundaries }

  cityBound <- select(cityBound, code = matches("FUA_CO"))

  c <- cityBound %>%
    filter(substr(code, 1, 5) %in% city_code) %>%
    st_buffer(1000)
cityBound %>%
  st_filter(c, .pred = st_intersects) %>%
  pull(code) %>%
  substr(1, 5) %>%
  unique() %>%
  return()
}


################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################

code_list <- proximity_checker(city_boundaries, city_code)
ua_directory <- "E:/UA2018"

UAgreen_space <- function(code_list, ua_directory, output = "directory")
{
  # LOAD PACKAGES AND FUNCTIONS
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  # green space classes
  greenSpaces <- c(14100, 30000, 31000)
  # list files in urban atlas directory
  p <- paste(code_list, collapse = "|")
  ua <- ua_directory %>%
    list.files(pattern = p, full.names = TRUE) %>%
    list.files(pattern = ".gpkg$",
               full.names = TRUE,
               recursive = TRUE)
  # generate output layer and file names
  outLayer <- paste0("green_spaces")
  gsDsn <- paste0(tempdir(), "\\", outLayer, ".gpkg")
  #set up progress bar
  pb <- txtProgressBar(min = 0, max = length(ua),
                       initial = 0, style = 3)
  stepi <- 0
  # interate through files
  for (uaFile in ua) {
    # for progress bar
    stepi <- stepi + 1
    setTxtProgressBar(pb, stepi)
    # get name of land use layer
    lyr <- st_layers(uaFile)$name[1]
    # write parks to one layer
    st_read(uaFile, layer = lyr, quiet = TRUE) %>%
      select(city_code = contains("FUA_OR") | contains("fua_code"),
             class = contains("code") & contains("20"),
             contains("identifier"),
             contains("area")) %>%
      filter(class %in% greenSpaces) %>%
      st_write(dsn = gsDsn, layer = outLayer,
               append = TRUE, quiet = TRUE) }
  if (grepl("dir", output, ignore.case = TRUE)) return(gsDsn) else gsDsn %>%
    st_read(quiet = TRUE) %>%
    return()
}


################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################
