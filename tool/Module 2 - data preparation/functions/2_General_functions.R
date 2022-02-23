################################################################################
# MODULE 2 - DATA PREPARATION
# PART 4 - NETWORK BLENDING
# 4c - OPERATOR FUNCTIONS
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################
#
# FUNCTIONS:
# 1. sfc2bb
#    -> Return a boundary box polygon for intersection
# 2. snapPointsToLines
#    -> Snap points to lines while preserving attributes
# 3. snapPTLsf
#    -> wrapper around snapPointsToLines for sf objects
################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################
# GET BBOX

sfc2bb <- function(sfc_object, crs = 3035)
{
  require(sf)
  require(dplyr)
  bb <- st_bbox(sfc_object)

  list(rbind(c(bb$xmin, bb$ymin),
             c(bb$xmax, bb$ymin),
             c(bb$xmax, bb$ymax),
             c(bb$xmin, bb$ymax),
             c(bb$xmin, bb$ymin))) %>%
    sf::st_polygon() %>%
    sf::st_sfc() %>%
    sf::st_sf(crs = crs) %>%
    return()
}


################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################

boundaryLoader <- function(city_boundaries, city_code = NULL, buffer_dist = 0,
                            code_string = "FUA_CO", crs = 3035)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  if (class(city_boundaries) == "character")  {
    cityBound <- st_read(city_boundaries, quiet = TRUE) } else {
      cityBound <- city_boundaries }
  # User communication
  message("Load city boundary")

  cityBound <- cityBound %>%
    select(code = matches(code_string)) %>%
    mutate(code = substr(code, 1, 5)) %>%
    st_transform(crs) %>%
    st_cast("POLYGON", do_split = TRUE, warn = FALSE)

  if (is.null(city_code)) return(cityBound) else {
    cityBound %>%
      filter(code %in% city_code) %>%
      st_buffer(buffer_dist) %>%
      return() }
}

