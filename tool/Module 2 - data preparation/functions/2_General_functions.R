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

boundaryLoader <- function(city_boundaries, city_code,
                           buffer_dist = 0, crs = 3035)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  # Create query
  q <- paste0("SELECT * FROM cities WHERE URAU_CODE LIKE '", city_code, "'")
  # User communication
  message("Load city boundary")
  # Load boundary matching city code
  city_boundaries %>%
    st_read(query = q, quiet = TRUE) %>%
    st_transform(crs) %>%
    st_cast("POLYGON", do_split = TRUE, warn = FALSE) %>%
    st_buffer(buffer_dist) %>%
    return()
}


################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################

roundGeometry <- function(x)
{
  dat <- st_drop_geometry(x)
  st_geometry(x)  %>%
    lapply(function(i) round(i, 0)) %>%
    st_sfc(crs = st_crs(x)) %>%
    st_sf() %>%
    bind_cols(dat) %>%
    rename(geom = geometry) %>%
    return()
}

################################################################################

#4.1.1
remove_overlap <- function(network)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  network <- distinct( select(network, geom) )
  eb <- st_buffer(network, 1e-5)
  network$covers <- c(st_covers(eb, network))
  covering <- filter(network, lengths(covers) > 1)
  notCovering <- filter(network, lengths(covers) < 2)
  if (nrow(covering) < 1) return( select(network, geom) )
  st_difference(st_union(covering), st_union(notCovering)) %>%
    st_cast("MULTILINESTRING", do_split = TRUE, warn = FALSE) %>%
    st_cast("LINESTRING", do_split = TRUE, warn = FALSE) %>%
    st_sf() %>%
    rename(geom = geometry) %>%
    bind_rows(notCovering, .) %>%
    distinct() %>%
    select(geom) %>%
    return()
}
################################################################################

