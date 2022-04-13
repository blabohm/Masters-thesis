################################################################################
# MODULE 'NUMBER' - MODULE NAME
# PART 'NUMBER' - SUBMODULE NAME
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

################################################################################
# INPUT VALUES FOR TESTING CODE

# DATA DIRECTORIES

# FUA CITY CODE


################################################################################

# 1. STEP
#    -> DESCRIPTION OF STEPS
boundaryLoader <- function(city_boundaries, buffer_dist = 0, crs = 3035)
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
    st_transform(3035) %>%
    st_cast("POLYGON", do_split = TRUE, warn = FALSE) %>%
    st_buffer(buffer_dist) %>%
    return()
}


################################################################################

dlOSM <- function(city_boundary, OSM_out)
{
  city_grid <- mkCityGrid(city_boundary)
  build_out <- paste0(OSM_out, "osm_buildings/")
  if (!dir.exists(build_out)) dir.create(build_out)
  net_out <- paste0(OSM_out, "osm_network/")
  if (!dir.exists(net_out)) dir.create(net_out)
  for (i in 1:nrow(city_grid)) {
    polygons <- OSM_downloader(city_grid$geom[i], key = "building")
    dsn1 <- paste0(build_out, city_boundary$URAU_CODE, "_", city_grid$id[i], ".gpkg")
    if (!is.null(polygons)) OSM_build_writer(polygons, dsn1)

    dsn2 <- paste0(net_out, city_boundary$URAU_CODE, "_", city_grid$id[i], ".gpkg")
    lines <- OSM_downloader(city_grid$geom[i], key = "highway")
    if (!is.null(lines)) OSM_network_writer(lines, dsn2)
  }
}


