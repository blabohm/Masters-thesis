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


################################################################################

mkCityGrid <- function(city_boundary, cellsize = 3000, crs = 3035)
{
  require(sf)
  city_boundary %>%
    st_transform(crs) %>%
    st_make_grid(cellsize = cellsize) %>%
    st_as_sf() %>%
    filter(st_intersects(., city_boundary, sparse = FALSE)) %>%
    rename(geom = x) %>%
    mutate(id = row_number()) %>%
    return()
}


################################################################################
OSM_downloader <- function(tile, key)
{
  require(sf, quietly = TRUE)
  require(osmdata, quietly = TRUE)
  set_overpass_url(APIselect())
  tryCatch({tile %>%
      st_transform(4326) %>%
      st_bbox() %>%
      opq() %>%
      add_osm_feature(key = key) %>%
      osmdata_sf()}, error = function(e) return(NULL)) %>%
    return()
}


################################################################################

OSM_build_writer <- function(polygons, OSM_out)
{
  require(sf, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  p <- tryCatch({select(polygons$osm_polygons, matches("build$")) %>%
      st_cast("MULTIPOLYGON") %>%
      st_cast("POLYGON", do_split = TRUE, warn = FALSE)},
      error = function(e) return(NULL))
  mp <- tryCatch({select(polygons$osm_multipolygons, matches("build$")) %>%
      st_cast("MULTIPOLYGON") %>%
      st_cast("POLYGON", do_split = TRUE, warn = FALSE)},
      error = function(e) return(NULL))
  if (is.null(p) & is.null(mp)) return(message("\n Empty tile. Proceeding..."))
  if (is.null(p)) write_sf(mp, OSM_out, append = FALSE) else if (is.null(mp)) {
    write_sf(p, OSM_out, append = FALSE) } else {
      write_sf(bind_rows(p, mp), OSM_out, append = FALSE)}
}


################################################################################

OSM_network_writer <- function(lines, OSM_out)
{
  require(sf, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  p <- tryCatch({select(lines$osm_lines, matches("highway")) %>%
      st_cast("MULTILINESTRING") %>%
      st_cast("LINESTRING", do_split = TRUE, warn = FALSE)},
      error = function(e) return(NULL))
  mp <- tryCatch({select(lines$osm_multilines, matches("highway")) %>%
      st_cast("MULTILINESTRING") %>%
      st_cast("LINESTRING", do_split = TRUE, warn = FALSE)},
      error = function(e) return(NULL))
  if (is.null(p) & is.null(mp)) return(message("\n Empty tile. Proceeding..."))
  if (is.null(p)) write_sf(mp, OSM_out, append = FALSE) else if (is.null(mp)) {
    write_sf(p, OSM_out, append = FALSE) } else {
      write_sf(bind_rows(p, mp), OSM_out, append = FALSE)}
}
