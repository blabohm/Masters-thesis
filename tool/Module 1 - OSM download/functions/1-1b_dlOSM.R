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
  if (!curl::has_internet()) assign("has_internet_via_proxy", TRUE,
                                    environment(curl::has_internet))
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
  p <- cleanPolygons(polygons$osm_polygons)
  mp <- cleanPolygons(polygons$osm_multipolygons)
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
  l <- cleanLines(lines$osm_lines)
  ml <- cleanLines(lines$osm_multilines)
  if (is.null(l) & is.null(ml)) return(message("\n Empty tile. Proceeding..."))
  if (is.null(l)) write_sf(ml, OSM_out, append = FALSE) else if (is.null(ml)) {
    write_sf(l, OSM_out, append = FALSE) } else {
      write_sf(bind_rows(l, ml), OSM_out, append = FALSE)}
}
