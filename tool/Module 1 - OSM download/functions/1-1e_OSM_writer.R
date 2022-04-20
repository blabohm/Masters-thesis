cleanPolygons <- function(osm_sf_object)
{
  suppressMessages({
    tryCatch({
      st_crs(osm_sf_object) <- st_crs(4326)
      osm_sf_object %>%
        select(matches("building$")) %>%
        st_cast("MULTIPOLYGON") %>%
        st_cast("POLYGON", do_split = TRUE, warn = FALSE)
    }, error = function(e) return(NULL)) %>%
      return()
  })
}

cleanLines <- function(osm_sf_object)
{
  suppressMessages({
    tryCatch({
      st_crs(osm_sf_object) <- st_crs(4326)
      osm_sf_object %>%
        select(matches("highway")) %>%
        st_cast("MULTILINESTRING") %>%
        st_cast("LINESTRING", do_split = TRUE, warn = FALSE)
    }, error = function(e) return(NULL)) %>%
      return()
  })
}

