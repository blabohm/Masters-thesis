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

sfc2bb <- function(sfc_object)
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
      sf::st_sf() %>%
      return()
  }


################################################################################
# SNAP POINTS TO LINES - by stackoverflow user Alvin_z
# REQUIRED SETTINGS:
# points: sfc-object with geometry type 'POINT'
# lines: sfc-object with geometry type 'LINESTRING'
# OPTIONAL SETTINGS:
# maxDist: maximum distance
# withAttrs: should attributes be preserved?
# idField: field representing id
################################################################################

snapPointsToLines <-  function(points, lines, maxDist = NA, withAttrs = TRUE, idField = NA)
{
  require(maptools, quietly = TRUE)
  if (rgeosStatus()) {
    if (!requireNamespace("rgeos", quietly = TRUE))
      stop("package rgeos required for snapPointsToLines")
  }
  else stop("rgeos not installed")
  if (is(points, "SpatialPointsDataFrame") == FALSE && missing(withAttrs))
    withAttrs = FALSE
  if (is(points, "SpatialPointsDataFrame") == FALSE && withAttrs == TRUE)
    stop("A SpatialPointsDataFrame object is needed! Please set withAttrs as FALSE.")


  d = rgeos::gDistance(points, lines, byid = TRUE)
  if (!is.na(maxDist)) {
    distToLine <- apply(d, 2, min, na.rm = TRUE)
    validPoints <- distToLine <= maxDist
    distToPoint <- apply(d, 1, min, na.rm = TRUE)
    validLines <- distToPoint <= maxDist
    points <- points[validPoints, ]
    lines = lines[validLines, ]
    d = d[validLines, validPoints, drop = FALSE]
    distToLine <- distToLine[validPoints]
    if (!any(validPoints)) {
      if (is.na(idField)) {
        idCol = character(0)
      }
      else {
        idCol = lines@data[, idField][0]
      }
      newCols = data.frame(nearest_line_id = idCol, snap_dist = numeric(0))
      if (withAttrs)
        df <- cbind(points@data, newCols)
      else df <- newCols
      res <- SpatialPointsDataFrame(points, data = df,
                                    proj4string = CRS(proj4string(points)), match.ID = FALSE)
      return(res)
    }
  }
  else {
    distToLine = apply(d, 2, min, na.rm = TRUE)
  }
  nearest_line_index = apply(d, 2, which.min)
  coordsLines = coordinates(lines)
  coordsPoints = coordinates(points)
  mNewCoords = vapply(1:length(points), function(x) nearestPointOnLine(coordsLines[[nearest_line_index[x]]][[1]],
                                                                       coordsPoints[x, ]), FUN.VALUE = c(0, 0))
  if (!is.na(idField)) {
    nearest_line_id = lines@data[, idField][nearest_line_index]
  }
  else {
    nearest_line_id = sapply(slot(lines, "lines"),
                             function(i) slot(i, "ID"))[nearest_line_index]
  }
  if (withAttrs)
    df = cbind(points@data, data.frame(nearest_line_id, snap_dist = distToLine))
  else df = data.frame(nearest_line_id, snap_dist = distToLine,
                       row.names = names(nearest_line_index))
  SpatialPointsDataFrame(coords = t(mNewCoords), data = df,
                         proj4string = CRS(proj4string(points)))
}


################################################################################
# WRAPPER FOR SNAPPING SF OBJECTS
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################

snapPTLsf <- function(points_sf, lines_sf, crs = 3035)
  {
  # convert points to sp object
  x <-
    points_sf %>%
    st_cast("POINT") %>%
    as_Spatial()

  # convert lines to sp object
  y <-
    lines_sf %>%
    st_cast("LINESTRING") %>%
    as_Spatial()

  output <- snapPointsToLines(points = x, lines = y, withAttrs = TRUE) %>%
    st_as_sf() %>%
    st_transform(crs)

  return(output)
}

################################################################################
# 6. CONVERT TO BUILDING CENTROID
#    -> POINT ON SURFACE
#    -> SNAP BUILDING CENTROIDS TO NETWORK
# 8. OUTPUT TO TEMP
################################################################################

OSMsnap <- function(osm_buildings, network_tile, crs = 3035)
{
  osm_buildings %>%
    st_point_on_surface() %>%
    snapPTLsf(network_tile)
}

