################################################################################
# MODULE 2 - DATA PREPARATION
# PART 4 - NETWORK BLENDING
# 4c - SNAPPING TOOL
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################
#
# FUNCTIONS:
# 1. st_snap_points
# 2. nearestPointOnLine
# 3. nearestPointOnSegment
#
################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################

st_snap_points = function(points, lines, maxDist = 1000)
{
  point_data <- st_drop_geometry(points)
  d <- st_distance(points, lines)
  distToLine <- apply(d, 1, min, na.rm = TRUE)
  validPoints <- distToLine <= maxDist
  distToPoint <- apply(d, 2, min, na.rm = TRUE)
  validLines <- distToPoint <= maxDist
  points <- points[validPoints, ]
  lines <- lines[validLines, ]
  d <- d[validPoints, validLines, drop = FALSE]
  distToLine <- distToLine[validPoints]
  if (!any(validPoints)) return(message("No points closer than threshold!"))
  nearest_line_index <- apply(d, 1, which.min)
  coordsLines <- st_coordinates(lines)
  coordsPoints <- st_coordinates(points)
  mNewCoords <- vapply(1:nrow(points),
                       function(x) nearestPointOnLine(coordsLines[coordsLines[,3] == nearest_line_index[x],],
                                                      coordsPoints[x, ]), FUN.VALUE = c(0, 0))
  t(mNewCoords) %>%
    as_tibble() %>%
    st_as_sf(coords = c("X", "Y"), crs = 3035) %>%
    bind_cols(point_data[validPoints,]) %>%
    return()
}


################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################

nearestPointOnLine <- function(coordsLine, coordsPoint)
{
  nearest_points <- vapply(2:nrow(coordsLine),
                           function(x) nearestPointOnSegment(coordsLine[(x - 1):x, ],
                                                             coordsPoint),
                           FUN.VALUE = c(0, 0, 0))
  nearest_points[1:2, which.min(nearest_points[3, ])]
}


################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################

nearestPointOnSegment <- function(s, p)
{
  ap = c(p[1] - s[1, 1], p[2] - s[1, 2])
  ab = c(s[2, 1] - s[1, 1], s[2, 2] - s[1, 2])
  t = sum(ap * ab)/sum(ab * ab)
  t = ifelse(t < 0, 0, ifelse(t > 1, 1, t))
  t = ifelse(is.na(t), 0, t)
  x = s[1, 1] + ab[1] * t
  y = s[1, 2] + ab[2] * t
  result = c(x, y, sqrt((x - p[1])^2 + (y - p[2])^2))
  names(result) = c("X", "Y", "distance")
  result
}


################################################################################
# END OF DOCUMENT
################################################################################
