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
# WRAPPER FOR SNAPPING SF OBJECTS
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
# WRAPPER FOR SNAPPING SF OBJECTS
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################

nearestPointOnLine <- function(coordsLine, coordsPoint)
{
  nearest_points <- vapply(2:nrow(coordsLine), function(x) nearestPointOnSegment(coordsLine[(x - 1):x, ],
                                                                                 coordsPoint), FUN.VALUE = c(0, 0, 0))
  nearest_points[1:2, which.min(nearest_points[3, ])]
}


################################################################################
# WRAPPER FOR SNAPPING SF OBJECTS
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
# 6. CONVERT TO BUILDING CENTROID
#    -> POINT ON SURFACE
#    -> SNAP BUILDING CENTROIDS TO NETWORK
# 8. OUTPUT TO TEMP
################################################################################
#out_dir <- "C:/Berlin/tiles/"
city_boundary <- "D:/Berlin/cities.gpkg"
city_code <- "DE001"
build_entries <- "D:/Berlin/buildings_cent.gpkg"
gs_entries <- "D:/Berlin/green_space_entries2.gpkg"
network <- "D:/Berlin/network_clean1.gpkg"
output_dir <- "D:/Berlin/net_blend/"
cellsize = 1000
crs = 3035

snapAndBlend <- function(city_boundary, city_code, build_entries, gs_entries, network,
                         output_dir, cellsize = 3000, crs = 3035)
{
  # Load required packages
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)
  require(doParallel, quietly = TRUE)
  # require(igraph)
  # require(tidygraph)

  bound_query <- paste0('SELECT * FROM \'cities\' WHERE URAU_CODE = \'',
                        city_code, '\'')
  city_boundary <- city_boundary %>%
    st_read(query = bound_query, quiet = TRUE) %>%
    st_transform(crs)
  # Tile boundaries
  cityGrid <- city_boundary %>%
    st_buffer(1000) %>%
    st_make_grid(cellsize = cellsize) %>%
    st_as_sf() %>%
    st_filter(city_boundary, .pred = st_intersects)

  #ncore <- 2
  #cl <- makeCluster(ncore, outfile = "")
  #registerDoParallel(cl)
  # Iterate through city tiles
  for (i in 725:nrow(cityGrid)) { # 13:05
    #foreach(i = 1:nrow(cityGrid), .combine = rbind) %dopar% ({
    require(dplyr)
    getwd() %>%
      paste0("/tool/Module 2 - data preparation/functions/") %>%
      list.files(pattern = "2-4[A-Za-z].*\\.R|2_.*\\.R", full.names = TRUE) %>%
      for (file in .) source(file)
    # output directories:
    #edge_out <- paste0(output_dir, "edges", i, ".gpkg")
    #node_out <- paste0(output_dir, "nodes", i, ".gpkg")
    edge_out <- paste0(output_dir, "edges.gpkg")
    node_out <- paste0(output_dir, "nodes.gpkg")
    #node_missed_out <- paste0(output_dir, "nodes_missed.gpkg")
    # User communication
    message(paste(i, "of", nrow(cityGrid)))
    # Intersect input with grid
    gridBox <- cityGrid[i,] %>%
      sf::st_geometry() %>%
      sf::st_as_text()
    # Buildings
    build_tile <- build_entries %>%
      sf::st_read(wkt_filter = gridBox, quiet = TRUE) %>%
      sf::st_cast("POINT")
    # Green space entries
    gs_tile <- gs_entries %>%
      sf::st_read(wkt_filter = gridBox, quiet = TRUE) %>%
      sf::st_cast("POINT")
    # Network (with buffer to avoid errors at edges)
    gridBox1 <- cityGrid[i,] %>%
      sf::st_buffer(100, nQuadSegs = 1) %>%
      sf::st_geometry() %>%
      sf::st_as_text()
    net_tile <- network %>%
      sf::st_read(wkt_filter = gridBox1, quiet = TRUE) %>%
      sf::st_cast("LINESTRING")
    # Snap buildings and green space entries to network
    if (nrow(build_tile) > 0) {
      build_snap <- st_snap_points(points = build_tile, lines = net_tile)
    } else build_snap <- NULL
    if (nrow(gs_tile) > 0) {
      gs_snap <- st_snap_points(points = gs_tile, lines = net_tile)
    } else gs_snap <- NULL
    # Make sure object is not empty
    if (is.null(build_snap) & is.null(gs_snap)) next
    if (is.null(build_snap)) {nodes <- mutate(gs_snap, population = NA, ID = NA)
    } else if (is.null(gs_snap)) {nodes <- mutate(build_snap, city_code = NA,
                                                  class = NA, identifier = NA,
                                                  area = NA)}  else {
                                                    nodes <- dplyr::bind_rows(build_snap, gs_snap) %>%
                                                      dplyr::distinct()}
    # Blend buildings and green space entries to network
    edges <- sf::st_collection_extract(lwgeom::st_split(net_tile, sf::st_buffer(nodes, 1e-5)),
                                       "LINESTRING")
    st_geometry(edges) <- edges %>%
      st_geometry() %>%
      lapply(function(x) round(x, 0)) %>%
      st_sfc(crs = 3035)
    edges <- distinct(edges)

    st_geometry(nodes) <- nodes %>%
      st_geometry() %>%
      lapply(function(x) round(x, 0)) %>%
      st_sfc(crs = 3035)
    nodes <- distinct(nodes)

    # nodes_net <- edges %>%
    #   as_sfnetwork() %>%
    #   activate("nodes") %>%
    #   st_join(nodes) %>%
    #   st_as_sf()

    st_write(nodes, node_out, quiet = TRUE, append = TRUE)
    st_write(edges, edge_out, quiet = TRUE, append = TRUE)

    # if (nrow(build_tile) > 0) {
    # filter(nodes, !(ID %in% nodes_net$ID)) %>%
    #   st_write(node_missed_out, quiet = TRUE, append = TRUE) }

    # tile_blend <-
    #   as_sfnetwork(net_tile) %>%
    #   st_network_blend(y = nodes)
    # # Write results to temp file
    # tile_blend %>%
    #   activate("edges") %>%
    #   st_as_sf() %>%
    #   st_write(edge_out, layer = "edges", quiet = TRUE, append = TRUE)
    # tile_blend %>%
    #   activate("nodes") %>%
    #   st_as_sf() %>%
    #   st_write(node_out, layer = "nodes", quiet = TRUE, append = TRUE)
    # Clean up
    # rm(tile_blend, nodes, net_tile, gs_tile, gs_snap,
    #    build_tile, build_snap)
    # gc()
    #return(i)
  }

  #stopCluster(cl)

}
# OSMsnap <- function(osm_buildings, network_tile, crs = 3035)
# {
#   osm_buildings %>%
#     st_point_on_surface() %>%
#     snapPTLsf(network_tile)
# }

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

# snapPointsToLines <-  function(points, lines, maxDist = NA, withAttrs = TRUE, idField = NA)
# {
#   require(maptools, quietly = TRUE)
#   if (rgeosStatus()) {
#     if (!requireNamespace("rgeos", quietly = TRUE))
#       stop("package rgeos required for snapPointsToLines")
#   } else stop("rgeos not installed")
#   if (is(points, "SpatialPointsDataFrame") == FALSE && missing(withAttrs))
#     withAttrs = FALSE
#   if (is(points, "SpatialPointsDataFrame") == FALSE && withAttrs == TRUE)
#     stop("A SpatialPointsDataFrame object is needed! Please set withAttrs as FALSE.")
#
#
#   d = rgeos::gDistance(points, lines, byid = TRUE)
#   if (!is.na(maxDist)) {
#     distToLine <- apply(d, 2, min, na.rm = TRUE)
#     validPoints <- distToLine <= maxDist
#     distToPoint <- apply(d, 1, min, na.rm = TRUE)
#     validLines <- distToPoint <= maxDist
#     points <- points[validPoints, ]
#     lines <- lines[validLines, ]
#     d <- d[validLines, validPoints, drop = FALSE]
#     distToLine <- distToLine[validPoints]
#     if (!any(validPoints)) {
#       if (is.na(idField)) {
#         idCol = character(0)
#       } else {
#         idCol = lines@data[, idField][0]
#       }
#       newCols = data.frame(nearest_line_id = idCol, snap_dist = numeric(0))
#       if (withAttrs) {
#         df <- cbind(points@data, newCols)
#       } else df <- newCols
#       res <- SpatialPointsDataFrame(points, data = df,
#                                     proj4string = CRS(proj4string(points)), match.ID = FALSE)
#       return(res)
#     }
#   } else {
#     distToLine = apply(d, 2, min, na.rm = TRUE)
#   }
#   nearest_line_index = apply(d, 2, which.min)
#   coordsLines = coordinates(lines)
#   coordsPoints = coordinates(points)
#   mNewCoords = vapply(1:length(points), function(x) nearestPointOnLine(coordsLines[[nearest_line_index[x]]][[1]],
#                                                                        coordsPoints[x, ]), FUN.VALUE = c(0, 0))
#   if (!is.na(idField)) {
#     nearest_line_id = lines@data[, idField][nearest_line_index]
#   } else {
#     nearest_line_id = sapply(slot(lines, "lines"),
#                              function(i) slot(i, "ID"))[nearest_line_index]
#   }
#   if (withAttrs) {
#     df = cbind(points@data, data.frame(nearest_line_id, snap_dist = distToLine))
#   } else df = data.frame(nearest_line_id, snap_dist = distToLine,
#                          row.names = names(nearest_line_index))
#   SpatialPointsDataFrame(coords = t(mNewCoords), data = df,
#                          proj4string = CRS(proj4string(points)))
# }
# snapPTLsf <- function(points_sf, lines_sf, max_distance, crs = 3035)
# {
#   # convert points to sp object
#   x <-
#     points_sf %>%
#     st_cast("POINT") %>%
#     as_Spatial()
#
#   # convert lines to sp object
#   y <-
#     lines_sf %>%
#     st_cast("LINESTRING") %>%
#     as_Spatial()
#
#   output <- snapPointsToLines(points = x, lines = y, maxDist = max_distance) %>%
#     st_as_sf() %>%
#     st_transform(crs)
#
#   return(output)
# }


