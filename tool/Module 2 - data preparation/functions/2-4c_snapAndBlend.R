

mkCityGrid <- function(city_code, city_boundaries, cellsize, crs = 3035)
{
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  bound_query <- paste0('SELECT * FROM \'cities\' WHERE URAU_CODE = \'',
                        city_code, '\'')
  city_boundary <- city_boundaries %>%
    st_read(query = bound_query, quiet = TRUE) %>%
    st_transform(crs) %>%
    st_buffer(1000)
  city_boundary %>%
    st_make_grid(cellsize = cellsize) %>%
    st_as_sf() %>%
    st_filter(city_boundary, .pred = st_intersects) %>%
    return()
}


getGb <- function(cityGrid, i)
{
  cityGrid[i,] %>%
    sf::st_geometry() %>%
    sf::st_as_text() %>%
    return()
}


getNodes <- function(directory, gridBox)
{
  tryCatch({directory %>%
      sf::st_read(wkt_filter = gridBox, quiet = TRUE) %>%
      sf::st_cast("POINT", do_split = TRUE, warn = FALSE)},
      error = function(e) return(NULL)) %>%
    return()
}


getEdges <- function(network, cityGrid, i)
{
  # Network (with buffer to avoid errors at edges)
  gridBox1 <- cityGrid[i,] %>%
    sf::st_buffer(100, nQuadSegs = 1) %>%
    sf::st_geometry() %>%
    sf::st_as_text()
  tryCatch({network %>%
      sf::st_read(wkt_filter = gridBox1, quiet = TRUE) %>%
      sf::st_cast("LINESTRING", do_split = TRUE, warn = FALSE)},
      error = function(e) return(NULL)) %>%
    return()
}


mergeNodes <- function(nodesBuild, nodesGS, node_out)
{
  if (!is.null(nodesBuild)) if (nrow(nodesBuild) == 0) nodesBuild <- NULL
  if (!is.null(nodesGS)) if (nrow(nodesGS) == 0) nodesGS <- NULL
  if (is.null(nodesBuild) & is.null(nodesGS)) return(NULL)
  if (is.null(nodesBuild)) return(mutate(nodesGS, population = NA, ID = NA))
  if (is.null(nodesGS)) return(mutate(nodesBuild, city_code = NA, class = NA,
                                      identifier = NA, area = NA))
  bind_rows(nodesBuild, nodesGS) %>%
    distinct() %>%
    return()
}


blend <- function(net_tile, nodes)
{
  require(sfnetworks)
  sfnetworks::as_sfnetwork(net_tile) %>%
    sfnetworks::st_network_blend(nodes, tolerance = 1e-4) %>%
    return()
}


writeOutput <- function(net, edge_out, node_out)
{
    activate(net, "edges") %>%
    st_as_sf() %>%
    select(-matches("from|to")) %>%
    distinct() %>%
    write_sf(edge_out, append = FALSE)

    activate(net, "nodes") %>%
    st_as_sf() %>%
    select(-matches("city_code|class")) %>%
    distinct() %>%
    write_sf(node_out, append = FALSE)
}

