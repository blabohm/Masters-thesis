################################################################################
# MODULE 2 - DATA PREPARATION
# PART 2 - BUILDING ENTRIES
# 2b - PROCESSING FUNCTIONS
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################
#
# FUNCTIONS:
# 1. OSMfilter
#    -> Filter OSM buildings inside UA residential polygons
#    -> Filter OSM buildings with non-residential classes
#    -> Join UA columns 'population', 'identifier' and 'code_2018'
# 2. OSMpop
#    -> Add population from UA
# 3. OSMbuildID
#    -> Add building ID
#
################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################

cityBoundLoader <- function(city_boundaries, city_code = NULL, buffer_dist = 0,
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
    st_transform(crs)

  if (is.null(city_code)) return(cityBound) else {
    cityBound %>%
      filter(code %in% city_code) %>%
      st_buffer(buffer_dist) %>%
      return() }
}


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
# 1. FILTER OSM BUILDINGS FOR INSIDE UA RESIDENTIAL POLYGONS
# REQUIRED SETTINGS:
# osm_buildings: OSM buildings as sfc_object with geometry type POLYGON
# ua_residential: UA residential as sfc_object with geometry type POLYGON
################################################################################

proximity_checker <- function(city_boundary, osm_file, city_code, crs = 3035)
{
  # Load required packages
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)

  # User communication
  message("Checking proximity")
  tmpDf <- tibble()
  for (i in osm_file) {
    # Filter for desired city
    lyr_tbl <- st_layers(i)
    x <- which.max(lyr_tbl$features)
    lyr1 <- lyr_tbl$name[x]
    c <- st_read(i, layer = lyr1, quiet = TRUE) %>%
      st_transform(crs) %>%
      sfc2bb(crs = crs)
    # Check for proximity of other cities
    if(any(st_intersects(c$geom, cityBound, sparse = FALSE)))
      tmpDf <- tibble(tile_dir = i) %>%
      bind_rows(tmpDf)
  }
  return(tmpDf)
}

################################################################################
# 1. FILTER OSM BUILDINGS FOR INSIDE UA RESIDENTIAL POLYGONS
# REQUIRED SETTINGS:
# osm_buildings: OSM buildings as sfc_object with geometry type POLYGON
# ua_residential: UA residential as sfc_object with geometry type POLYGON
################################################################################

OSMfilter <- function(osm_buildings, ua_residential, city_boundaries)
{
  # required packages
  require(dplyr)
  require(sf)
  # bildings that are not residential
  not_res <- c("^mall$", "train_station", "garages", "hospital",
               "parking", "sports_centre", "university", "gas_station",
               "school", "hall", "government", "prison", "sports_hall",
               "carport", "garbage", "waste")
  # user communication
  message("\n filtering OSM buildings... \n")
  # filtering
  osm_buildings %>%
    st_filter(cityBound, .pred = st_intersects) %>%
    st_join(ua_residential) %>%
    # filter OSM buildings inside UA residential polygons
    filter(!is.na(code_2018)) %>%
    # filter OSM buildings with non residential classes
    filter(!(building %in% not_res)) %>%
    select(Pop2018, identifier, code_2018) %>%
    return()
}


################################################################################
# 2. ADD POPULATION FROM URBAN ATLAS
# REQUIRED SETTINGS:
# osm_buildings: filtered OSM buildings as sfc_object with geometry type POLYGON
# with joined UA columns 'population', 'identifier' and 'code_2018'
################################################################################

OSMpop <- function(osm_buildings)
{
  # required packages
  require(dplyr)
  require(sf)
  # add area column
  osm <- mutate(osm_buildings, area = st_area(geom))
  # user communication
  message("\n calculating Population per building... \n")
  # sum osm areas in each ua polygon (by ua identifier)
  area_df <-
    osm %>%
    st_drop_geometry() %>%
    group_by(identifier) %>%
    summarise(area_sum = sum(area))
  # calculate Population per building by building area
  pop_sf <-
    osm %>%
    merge(area_df) %>%
    mutate(population = round(area / area_sum * Pop2018)) %>%
    select(population, area, area_sum,
           Pop2018, identifier, code_2018)
  # calculate population per square meter by average pop / area / ua class
  pop_df <-
    pop_sf %>%
    na.omit() %>%
    st_drop_geometry() %>%
    mutate(pop_per_sqm = population / area) %>%
    group_by(code_2018) %>%
    summarise(pop_per_sqm = mean(pop_per_sqm))
  # generate output
  pop_sf %>%
    merge(pop_df, all.x = TRUE) %>%
    mutate(pop_by_area =  round(area * pop_per_sqm)) %>%
    mutate(population = as.double(population),
           pop_by_area = as.double(pop_by_area)) %>%
    mutate(population = ifelse(population == 0 & pop_by_area > population,
                               pop_by_area, population)) %>%
    select(identifier, population) %>%
    filter(population > 0) %>%
    return()
}


################################################################################
# 3. ADD BUILDING ID
# REQUIRED SETTINGS:
# osm_buildings: filtered OSM buildings as sfc_object with geometry type POLYGON
# with columns 'population' and 'identifier'
################################################################################

OSMbuildID <- function(osm_buildings)
{
  require(dplyr)
  osm_buildings %>%
    group_by(identifier) %>%
    mutate(ID = paste0(identifier, row_number())) %>%
    select(-identifier)
}

