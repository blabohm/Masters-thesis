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
# 1. FILTER OSM BUILDINGS FOR INSIDE UA RESIDENTIAL POLYGONS
# REQUIRED SETTINGS:
# osm_buildings: OSM buildings as sfc_object with geometry type POLYGON
# ua_residential: UA residential as sfc_object with geometry type POLYGON
################################################################################
OSMfilter <- function(osm_buildings, ua_residential) {
  # required packages
  require(dplyr)
  require(sf)
  # user communication
  message("\n filtering OSM buildings... \n")
  # bildings that are not residential
  not_res <- c("^mall$", "train_station", "garages", "hospital",
               "parking", "sports_centre", "university", "gas_station",
               "school", "hall", "government", "prison", "sports_hall",
               "carport", "garbage", "waste")
  osm_buildings %>%
    st_join(ua_residential) %>%
    # filter OSM buildings inside UA residential polygons
    filter(!is.na(code_2018)) %>%
    # filter OSM buildings with non residential classes
    filter(!(building %in% not_res)) %>%
    select(Pop2018, identifier, code_2018) %>%
    return() }


################################################################################
# 2. ADD POPULATION FROM URBAN ATLAS
# REQUIRED SETTINGS:
# osm_buildings: filtered OSM buildings as sfc_object with geometry type POLYGON
# with joined UA columns 'population', 'identifier' and 'code_2018'
################################################################################
OSMpop <- function(osm_buildings) {
  # required packages
  require(dplyr)
  require(sf)
  # user communication
  message("calculating Population per building... \n")
  # add area column
  osm <- mutate(osm_buildings, area = st_area(geom))
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
    mutate(population = (area / area_sum * Pop2018) %>%
             round()) %>%
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
    mutate(pop_by_area = (area * pop_per_sqm) %>% round()) %>%
    mutate(population = as.double(population),
           pop_by_area = as.double(pop_by_area)) %>%
    mutate(population = ifelse(population == 0 & pop_by_area > population,
                               pop_by_area, population)) %>%
    select(code_2018, identifier,
           area, area_sum, pop_per_sqm,
           pop_by_area, population, Pop2018) %>%
    return() }


################################################################################
# 5. ADD BUILDING ID
#    -> IDENTIFIER + NUMBER
# 6. CONVERT TO BUILDING CENTROID
#    -> POINT ON SURFACE
# 7. READ OSM NETWORK
#    -> SNAP BUILDING CENTROIDS TO NETWORK
# 8. OUTPUT TO TEMP
# REQUIRED SETTINGS:
# osm_buildings: filtered OSM buildings as sfc_object with geometry type POLYGON
# with joined UA columns 'population', 'identifier' and 'code_2018'
################################################################################
