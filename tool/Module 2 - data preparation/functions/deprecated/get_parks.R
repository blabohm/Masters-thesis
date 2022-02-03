library(dplyr)
library(sf)
library(ggplot2)


################################################################################
# PREPARATION PHASE
library(dplyr)
library(sf)

yrs <- c(2006, 2012, 2018)

for (yr in yrs) {

  # urban atlas directory
  indir <- paste0("Z:/UA", yr, "/")

  if(yr > 2006) {
    # urban atlas 2012/2018 gpkg files
    ua <-
      list.files(indir, full.names = TRUE) %>%
      list.files(pattern = "gpkg$",
                 full.names = TRUE,
                 recursive = TRUE)
  } else {
    # urban atlas 2006 shp files
    ua <-
      list.files(indir, full.names = TRUE) %>%
      list.files(pattern = "shp$",
                 full.names = TRUE,
                 recursive = TRUE) %>%
      grep("evised", ., value = TRUE)
  }

  # urban atlas (ua) residential classes
  res_class <- c(11100, 11210, 11220,
                 11230, 11240, 11300)

  #res_class <- c(14100, 30000, 31000)

  # layer and destination for output
  outLayer <- paste0("UA", yr, "_residential_buffer300m")
  outDsn <- paste0("Z:/", outLayer, ".gpkg")

  # set up progress bar
  pb <- txtProgressBar(min = 0, max = length(ua),
                       initial = 0, style = 3)
  stepi <- 0

  for (uaFile in ua) {

    # for progress bar
    stepi <- stepi + 1
    setTxtProgressBar(pb, stepi)

    # get name of land use layer
    lyr <- st_layers(uaFile)$name[1]

    # write parks to one layer
    st_read(uaFile, layer = lyr, quiet = TRUE) %>%
      select(city_code = contains("FUA_OR") | contains("fua_code"),
             class = contains("code") & contains("20")) %>%
      filter(class %in% res_class) %>%
      st_buffer(300) %>%
      st_union() %>%
      st_write(dsn = outDsn, layer = outLayer,
               append = TRUE, quiet = TRUE)
  }
}


# READ UA DATA AND FILTER FOREST + URBAN GREEN CLASSES
gpkg <- "E:/UA2018/DE001L1_BERLIN_UA2018_v012/Data/DE001L1_BERLIN_UA2018_v012.gpkg"

# READ UA GPKG
lr <- st_layers(gpkg)
land_use <- st_read(gpkg, lr$name[1])

# FILTER
parks <-
  land_use %>%
  filter(code_2018 %in% c(14100, 31000)) %>%
  select(contains("identifier"),
         contains("area"))

# WRITE TO FILE
parks %>%
  st_write("E:/temp/parks.gpkg", append = FALSE)

# READ FROM FILE
# parks <-
#   st_read("E:/temp/parks.gpkg")
parks <-
  # st_read("E:/temp/vali_parks.gpkg")
  st_read(park_file) %>%
  st_intersection(core)

# BUFFER PARKS BY x METERS
x <- 0

parks_buffered <-
  parks %>%
  st_buffer(x) %>%
  #st_union() %>%
  st_cast("MULTILINESTRING")# %>%
  #st_sf()

# parks_buffered %>%
#   st_write("E:/temp/parks_buffered_10m.gpkg", append = FALSE)

# FILTER WALKABLE PATHS
# paths <-
#   st_read("E:/temp/paths.gpkg")

paths_filtered <-
  paths %>%
  filter(!grepl("primary", highway)) %>%
  filter(!grepl("trunk", highway)) %>%
  filter(!grepl("motorway", highway)) %>%
  st_transform(3035)

paths_filtered %>%
  select(osm_id, highway) %>%
  st_write("E:/temp/paths_filtered.gpkg", append = FALSE)

# paths_filtered <-
#   st_read("E:/temp/paths_filtered.gpkg")


#paths_barriers <- st_difference(paths_filtered, barriers_buffered)


# parks_buffered <-
#   st_read("E:/temp/parks_buffered_5m.gpkg")


st_crs(highway_clean) <- 3035

system.time({
park_entries <-
  st_intersection(parks_buffered, highway_clean)
})

b <- 5

system.time({

  while (
    parks %>%
    filter(!(identifier %in% unique(park_entries$identifier))) %>%
    nrow() > 0
  ) {
    park_entries <-
      parks %>%
      filter(!(identifier %in% unique(park_entries$identifier))) %>%
      st_buffer(b) %>%
      st_cast("MULTILINESTRING") %>%
      st_intersection(highway_clean) %>%
      bind_rows(park_entries)

    if (b <= 50) {b <- b + 5
    } else if (b <= 100) {b <- b + 10
    } else if (b <= 200) {b <- b + 25
    } else {b <- b + 50}
  }
})

park_entries %>%
  #st_cast("POINT") %>%
  st_write("C:/Berlin/park_entries.gpkg", append = FALSE)

################################################################################

gcp <-
  read.csv("E:/gcp/Park_entries_001.csv") %>%
  select(lon, lat, name) %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326)

st_write(gcp, "E:/temp/gcp.gpkg", append = FALSE)


gcp <-
  read.csv("E:/temp/gcp.csv") %>%
  mutate(X = round(X, 5),
         Y = round(Y, 5)) %>%
  select(X, Y, name) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326)




################################################################################

park_entries <- st_read("E:/temp/park_entries.gpkg")
park_entries_5m <- st_read("E:/temp/park_entries_5m.gpkg")
park_entries_10m <- st_read("E:/temp/park_entries_10m.gpkg")


park_entries %>%
  st_difference(barriers_buffered) %>%
  st_write("E:/temp/park_entries_filtered")

park_entries_5m  %>%
  st_difference(barriers_buffered) %>%
  st_write("E:/temp/park_entries_5m_filtered")

park_entries_10m  %>%
  st_difference(barriers_buffered) %>%
  st_write("E:/temp/park_entries_10m_filtered")




parks_buffered <-
  st_read("E:/temp/parks_buffered_5m.gpkg")

diss <-
  parks_buffered %>%
  st_union() %>%
  st_cast("LINESTRING")
test <- st_cast(diss, "POLYGON")

write_sf(diss, "E:/temp/parks_dissolved.gpkg")



################################################################################

# FIND KEYS FOR HIGHWAY FILTER
# a <-
#   lpzg_paths %>%
# #  filter(foot != "no") %>%
#  # filter(!grepl("primary", highway)) %>%
#   pull(highway) %>%
#   unique()
# a
# b <-
#   lpzg_paths$highway %>% unique()

# barriers <-
#   st_read("E:/temp/barriers.gpkg")
#
# barriers_filtered <-
#   barriers %>%
#   filter(!grepl("gate", barrier)) %>%
#   filter(!grepl("rail", barrier)) %>%
#   filter(!grepl("block", barrier)) %>%
#   filter(!grepl("cycle", barrier)) %>%
#   filter(!grepl("entrance", barrier)) %>%
#   filter(!grepl("stile", barrier)) %>%
#   filter(!grepl("restrictor", barrier)) %>%
#   filter(!grepl("motorcycle", barrier)) %>%
#   filter(!grepl("port", barrier)) %>%
#   filter(!grepl("spikes", barrier)) %>%
#   filter(!grepl("stump", barrier)) %>%
#   filter(!grepl("booth", barrier)) %>%
#   filter(!grepl("planter", barrier)) %>%
#   filter(!grepl("bollard", barrier)) %>%
#   filter(!grepl("chain", barrier)) %>%
#   filter(!grepl("kerb", barrier)) %>%
#   filter(!grepl("rope", barrier)) %>%
#   filter(!grepl("trap", barrier)) %>%
#   filter(!grepl("rock", barrier)) %>%
#   filter(!grepl("island", barrier)) %>%
#   filter(!grepl("wood", barrier)) %>%
#   filter(!grepl("reling", barrier)) %>%
#   filter(!grepl("flower", barrier)) %>%
#   filter(!grepl("tree", barrier)) %>%
#   filter(!grepl("pipe", barrier)) %>%
#   filter(!grepl("debris", barrier)) %>%
#   filter(!grepl("step", barrier)) %>%
#   filter(!grepl("barrier", barrier)) %>%
#   filter(!grepl("pollar", barrier)) %>%
#   filter(!grepl("line", barrier))
#
# barriers_filtered %>%
#   st_write("E:/temp/barriers_filtered.gpkg", append = FALSE)
#
#
#
# barriers_buffered <-
#   st_read("E:/temp/barriers_filtered.gpkg") %>%
#   st_transform(3035) %>%
#   st_buffer(5)
#
# barriers_buffered %>%
# st_write("E:/temp/barriers_buffered.gpkg", append = FALSE)
