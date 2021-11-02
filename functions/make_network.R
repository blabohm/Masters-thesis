city_dir <- "C:/Berlin/berlin_cent.gpkg"
coreFile <- "C:/Berlin/DE001L1_BERLIN_UA2018_v013.gpkg"
park_file <- "C:/Berlin/parks.gpkg"

highway_clean <- st_read("C:/Berlin/highway_clean.gpkg")


# if(!"remotes" %in% installed.packages()) {
#   install.packages("remotes")
# }
# 
# cran_pkgs = c(
#   "sf",
#   "tidygraph",
#   "igraph",
#   "osmdata",
#   "dplyr",
#   "tibble",
#   "ggplot2",
#   "units",
#   "tmap",
#   "rgrass7",
#   "link2GI",
#   "nabor"
# )
# 
# remotes::install_cran(cran_pkgs)

library(sf)
library(tidygraph)
library(igraph)
library(dplyr)
library(tibble)
library(ggplot2)
library(units)
library(tmap)
library(osmdata)
library(rgrass7)
library(link2GI)
library(nabor)

source("D:/rgrass7-setup-win-osgeo4w.R")


core <- 
  st_layers(coreFile)$name[3] %>% 
  st_read(dsn = coreFile, layer = .) %>% 
  st_transform(3035)

highway <- 
  highway_file %>% 
  st_read() %>% 
  select(highway) %>% 
  st_intersection(core %>% st_buffer(5000))

cityCent <- 
  st_read(city_dir) %>% 
  st_transform(3035) %>% 
  st_intersection(core)


 # ggplot(data = highway) + 
#   geom_sf()

write_sf(highway, "D:/temp/core_highway.gpkg")
highway <- read_sf("D:/temp/core_highway.gpkg")

# Link to GRASS GIS
linkGRASS7(x = highway,
           ver_select = TRUE, 
           default_GRASS7 = NULL#"C://OSGeo4W64//apps//grass//grass76"
           )
gisBase = "C://OSGeo4W64//apps//grass//grass76"
location <- basename(tempfile())
gisdbase <- tempdir()

initGRASS(
  gisBase = gisBase,
  home = tempdir(),
  gisDbase = gisdbase,
  mapset = "PERMANENT",
  location = location,
  override = TRUE,
)

writeVECT(SDF = highway,
          vname = "highway",
          v.in.ogr_flags = c("o", "overwrite"))

proj4 <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

execGRASS("g.proj", flags = c("c", "quiet"), proj4 = proj4)
execGRASS(
  cmd = 'v.clean', 
  input = 'highway', 
  output = 'highway_cleaned',        
  tool = 'break', 
  flags = c('overwrite', 'c')
)

# Read back into R
use_sf()
highway_clean <- readVECT('highway_cleaned') %>%
  rename(geometry = geom) %>%
  select(-cat)


