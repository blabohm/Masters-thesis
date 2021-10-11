if(!"remotes" %in% installed.packages()) {
  install.packages("remotes")
}

cran_pkgs = c(
  "sf",
  "tidygraph",
  "igraph",
  "osmdata",
  "dplyr",
  "tibble",
  "ggplot2",
  "units",
  "tmap",
  "rgrass7",
  "link2GI",
  "nabor"
)

remotes::install_cran(cran_pkgs)

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

coreFile <- "D:/UA2018/DE001L1_BERLIN_UA2018_v012/Data/DE001L1_BERLIN_UA2018_v012.gpkg"

core <- 
  st_layers(coreFile)$name[3] %>% 
  st_read(dsn = coreFile, layer = .) %>% 
  st_buffer(5000) %>% 
  st_transform(3035)

highway <- 
  "D:/temp/paths_filtered.gpkg" %>% 
  st_read() %>% 
  select(highway) %>% 
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


highway_clean %>% write_sf("D:/temp/highway_clean.gpkg")
