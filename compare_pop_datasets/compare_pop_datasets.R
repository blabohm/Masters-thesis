# dplyr for pipes
library(dplyr)
library(ggplot2)

# directories
indir <- "E:/popdata/"
outdir <- "E:/popdata/schug/"

# used crs
EPSG3035 <- sf::st_crs(3035) 
WGS84 <- sf::st_crs(4326) 

# shape of Leipzig
citySHP <- 
  sf::read_sf("E:/citiesEurope/Cities.shp") %>% 
  #filter(grepl("Leipzig|Halle", URAU_NAME)) %>% 
  filter(grepl("Leipzig", URAU_NAME)) %>% 
  sf::st_transform(EPSG3035)

#cityUnion <-
#  sf::st_union(citySHP[1], citySHP[2])

# LOAD AND MERGE DATA SET FROM FRANZ SCHUG
# destination of Schug population
dsn <- paste0(outdir,
              "leipzig_schug.tif")

# rstList <-
#   list.files(indir,
#              recursive = TRUE,
#              pattern = "BP.tif$",
#              full.names = TRUE)

# popRst <- raster::raster()
# raster::crs(popRst) <- EPSG3035
# raster::res(popRst) <- 10
# raster::origin(popRst) <- c(-4, -.5)

# a <- raster::raster(rstList[1])
# b <- raster::raster(rstList[2])
# c <- raster::raster(rstList[3])
# d <- raster::raster(rstList[4])
# ab <- raster::merge(a, b)
# cd <- raster::merge(c, d)
# 
# popRst <- raster::merge(ab, cd)
# raster::writeRaster(popRst * 10000, 
#                     dsn, overwrite = TRUE, 
#                     datatype = "INT2U")
#raster::crs(popRst) <- EPSG3035$proj4string

popRstSchug <- 
  raster::raster(dsn) %>% 
  # crop population raster to Leipzig shape extent
  .[raster::extent(citySHP), drop = FALSE] %>% 
  raster::aggregate(10, fun = sum) %>% 
  raster::mask(citySHP)

# load WorldPop data 2018
popRstWorld <-
  raster::raster("E:/Manu/deu_ppp_2018_UNadj.tif") %>% 
  # clip WorldPop to Leipzig extent
  .[citySHP %>% 
      sf::st_transform(WGS84) %>% 
      raster::extent(), 
    drop = FALSE] %>% 
  raster::mask(citySHP %>% sf::st_transform(WGS84))

# load WorldPop data 2012
popRstWorld12 <-
  raster::raster("E:/Manu/deu_ppp_2012_UNadj.tif") %>% 
  # clip WorldPop to Leipzig extent
  .[citySHP %>% 
      sf::st_transform(WGS84) %>% 
      raster::extent(), 
    drop = FALSE] %>% 
  raster::mask(citySHP %>% sf::st_transform(WGS84))

# CHANGE RASTER
changeRst <- 
1 / popRstWorld12$deu_ppp_2012_UNadj * popRstWorld$deu_ppp_2018_UNadj

# TRANSECT
tsct <- sf::read_sf("E:/popdata/transect/transect.gpkg")

# EXTRACT SCHUG
schugExt <- 
  raster::extract(popRstSchug, tsct) %>% 
  unlist() %>% 
  tibble(nPop = . / 10000) %>% 
  mutate(x = row_number(),
         x = x / max(x),
         dataset = "Schug") %>% 
  select(x, nPop, dataset)

# EXTRACT WORDLPOP
wldExt <- 
  raster::extract(popRstWorld, tsct) %>% 
  unlist() %>% 
  tibble(nPop = .) %>% 
  mutate(x = row_number(),
         x = x / max(x),
         dataset = "WorldPop")

# EXTRACT URBAN ATLAS POLYGONS
# template raster
extRst <- 
  raster::extent(citySHP) %>%
  raster::raster(., resolution = 100,
         crs = sf::st_crs(EPSG3035)$proj4string)

# LOAD UA DATA
UA <-
  "Z:/UA2012/DE008L2_LEIPZIG_013/Data/DE008L2_LEIPZIG_013.gpkg" %>%
  sf::read_sf() %>%
  sf::st_intersection(citySHP) %>%
  select(Pop2012)

# CALCULATE CENTROIDS
UA_mp <- sf::st_centroid(UA)

# RASTERIZE 
popRstUA <- 
  raster::rasterize(UA_mp, extRst, 
                       field = "Pop2012", fun = "sum", background = 0) %>% 
  raster::mask(citySHP)

# EXTRACT UA
UAExt <- 
  raster::extract(popRstUA, tsct) %>% 
  unlist() %>% 
  tibble(nPop = .) %>% 
  mutate(x = row_number(),
         x = x / max(x),
         dataset = "UA2012")

# PLOT
schugExt %>% 
  bind_rows(wldExt) %>% 
  bind_rows(UAExt) %>% 
  ggplot(aes(x = x)) + 
  geom_line(aes(y = nPop, col = dataset))


# WRITE RASTERS TO FILE
raster::writeRaster(popRstUA,
                    "E:/popdata/UApop.tif",overwrite = TRUE)

raster::writeRaster(popRstSchug / 10000,
                    "E:/popdata/Schug.tif", overwrite = TRUE)

raster::writeRaster(popRstWorld,
                    "E:/popdata/WorldPopRdy.tif", overwrite = TRUE)

raster::writeRaster(changeRst,
                    "E:/popdata/change12_18.tif", overwrite = TRUE)
