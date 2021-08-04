library(dplyr)


indir <- "E:/popdata/"
outdir <- "E:/popdata/schug/"

citySHP <- 
  sf::read_sf("E:/citiesEurope/Cities.shp") %>% 
  filter(grepl("Leipzig|Halle", URAU_NAME))


rstList <-
  list.files(indir,
             recursive = TRUE,
             pattern = "BP.tif$",
             full.names = TRUE)

dsn <- paste0(outdir,
              "leipzig_halle.tif")

gdalUtils::mosaic_rasters(rstList,
                          dst_dataset = dsn, )

popRst <- raster::raster(dsn)
raster::crs(popRst) <- '+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

