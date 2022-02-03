
# load packages
library(dplyr)

# load functions
source("C:/Users/labohben/Documents/GitHub/MA/functions/getOSMfuns1_2.R")

# priority table
priority_df <- 
  read.csv("C:/Users/labohben/Documents/GitHub/MA/priority_df.csv")

# UA 2006 compatible
comp_df <- 
  read.csv("Z:/UA/master_table.csv") %>% 
  filter(!is.na(UA2006), 
         !is.na(UA2012), 
         !is.na(UA2018)) %>% 
  mutate(code = FUA)

# Data directory
dataDir <- "E:/UA2018/"

# generate list of input files
in_list <-
  dataDir %>% 
  list.files(recursive = T,
             pattern = ".gpkg$",
             full.names = T) %>% 
  as_tibble() %>% 
  mutate(code = namify(value)) %>% 
  merge(priority_df,
        by = "code",
        all.x = T) %>% 
  merge(comp_df,
        by = "code",
        all.y = T) %>% 
  arrange(priority)

in_list <-
  in_list %>% 
  filter(priority < 2)
  
# OR
# use boundary layer:
# in_list <-
#   "E:/citiesEurope/Cities.shp" %>% 
#   sf::st_read()

# output directory
out_path <- "E:/osm_buildings/"
OSMkey <- "building"

#out_path <- "E:/osm_paths/"
#OSMkey <- "highway"

# out_path <- "E:/osm_barriers/"
# OSMkey <- "barrier"
# names for boundary box (bbox) list
dfNames <- list(NULL, c("cityTag", "xmin", "xmax", "ymin", "ymax"))


################################################################################

# empty bbox list
bbList <- 
  tibble()
  # matrix(ncol = 5, nrow = 1, dimnames = dfNames) %>% 
  # data.frame() 

# set up progress bar
pb <- txtProgressBar(min = 0, max = nrow(in_list), 
                     initial = 0, style = 3)
stepi <- 0

# create list of bboxes
for (in_file in in_list$value) {
  bbList <- createBB(in_file = in_file,
                     bboxList = bbList) %>% 
    na.omit()
  # for progress bar
  stepi <- stepi + 1
  setTxtProgressBar(pb, stepi)
}
 
# check which files to download
to_do <- 
  check.up(bbList$cityTag, out_path)

################################################################################

# set up progress bar
#pb <- txtProgressBar(min = 0, max = nrow(bbList[to_do,]), 
#                     initial = 0, style = 3)
#stepi <- 0
# list of API links
api_list <- dplyr::tibble(interpreter = 
                            c('http://overpass-api.de/api/interpreter',
                              'https://lz4.overpass-api.de/api/interpreter',
                              'https://z.overpass-api.de/api/interpreter'#,
                              #'https://overpass.kumi.systems/api/interpreter'
                            ),
                          nTry = c(0, 0, 0)) # manipulate to have second decision


# apply download function
for (i in to_do) {
  print(i)
  
  dlOSM(in_vector = bbList[i,], 
        out_path = out_path,
        OSMkey = OSMkey)
  # for progress bar
#  stepi <- stepi + 1
#  setTxtProgressBar(pb, stepi)
  }


# cut failed downloads into even smaller pieces and repeat process:
bbListNew <-
  tibble()

bbListNew <-
  splitBBdf(bbList[to_do,], bbListNew, 0.1)

to_do <- 
  check.up(bbListNew$cityTag, out_path)

# apply download function
for (i in to_do) {
  print(i)
  
  dlOSM(in_vector = bbListNew[i,], 
        out_path = out_path,
        OSMkey = OSMkey)
  # for progress bar
  #  stepi <- stepi + 1
  #  setTxtProgressBar(pb, stepi)
}

# cut failed downloads into even smaller pieces and repeat process:

bbListNew1 <- tibble()
  
bbListNew1 <-
  splitBBdf(bbListNew[to_do,], bbListNew1, 0.05)

to_do <- 
  check.up(bbListNew1$cityTag, out_path)

# apply download function
for (i in to_do) {
  print(i)
  
  dlOSM(in_vector = bbListNew1[i,], 
        out_path = out_path,
        OSMkey = OSMkey)
  # for progress bar
  #  stepi <- stepi + 1
  #  setTxtProgressBar(pb, stepi)
}

################################################################################
# system.time(
# # write to file
# sf::st_write(sf::st_as_sf(p1),
#              dsn = "E:/temp/buildings5.gpkg",
#              append = FALSE
#              #layer = lyr,
#              #driver = "GPKG"
#              )
# )
# debugging
# test@polygons$x_55913888@ID <- 
#   test@polygons$x_55913888@ID %>% 
#   paste0("x_",.)
# 
# p2 <- 
#   p1 %>% 
#   #  as(., "SpatialPolygonsDataFrame") #%>% 
#   sp::SpatialPolygonsDataFrame(.,
#                                data = p1@data)
# 
# b1 <- 
#   sp::SpatialPointsDataFrame(buildings$osm_polygons,
#                              data = buildings$osm_polygons@data)
# b2 <- 
#     p1 %>% 
# #  as(., "SpatialPolygonsDataFrame") #%>% 
#   sp::SpatialPolygonsDataFrame(.,
#                                data = buildings$osm_polygons@data)
# 
# b1@data <- b1@data[1]
# 
# dsn = "E:/temp/buildings2.gpkg"
# lyr = "buildings"
# writeOGR(b2,
#          dsn = dsn,
#          layer = lyr,
#          driver = "GPKG",
#          overwrite_layer = T)

#   b1 <-
#   sf::st_read("E:/temp/buildings.osm", layer = "osm_polygons")
# b2 <-
#   b1 %>% 
#   as(., "Spatial")
# 
# 
# 
# writeOGR(b2,
#          dsn = "E:/temp/buildings.gpkg",
#          layer = "buildings",
#          driver = "GPKG",
#          overwrite_layer = T)

# b2 <-
#   buildings %>% 
#   trim_osmdata(boundary)

# test <- 
#   getbb("Tirana")  %>% 
#   opq() %>% 
#   add_osm_feature(., 
#                   key = "building"#, 
#                   #value = "building"#, 
#                   #bbox = bbox(.)
#   ) %>% 
#   osmdata_sp()


# buildings$bbox %>% 
#   strsplit(",") %>% 
#   unlist() %>% 
#   as.numeric() %>% 
#   matrix(nrow = 2) %>% 
#   plot()
# 
# test$bbox %>% 
#   strsplit(",") %>% 
#   unlist() %>% 
#   as.numeric() %>% 
#   matrix(nrow = 2) %>% 
#   points(., add = T, col = "red")
# 
# get_overpass_url()
