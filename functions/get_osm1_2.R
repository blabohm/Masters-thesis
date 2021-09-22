
# load packages
library(dplyr)

# load functions
source("C:/Users/labohben/Documents/GitHub/MA/functions/getOSMfuns1_4.R")

# priority table
priority_df <- 
  read.csv("C:/Users/labohben/Documents/GitHub/MA/priority_df.csv")

# UA 2006 compatible
comp_df <- 
  read.csv("C:/Users/labohben/Documents/GitHub/MA/master_table.csv") %>% 
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
  
in_list <-
  in_list %>% 
  filter(NAME == "LEIPZIG")
# OR
# use boundary layer:
 # in_list <-
 #   "E:/citiesEurope/Cities.shp" %>% 
 #   sf::st_read()


################################################################################

# empty bbox list
bbList <-
  tibble()

# set up progress bar
pb <- txtProgressBar(min = 0, max = nrow(in_list),
                     initial = 0, style = 3)
stepi <- 0

# target Size of boundary boxes
targetSize <- .2

# create list of bboxes
for (in_file in in_list$value) {
  bbList <- createBB(in_file = in_file,
                     bboxList = bbList,
                     tSize = targetSize) %>%
    na.omit()
  # for progress bar
  stepi <- stepi + 1
  setTxtProgressBar(pb, stepi)
}

# check which files to download
to_do <-
  check.up(bbList$cityTag, out_path)

################################################################################
  
# list of API links
api_list <- dplyr::tibble(interpreter = 
                            c('http://overpass-api.de/api/interpreter',
                              'https://lz4.overpass-api.de/api/interpreter',
                              'https://z.overpass-api.de/api/interpreter'#,
                              #'https://overpass.kumi.systems/api/interpreter'
                            )) 

# apply download function
for (i in to_do) {
  paste(i, "of", max(to_do)) %>% 
    print()
  
  # BUILDINGS
  dlOSM(in_vector = bbList[i,], # vector containing xmin, xmax, ymin, ymax, cityTag
        out_path = "E:/osm_buildings/",
        OSMkey = "building")
}

for (i in to_do) {
  paste(i, "of", max(to_do)) %>% 
    print()
  # PATHS
  dlOSM(in_vector = bbList[i,], # vector containing xmin, xmax, ymin, ymax, cityTag
        out_path = "E:/osm_paths/",
        OSMkey = "highway")
}

for (i in to_do) {
  paste(i, "of", max(to_do)) %>% 
    print()  
  # BARRIERS
  dlOSM(in_vector = bbList[i,], # vector containing xmin, xmax, ymin, ymax, cityTag
        out_path = "E:/osm_barriers/",
        OSMkey = "barrier")
}

# second iteration - try splitting and downloading again
# check which bbox tiles were downloaded
to_do <-
  check.up(bbList$cityTag, out_path)

# split remaining tiles
bbList2 <-
  bbList[to_do,] %>% 
  splitBBdf()

# check if resulting tiles were already downloaded
to_do <-
  check.up(bbList2$cityTag, out_path)

# download 
for (i in to_do) {
  paste(i, "of", max(to_do)) %>% 
    print()
  
  dlOSM(in_vector = bbList2[i,], 
        out_path = out_path,
        OSMkey = OSMkey)
}



################################################################################
# check if all tiles were downloaded correctly
bbList3 <-
  tibble()

for (i in 1:nrow(in_list)) {
  paste(i, "of", nrow(in_list)) %>%
    print()
  
  bbox <- bboxFromUA(in_list$value[i])
  
  bbList3 <-
  check.up2(id = in_list$code[i],
            fileDirectory = out_path,
            bbox = bbox,
            boundaryDirectory = in_list$value[i]) %>% 
    bind_rows(bbList3, .)
}
  
# apply download function for failed downloads
for (i in 1:nrow(in_list)) {
  paste(i, "of", nrow(in_list)) %>%
    print()

  dlOSM2(in_vector = in_list[i,],
         out_path = out_path,
         OSMkey = OSMkey)
}
