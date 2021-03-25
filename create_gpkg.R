################################################################################
# The purpose of this Skript is to combine the shape and geopackage urban atlas
# files into one geopackage for each year / change step. To unite the result,
# it is advised to use QGIS (Vector -> Data Management tools -> merge shapefile)
################################################################################

# load dplyr for pipes and convenience functions
library(dplyr)

# load functions
source("C:/Users/labohben/Documents/GitHub/MA/createGPKGfuns.R")

# list all cities compatible with 2006
UA_2006_comp <- 
  read.csv("Z:/UA_names_2006_compatible.csv")

# set-up / create directories
set_up <-
  set.up(
    base_directory = "E:/",  # directory containing folders with zip files
    target_layer = "land-use", # which layer? 1 = land-use, 2 = boundary, 3 = core
    out_name = "06comp.gpkg"#, # desired ending for results file name
    #year = c("2012")  # to set certain years
  )


for (i in 1:ncol(set_up)) {
  
  if (set_up$year[i] %>% grepl("2006", .)) {
    
  combineSHP(input_directory = set_up$indir[i],
             output_directory = set_up$dir[i],
             layer = set_up$layer[i],
             year = set_up$year[i])
    
    } else {
      
  combineGPKG(input_directory = set_up$indir[i],
              output_directory = set_up$dir[i],
              layer = set_up$layer[i],
              year = set_up$year[i])
    }

}
