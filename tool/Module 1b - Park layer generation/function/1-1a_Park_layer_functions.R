################################################################################
# MODULE 'NUMBER' - MODULE NAME
# PART 'NUMBER' - SUBMODULE NAME
# PART 'NUMBER' + letter - FUNCTIONS THEME
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################
#
# FUNCTIONS:
# 1. FUNCTION NAME
#    -> Function description
#
################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################


################################################################################

sfc2bb <- function(sfc_object)
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
    sf::st_sf() %>%
    return()
}

UAgreen_spaces <- function(directory, output = "directory")
{
  # LOAD PACKAGES AND FUNCTIONS
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  # green space classes
  greenSpaces <- c(14100, 30000, 31000)
  # list files in urban atlas directory
  ua <-
    list.files(indir, full.names = TRUE) %>%
    list.files(pattern = "gpkg$",
               full.names = TRUE,
               recursive = TRUE)
  # generate output layer and file names
  outLayer <- paste0("green_spaces")
  gsDsn <- paste0(tempdir(), "\\", outLayer, ".gpkg")
  #set up progress bar
  pb <- txtProgressBar(min = 0, max = length(ua),
                       initial = 0, style = 3)
  stepi <- 0
  # interate through files
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
      filter(class %in% greenSpaces) %>%
      st_write(dsn = gsDsn, layer = outLayer,
               append = TRUE, quiet = TRUE) }
  if (grepl("dir", output, ignore.case = TRUE)) return(gsDsn) else gsDsn %>%
    st_read(quiet = TRUE) %>%
    return()
}
