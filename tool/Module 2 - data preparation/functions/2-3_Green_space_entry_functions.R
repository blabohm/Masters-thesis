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
# city_boundaries <- "E:/citiesEurope/Cities.shp"
# city_code <- "DE001"

proximity_checker1 <- function(city_boundaries, city_code)
{
  # Load required packages
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  # LOAD CITY CORE BOUNDARY
  city_boundary <- boundaryLoader(city_boundaries = city_boundaries,
                                  city_code = city_code,
                                  buffer_dist = 1000)
  # Create filter
  f <- city_boundary %>%
    st_geometry() %>%
    st_as_text()
  # Check for proximity of other cities
  city_boundaries %>%
    st_read(wkt_filter = f,
            quiet = TRUE) %>%
    pull(URAU_CODE) %>%
    substr(1, 5) %>%
    unique() %>%
    return()
}



################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################
# cityBound <- cityBoundLoader(city_boundaries = city_boundaries,
#                              city_code = city_code, code_string = "URAU_CO")
# code_list <- proximity_checker(city_boundaries, city_code)
# ua_directory <- "E:/UA2018"

UAgreen_space <- function(code_list, ua_directory, city_boundaries, city_code,
                          output = "directory", crs = 3035)
{
  # LOAD PACKAGES AND FUNCTIONS
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  # green space classes
  greenSpaces <- c(14100, 30000, 31000)
  # list files in urban atlas directory
  p <- paste(code_list, collapse = "|")
  ua <- ua_directory %>%
    list.files(pattern = p, full.names = TRUE) %>%
    list.files(pattern = ".gpkg$",
               full.names = TRUE,
               recursive = TRUE)
  # User communication
  message("Load green spaces")
  # generate output layer and file names
  outLayer <- paste0("green_spaces")
  gsDsn <- paste0(tempdir(), "\\", outLayer, ".gpkg")
  # load city boundary
  city_boundary <- boundaryLoader(city_boundaries = city_boundaries,
                                  city_code = city_code,
                                  buffer_dist = 1000) %>%
    st_geometry() %>%
    st_as_text()
  # set up progress bar
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
    st_read(uaFile, layer = lyr, wkt_filter = city_boundary , quiet = TRUE) %>%
      st_transform(crs) %>%
      select(city_code = contains("FUA_OR") | contains("fua_code"),
             class = contains("code") & contains("20"),
             contains("identifier"),
             contains("area")) %>%
      filter(class %in% greenSpaces) %>%
      st_write(dsn = gsDsn, layer = outLayer,
               append = TRUE, quiet = TRUE) }
  # check for output style and return
  if (grepl("dir", output, ignore.case = TRUE)) return(gsDsn) else out <-
    gsDsn %>%
    st_read(quiet = TRUE)
  unlink(gsDsn)
  return(out)
}


################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################
# green_spaces <- UAgreen_space(code_list = codeListUA, ua_directory = uaDirectory,
#                               city_boundaries = cityBoundary_file,
#                               city_code = cityCode, output = "sf")
# network <- "C:/Berlin/network_clean1.gpkg"

findGSentries <- function(green_spaces, network, crs = 3035)
{
  # LOAD PACKAGES AND FUNCTIONS
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  # Check inputs
  if (class(green_spaces)[1] == "character") {
    gs <- st_read(green_spaces, quiet = TRUE)
  } else gs <- green_spaces
  if (class(network)[1] == "character") {
    net <- st_read(network, quiet = TRUE)
  } else net <- network
  # User communication
  message("Finding green space entries")
  # First iteration of intersecting network with park outlines
  gsEntries <- gs %>%
    st_buffer(-5) %>%
    st_cast("MULTILINESTRING") %>%
    st_intersection(net)
  # Further iterations
  b <- 0
  while (
    # Check if any parks are missing
    gs %>%
    filter(!(identifier %in% unique(gsEntries$identifier))) %>%
    nrow() > 0 ) {
    # Intersect network with parks and new buffer size
    gsEntries <- gs %>%
      filter(!(identifier %in% unique(gsEntries$identifier))) %>%
      st_buffer(b) %>%
      st_cast("MULTILINESTRING") %>%
      st_intersection(net) %>%
      bind_rows(gsEntries)
    # Increase buffer
    if (b <= 50) {b <- b + 5
    } else if (b <= 100) {b <- b + 10
    } else if (b <= 200) {b <- b + 25
    } else {b <- b + 50}
  }
  gsEntries %>%
    st_cast("POINT") %>%
    return()
}

#gse <- findGSentries(green_spaces, network)
