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

UAdir <- "E:/UA2018/"

################################################################################
# 1. FUNCTION DESCRIPTION (SHORT)
# REQUIRED SETTINGS:
# setting_name: Setting description
# OPTIONAL SETTINGS:
# setting_name: Setting description - DEFAULT values
################################################################################



################################################################################

# sfc2bb <- function(sfc_object)
# {
#   require(sf, quietly = TRUE)
#   require(dplyr, quietly = TRUE)
#   bb <- st_bbox(sfc_object)
#
#   list(rbind(c(bb$xmin, bb$ymin),
#              c(bb$xmax, bb$ymin),
#              c(bb$xmax, bb$ymax),
#              c(bb$xmin, bb$ymax),
#              c(bb$xmin, bb$ymin))) %>%
#     sf::st_polygon() %>%
#     sf::st_sfc() %>%
#     sf::st_sf() %>%
#     return()
# }
#
# UAbbox <- function(ua_directory)
# {
#   require(sf, quietly = TRUE)
#   require(dplyr, quietly = TRUE)
#   l <-  ua_directory %>%
#     list.files(full.names = TRUE) %>%
#     list.files(pattern = ".gpkg$",
#                full.names = TRUE,
#                recursive = TRUE)
#   tmp <- paste0(tempdir(), "/bbox.gpkg")
#   for (ua_file in l) {
#     lyr <- st_layers(ua_file)$name[]
#     st_read(ua_file)
#   }
#
# }