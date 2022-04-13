################################################################################
# MODULE 'NUMBER' - MODULE NAME
# PART 'NUMBER' - SUBMODULE NAME
################################################################################

# INPUT:
# - INPUT NAME

# OUTPUT:
# - OUTPUT NAME

################################################################################
# OVERVIEW:
# 1. DESCRIPTION OF STEPS
################################################################################


################################################################################
# INPUT VALUES FOR TESTING CODE

# DATA DIRECTORIES
#working_directory <- "D:/temp/"
################################################################################
getIndices <- function(working_directory,
                       output = paste0(working_directory, "indices/"),
                       nodes = paste0(working_directory, "nodes.gpkg"),
                       edges = paste0(working_directory, "edges.gpkg"),
                       buildings = paste0(working_directory, "buildings.gpkg")
                       )
{
  # LOAD PACKAGES AND FUNCTIONS
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  getwd() %>%
    paste0("/tool/Module 3 - index building/functions/") %>%
    list.files(pattern = "3-1[A-Za-z].*\\.R", full.names = TRUE) %>%
    for (file in .) source(file)
  # LOAD NODES, FILTER FOR GREEN SPACE ENTRIES AND GET IDENTIFIER VALUES
  green_space_IDs <- nodes %>%
    st_read(query = "SELECT identifier FROM nodes WHERE identifier is not null",
            quiet = TRUE) %>%
    pull(identifier) %>%
    unique()
  # ITERATE THROUGH GREEN SPACE IDs AND CREATE IDICES - WRITE OUTPUT TO TEMP FILES
  calcIndices(green_space_IDs = green_space_IDs,
              folder = working_directory,
              perc_core = .5,
              d = 500)
  # UNITE OUTPUT TO ONE LAYER PER INDEX
  gatherDI(building_polygons = buildings,
           index_dir = output,
           output_dir = gsub("indices/", "detour_index.gpkg", output))
  gatherLS(edges = edges,
           index_dir = output,
           output_dir = gsub("indices/", "local_significance.gpkg", output))
}


################################################################################
# END OF DOCUMENT
################################################################################

