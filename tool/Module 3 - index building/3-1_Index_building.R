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
# drive <- "D:/temp/"
# nodeDir <- paste0(drive, "/nodes.gpkg")
# edgeDir <- paste0(drive, "/edges.gpkg")
# FUA CITY CODE

getIndices(node_directory = nodeDir,
           edge_directory = edgeDir,
           building_directory = buildEntries,
           output_directory = indexDir)
################################################################################
getIndices <- function(node_directory, edge_directory,
                       building_directory, output_directory)
{
  # LOAD PACKAGES AND FUNCTIONS
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  getwd() %>%
    paste0("/tool/Module 3 - index building/functions/") %>%
    list.files(pattern = "3-1[A-Za-z].*\\.R", full.names = TRUE) %>%
    for (file in .) source(file)
  # LOAD NODES, FILTER FOR GREEN SPACE ENTRIES AND GET IDENTIFIER VALUES
  gs_IDs <- node_directory %>%
    st_read(query = "SELECT identifier FROM nodes WHERE identifier is not null",
            quiet = TRUE) %>%
    pull(identifier) %>%
    unique()
  # ITERATE THROUGH GREEN SPACE IDs AND CREATE IDICES - WRITE OUTPUT TO TEMP FILES
  calcIndices(green_space_IDs = gs_IDs,
              nodes = node_directory,
              edges = edge_directory,
              output = output_directory,
              perc_core = .5,
              d = 500)
  # UNITE OUTPUT TO ONE LAYER PER INDEX
  gatherDI(building_polygons = building_directory,
           index_dir = output_directory,
           output_dir = gsub("/indices/", "detour_index.gpkg", output_directory))
  gatherLS(edges = edge_directory,
           index_dir = output_directory,
           output_dir = gsub("/indices/", "local_significance.gpkg", output_directory))
}


################################################################################
# END OF DOCUMENT
################################################################################

