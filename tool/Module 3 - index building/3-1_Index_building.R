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

# LOAD PACKAGES AND FUNCTIONS
require(dplyr, quietly = TRUE)
require(sf, quietly = TRUE)
getwd() %>%
  paste0("/tool/Module 3 - index building/functions/") %>%
  list.files(pattern = "3-1[A-Za-z].*\\.R", full.names = TRUE) %>%
  for (file in .) source(file)

################################################################################
# INPUT VALUES FOR TESTING CODE

# DATA DIRECTORIES
drive <- "D:/"
nodeDir <- paste0(drive, "Berlin/nodes.gpkg")
edgeDir <- paste0(drive, "Berlin/edges.gpkg")
# FUA CITY CODE

getIndices(node_directory = nodeDir,
          edge_directory = edgeDir)
################################################################################
getIndices <- function(node_directory, edge_directory)
  # LOAD NODES, FILTER FOR GREEN SPACE ENTRIES AND GET IDENTIFIER VALUES
  gs_IDs <- node_directory %>%
  st_read(query = "SELECT identifier FROM nodes WHERE identifier is not null",
          quiet = TRUE) %>%
  pull(identifier) %>%
  unique()
# ITERATE THROUGH GREEN SPACE IDs AND CREATE IDICES - WRITE OUTPUT TO TEMP FILES

# UNITE OUTPUT TO ONE LAYER PER INDEX
