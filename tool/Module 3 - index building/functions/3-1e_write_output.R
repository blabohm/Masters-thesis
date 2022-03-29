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
# 5.1
write_di <- function(out, folder, ID)
{
  node_out <- paste0(folder, "/nodes_", ID, ".csv")
  out %>%
    select(ID, di) %>%
    filter(!is.infinite(di)) %>%
    mutate(di = round(di, 2)) %>%
    write.csv(node_out, row.names = FALSE)
}


# 5.2
write_ls <- function(out, network, folder, ID)
{
  edge_out <- paste0(folder, "/edges_", ID, ".csv")
  out %>%
    select(sPath, ls) %>%
    mutate(sPath = sapply(sPath, function(x) unlist(x) %>%
                            paste(collapse = " "))) %>%
    tidyr::separate_rows(sPath, sep = " ") %>%
    group_by(sPath) %>%
    summarise(ls = round(sum(ls, na.rm = TRUE))) %>%
    mutate(sPath = as.numeric(sPath),
           edge_id = network$edge_id[sPath]) %>%
    na.omit() %>%
    select(-sPath) %>%
    write.csv(edge_out, row.names = FALSE)
}

