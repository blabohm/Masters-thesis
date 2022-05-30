library(dplyr)
library(sf)
library(ggplot2)
wd <- "D:/output/DE008/scenarios/"
ls_scen <- list.files(wd, pattern = "ls", full.names = TRUE)
di_scen <- list.files(wd, pattern = "di", full.names = TRUE)

ls_query <- paste0("SELECT * FROM ", st_layers(ls_scen[1])$name[1],
                   " WHERE ls is not null")
ls_df <- read_sf(ls_scen[1], query = ls_query) %>%
  mutate(scenario = "base")
for (ls_file in ls_scen) {
  lyr <- st_layers(ls_file)$name[1]
  ls_query <- paste0("SELECT * FROM ", lyr,
                     " WHERE ls is not null")
  ls_temp <- read_sf(ls_file, query = ls_query) %>%
    mutate(scenario = lyr)
  ls_df <- bind_rows(ls_df, ls_temp)
}

ls_values <- ls_df %>%
  st_drop_geometry() %>%
  tidyr::pivot_wider(names_from = scenario, values_from = ls) %>%
  mutate(d_ls1 = ls1 - base,
         d_ls2 = ls2 - base,
         d_ls3 = ls3 - base,
         d_ls4 = ls4 - base) %>%
  left_join(., read_sf(paste0(wd, "edges.gpkg")))

write_sf(ls_values, paste0(wd,  "ls_values.gpkg"))


di_query <- paste0("SELECT * FROM ", st_layers(di_scen[1])$name[1],
                   " WHERE di is not null")
di_df <- read_sf(di_scen[1], query = di_query) %>%
  mutate(scenario = "base")
for (di_file in di_scen) {
  lyr <- st_layers(di_file)$name[1]
  di_query <- paste0("SELECT * FROM ", lyr,
                     " WHERE di is not null")
  di_temp <- read_sf(di_file, query = di_query) %>%
    mutate(scenario = lyr)
  di_df <- bind_rows(di_df, di_temp)
}

di_values <- di_df %>%
  st_drop_geometry() %>%
  tidyr::pivot_wider(names_from = scenario, values_from = di) %>%
  mutate(d_di1 = di1 - base,
         d_di2 = di2 - base,
         d_di3 = di3 - base,
         d_di4 = di4 - base) %>%
  left_join(., read_sf(paste0(gsub("scenarios/", "", wd), "buildings.gpkg")))

write_sf(di_values, paste0(wd,  "di_values.gpkg"))

# id <- "23473-DE008L2"
# # rename_cols
# # bind_cols
# # scenario_cols - base_index_cols
# base_indices <- paste0(wd, "base_indices")
# base_di <- read_sf(paste0(base_indices, "/di.gpkg")) %>%
#   rename(base_di = di) %>% na.omit() %>% st_drop_geometry() %>%
#   mutate(base_di = ifelse(base_di > 1, 1, round(base_di, digits = 3)))
# base_ls <- read_sf(paste0(base_indices, "/ls.gpkg")) %>%
#   rename(base_ls = ls) %>% na.omit() %>% st_drop_geometry()
#
# scenarios <- list.files(wd, pattern = "scenario", full.names = TRUE)
# for (scenario in scenarios) {
#   read_sf(paste0(scenario, "/di.gpkg")) %>% na.omit() %>%
#     mutate(di = ifelse(di > 1, 1, round(di, digits = 3))) %>%
#     left_join(base_di, by = "ID") %>% mutate(delta_di = di - base_di) %>%
#     write_sf(paste0(scenario, "/delta_di.gpkg"))
#   read_sf(paste0(scenario, "/ls.gpkg")) %>%
#     na.omit() %>% left_join(base_ls) %>% mutate(delta_ls = ls - base_ls) %>%
#     write_sf(paste0(scenario, "/delta_ls.gpkg"))
# }
#
