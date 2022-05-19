library(dplyr)
library(sf)
library(ggplot2)
wd <- "C:/Users/labohben/Desktop/DE008/"
id <- "23473-DE008L2"
# rename_cols
# bind_cols
# scenario_cols - base_index_cols
base_indices <- paste0(wd, "base_indices")
base_di <- read_sf(paste0(base_indices, "/di.gpkg")) %>%
  rename(base_di = di) %>% na.omit() %>% st_drop_geometry() %>%
  mutate(base_di = ifelse(base_di > 1, 1, round(base_di, digits = 3)))
base_ls <- read_sf(paste0(base_indices, "/ls.gpkg")) %>%
  rename(base_ls = ls) %>% na.omit() %>% st_drop_geometry()

scenarios <- list.files(wd, pattern = "scenario", full.names = TRUE)
for (scenario in scenarios) {
  read_sf(paste0(scenario, "/di.gpkg")) %>% na.omit() %>%
    mutate(di = ifelse(di > 1, 1, round(di, digits = 3))) %>%
    left_join(base_di, by = "ID") %>% mutate(delta_di = di - base_di) %>%
    write_sf(paste0(scenario, "/delta_di.gpkg"))
  read_sf(paste0(scenario, "/ls.gpkg")) %>%
    na.omit() %>% left_join(base_ls) %>% mutate(delta_ls = ls - base_ls) %>%
    write_sf(paste0(scenario, "/delta_ls.gpkg"))
}

