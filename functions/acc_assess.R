library(dplyr)
library(sf)
library(ggplot2)

base_dir <- "D:/temp/"

parks <-
  paste0(base_dir, "vali_parks.gpkg") %>% 
  st_read() %>% 
  st_buffer(11)

entrance_paths <-
  paste0(base_dir, "entrance_lines.gpkg") %>% 
  st_read() %>% 
  st_buffer(1) %>% 
  mutate(reference = "y",
         id = paste0("ref", 1:nrow(.))) %>% 
  st_join(parks) %>% 
  select(reference, park = park.y, id)


entrance_files <- 
  list.files(base_dir,
             pattern = "entries*.*.gpkg$",
             full.names = TRUE)

aa_df <- 
  entrance_files[2] %>% 
  st_read() %>% 
  st_join(parks) %>% 
  mutate(classification = "y") %>% 
  st_join(entrance_paths) %>% 
  mutate(reference = if_else(
    is.na(reference), "n", "y")) %>% 
  select(park = park.x,
         reference,
         classification,
         id) %>% 
  bind_rows(entrance_paths %>% 
              filter(!(entrance_paths$id %in% aa_df$id))) 

aa_df <- 
  aa_df %>% 
  mutate(classification = if_else(!is.na(id) & is.na(classification), 
                                 "n", "y"),
         reference = if_else(duplicated(id), "n", "y"))

acc.assess(aa_df)

for (i in entrance_files[2:length(entrance_files)]) {
  
  aa_df <- 
    entrance_files[i] %>% 
    st_read() %>% 
    st_join(parks) %>% 
    mutate(classification = "y") %>% 
    st_join(entrance_paths) %>% 
    mutate(reference = if_else(
      is.na(reference), "n", "y")) %>% 
    select(park = park.x,
           reference,
           classification,
           id) %>% 
    bind_rows(entrance_paths %>% 
                filter(!(entrance_paths$id %in% aa_df$id))) 
  
  aa_df <- 
    aa_df %>% 
    mutate(classification = if_else(!is.na(id) & is.na(classification), 
                                    "n", "y"),
           reference = if_else(duplicated(id), "n", "y"))
  
  acc.assess(aa_df)

}



aa_df %>% 
  select(buffer, park, hit) %>% 
  bind_rows(gcp) %>%
  st_drop_geometry() %>% 
  mutate(hit = if_else(hit, 1, 0)) %>% 
  group_by(buffer, park) %>% 
  summarise(hit = sum(hit)) %>%
  mutate(buffer = gsub("m", "", buffer) %>% as.double()) %>%
  mutate(buffer = if_else(is.na(buffer), 15, buffer)) %>% 
  ggplot(aes(y = hit, x = buffer)) +
  geom_col() +
  facet_wrap(. ~ park)

aa_df %>% 
  select(buffer, park, hit) %>% 
  bind_rows(gcp) %>%
  st_drop_geometry() %>% 
  mutate(hit = if_else(hit, 1, 0)) %>% 
  group_by(buffer, park) %>% 
  summarise(hit = length(hit)) %>%
  mutate(buffer = gsub("m", "", buffer) %>% as.double()) %>%
  mutate(buffer = if_else(is.na(buffer), 15, buffer)) %>% 
  ggplot(aes(y = hit, x = buffer)) +
  geom_col() +
  facet_wrap(. ~ park)
################################################################################

# 
# gcp$park[grepl("v", gcp$name)] <- "Viktoriapark"
# gcp$park[grepl("t[1-9]", gcp$name)] <- "Treptower-Park"
# gcp$park[grepl("tf", gcp$name)] <- "Tempelhofer-Feld"
# 
# paths_filtered <-
#   st_read("E:/temp/paths_filtered.gpkg")
# 
# buffers <- c(0, 5, 10, -5, -10)
# 
# 
# for (x in buffers) {
#   
#   dirBuff <- paste0("E:/temp/parks_buffered_", x, "m.gpkg")
#   dirEntry <- paste0("E:/temp/park_entries_", x, "m.gpkg")
#   
#   parks_buffered <-
#     parks %>% 
#     st_buffer(x) %>%
#     st_cast("MULTILINESTRING")
#   
#   parks_buffered %>% 
#     st_write(dirBuff, append = FALSE)
#   
#   #park_entries
#   st_intersection(parks_buffered, paths_filtered) %>% 
#     st_write(dirEntry, append = FALSE) 
#   
# }
