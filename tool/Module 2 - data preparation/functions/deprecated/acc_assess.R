
# load packages
library(tidyverse)
library(sf)
library(ggplot2)

# base directory
base_dir <- "E:/temp/"

# load park layer and buffer
parks <-
  paste0(base_dir, "vali_parks.gpkg") %>% 
  st_read() %>% 
  st_buffer(11)

# load the "reference data"
entrance_paths <-
  paste0(base_dir, "entrance_lines.gpkg") %>% 
  st_read() %>% 
  st_buffer(1) %>% 
  mutate(reference = "y",
         id = paste0("ref", 1:nrow(.))) %>% 
  st_join(parks) %>% 
  select(reference, park = park.y, id)

# list park entry files created with different buffers
entrance_files <- 
  list.files(base_dir,
             pattern = "entries*.*.gpkg$",
             full.names = TRUE)

# data wrangling for accuracy assessment
aa_df <- 
  entrance_files[1] %>% 
  st_read() %>% 
  st_join(parks) %>% 
  mutate(classification = "y") %>% 
  st_join(entrance_paths) %>% 
  mutate(reference = if_else(
    is.na(reference), "n", "y")) %>% 
  select(park = park.x,
         reference,
         classification,
         id) 
aa_df <- 
  aa_df %>%
  bind_rows(filter(entrance_paths, !(entrance_paths$id %in% aa_df$id))) %>% 
  mutate(classification = if_else(!is.na(id) & is.na(classification), 
                                  "n", "y"),
         reference = if_else(duplicated(id), "n", "y"),
         reference = if_else(is.na(id) & reference == "y", "n", reference)
  ) %>% 
  mutate(buffer = 0)  %>% 
  mutate(classification = classification %>% gsub("y", "j", .),
         reference = reference %>% gsub("y", "j", .)) 

# accuracy assessment table
out_df <-
  tibble(
    buffer = 0,
    overall_acc = acc.assess(class_data = aa_df$classification, 
                             ref_data = aa_df$reference, out = "oa"),
    producer_acc = acc.assess(class_data = aa_df$classification, 
                              ref_data = aa_df$reference, out = "prod"),
    commission_err = acc.assess(class_data = aa_df$classification, 
                                ref_data = aa_df$reference, out = "com")
  )

for (i in entrance_files[2:length(entrance_files)]) {
  
  buffer_size <- 
    i %>% 
    gsub("E:/temp/park_entries_", "", .) %>% 
    gsub("m.gpkg", "", .) %>% as.double()
  
  temp_df <- 
    i %>% 
    st_read() %>% 
    st_join(parks) %>% 
    mutate(classification = "y") %>% 
    st_join(entrance_paths) %>% 
    mutate(reference = if_else(
      is.na(reference), "n", "y")) %>% 
    select(park = park.x,
           reference,
           classification,
           id) 

  temp_df <- 
    temp_df %>% 
    bind_rows(filter(entrance_paths, !(entrance_paths$id %in% temp_df$id))) %>% 
    mutate(classification = if_else(!is.na(id) & is.na(classification), 
                                    "n", "y"),
           reference = if_else(duplicated(id), "n", "y")) %>% 
    mutate(buffer = buffer_size) %>% 
    mutate(classification = classification %>% gsub("y", "j", .),
           reference = reference %>% gsub("y", "j", .)) 

  aa_df <-
    bind_rows(aa_df, temp_df)
  # acc.assess(class_data = temp_df$classification, 
  #            ref_data = temp_df$reference) %>% 
  #   print()
  
  out_df <-
    tibble(
    buffer = buffer_size,
    overall_acc = acc.assess(class_data = temp_df$classification, 
                             ref_data = temp_df$reference, out = "oa"),
    producer_acc = acc.assess(class_data = temp_df$classification, 
                              ref_data = temp_df$reference, out = "prod"),
    commission_err = acc.assess(class_data = temp_df$classification, 
                                ref_data = temp_df$reference, out = "com")
  ) %>% 
    bind_rows(out_df)


}

mean_line <-
  tibble(
    buffer = "total",
    "1 Overall Accuracy" = acc.assess(class_data = aa_df$classification, 
                                      ref_data = aa_df$reference, out = "oa"),
    "2 Producer Accuracy" = acc.assess(class_data = aa_df$classification, 
                                       ref_data = aa_df$reference, out = "prod"),
    "3 Commission Error" = acc.assess(class_data = aa_df$classification, 
                                      ref_data = aa_df$reference, out = "com")
  ) %>% 
  mutate(`1 Overall Accuracy` = `1 Overall Accuracy` * 100) %>%
  pivot_longer(cols = `1 Overall Accuracy`:`3 Commission Error`) 

out_df %>% 
  mutate(overall_acc = overall_acc * 100) %>% 
  rename("1 Overall Accuracy" = overall_acc,
         "2 Producer Accuracy" = producer_acc,
         "3 Commission Error" = commission_err) %>% 
  pivot_longer(cols = `1 Overall Accuracy`:`3 Commission Error`) %>% 
  ggplot(aes(x = buffer)) +
  geom_line(aes(y = value)) +
  geom_abline(data = mean_line, aes(intercept = value, slope = 0), 
              color = "red") +
  facet_wrap(. ~ name, scales = "free") +
  labs(x = "buffer size [m]", y = "percent [%]",
       title = "Accuracy statistics: park entries")

  

  




# aa_df %>% 
#   st_drop_geometry() %>% 
#   group_by(buffer) %>% 
#   group_map(~ acc.assess(class_data = .$classification, 
#                             ref_data = .$reference, out = "oa"))
# 
# aa_df %>% 
#   st_drop_geometry() %>% 
#   group_by(buffer) %>% 
#   summarise(OA = acc.assess(class_data = .$classification, 
#                             ref_data = .$reference, out = "oa"))
# 
# aa_df %>% 
#   st_drop_geometry() %>% 
#   group_by(buffer) %>% 
#   summarise(OA = acc.assess(class_data = .$classification, 
#                             ref_data = .$reference, out = "prod"))
# 
# aa_df %>% 
#   filter(buffer == 0) %>%  
#   mutate(classification = classification %>% gsub("y", "j", .),
#                                    reference = reference %>% gsub("y", "j", .)) %>% 
#   acc.assess(class_data = .$classification, ref_data = .$reference) 


# aa_df %>% 
#   select(buffer, park, hit) %>% 
#   bind_rows(gcp) %>%
#   st_drop_geometry() %>% 
#   mutate(hit = if_else(hit, 1, 0)) %>% 
#   group_by(buffer, park) %>% 
#   summarise(hit = sum(hit)) %>%
#   mutate(buffer = gsub("m", "", buffer) %>% as.double()) %>%
#   mutate(buffer = if_else(is.na(buffer), 15, buffer)) %>% 
#   ggplot(aes(y = hit, x = buffer)) +
#   geom_col() +
#   facet_wrap(. ~ park)
# 
# aa_df %>% 
#   select(buffer, park, hit) %>% 
#   bind_rows(gcp) %>%
#   st_drop_geometry() %>% 
#   mutate(hit = if_else(hit, 1, 0)) %>% 
#   group_by(buffer, park) %>% 
#   summarise(hit = length(hit)) %>%
#   mutate(buffer = gsub("m", "", buffer) %>% as.double()) %>%
#   mutate(buffer = if_else(is.na(buffer), 15, buffer)) %>% 
#   ggplot(aes(y = hit, x = buffer)) +
#   geom_col() +
#   facet_wrap(. ~ park)
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
