library(dplyr)
library(sf)
library(ggplot2)

getwd() %>%
  paste0("/tool/Module 2 - data preparation/functions/") %>%
  list.files(pattern = "2-2[A-Za-z].*\\.R|2_.*\\.R", full.names = TRUE) %>%
  for (file in .) source(file)

wd <- "Z:/output/"
cities <- list.files(wd)
ua_directory <- gsub("output/", "input/UA2018/", wd)
city_boundaries <- gsub("output/", "input/cities.gpkg", wd)

# perc_coverage <- tibble()
# for (city_code in cities[18:832]) {
#   print(paste(which(cities == city_code), "of", length(cities)))
#   try({
#   city_boundary <- boundaryLoader(city_boundaries = city_boundaries,
#                                   city_code = city_code)
#   UAresidential <- UAresLoader(ua_dir = ua_directory,
#                                fua_code = city_boundary$FUA_CODE,
#                                boundary = city_boundary) %>%
#     filter(Pop2018 > 2)
#   osm_dir <- paste0(wd, city_code, "/buildings.gpkg")
#   OSMresidential <- read_sf(osm_dir)
#   covered <- st_covers(x = UAresidential, y = OSMresidential)
#   N <- nrow(UAresidential)
#   i <- nrow(UAresidential[lapply(covered, length) > 0,])
#   i/N
#   perc_coverage <- tibble(city_code = city_code, percent_coverage = i / N) %>%
#     bind_rows(perc_coverage)
#   })
# }

#city_code <- cities[157]
out <- tibble()
for (city_code in cities) {
  print(paste(which(cities == city_code), "of", length(cities)))
  try({
  #  system.time({
      gsq <- "SELECT * FROM nodes WHERE identifier is not null"
      gse <- list.files(wd, pattern = city_code, full.names = TRUE) %>% 
        paste0(., "/nodes.gpkg") %>% 
        read_sf(query = gsq) %>%
        st_buffer(1)  
      
      # edges
      lsq <- "SELECT * FROM local_significance WHERE ls is not null"
      ls <- list.files(wd, pattern = city_code, full.names = TRUE) %>% 
        paste0(., "/local_significance.gpkg") %>% 
        read_sf(query = lsq)
      
      out <- st_join(gse, ls, left = TRUE) %>% 
        st_drop_geometry() %>% 
        filter(!is.na(ls)) %>% 
        group_by(identifier) %>% 
        summarise(ls = sum(ls)) %>% 
        mutate(city_code = city_code) %>%
        # group_by(city_code) %>% 
        # summarise(ls_mean = mean(ls),
        #           ls = sum(ls), 
        #           n_parks_ls = n()) %>% 
        # mutate(n_parks = length(unique(gse$identifier))) %>% 
        bind_rows(out, .)
   # })

  })
}

write.csv(out, "Z:/ls_full.csv", row.names = FALSE)


city_info <- read.csv("Z:/city_info.csv") %>%
  tibble() %>%
  mutate(city_code = substr(URAU_COD_1, 1, 5),
         URAU_NAME = ifelse(city_code == "XK003", "Mitrovica", URAU_NAME))

perc_coverage <- read.csv("Z:/MA/percent_OSM_coverage.csv")
city_sf <- read_sf(city_boundaries) %>%
  rename(city_code = URAU_CODE) %>%
  left_join(perc_coverage) %>%
  left_join(city_info, by = "city_code")

nuts <- read_sf("Z:/nuts/NUTS_RG_20M_2021_3035.gpkg") %>%
  filter(LEVL_CODE == 0) %>%
  select(NUTS_ID)

ggplot() +
  geom_sf(data = nuts) +
  geom_sf(data = city_sf, aes(col = percent_coverage, fill = percent_coverage)) +
  labs(title = "Percent of UA residential class polygons covered by OSM buildings",
       color = "Percent coverage") +
  guides(fill = "none") +
  theme(legend.position = "bottom") +
  # scale_color_viridis_d(begin = 0, end = 1) +
  coord_sf(xlim = c(2700000, 5748970),
           ylim = c(1500000, 4500000))

