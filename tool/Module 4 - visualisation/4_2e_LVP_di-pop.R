library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)
library(ggspatial)
library(cowplot)

github <- "C:/Users/labohben/Documents/GitHub/MA/"
wd <- "C:/Users/labohben/Desktop/DE008/"
DRIVE <- "D:/"
github <- paste0(DRIVE, "MA/")
wd <- paste0(DRIVE,"output/DE008/")

lvp_plot_df <- read_sf(paste0(wd, "scenarios/di_values.gpkg")) %>%
  st_drop_geometry() %>%
  arrange(di) %>%
  transmute(pop_cum = cumsum(population),
            pop_sum = sum(population),
            pop_rel = (pop_cum / pop_sum) * 100,
            di = round(di, digits = 2)) %>%
  group_by(di) %>%
  summarise(pop_rel = mean(pop_rel))

yint <- filter(lvp_plot_df, di == .8) %>%
  pull(pop_rel) %>% max()

p <- lvp_plot_df %>%
  ggplot(aes(x = di, y = pop_rel)) +
  geom_line() +
  #scale_x_continuous(breaks = c(.2, .4, .6, .8, 1)) +
  geom_segment(aes(x = .8, y = yint, xend = .4, yend = yint),
               color = "red", linetype = "dashed") +
  geom_segment(aes(x = .8, y = yint, xend = .8, yend = 0),
               color = "red", linetype = "dashed") +
  #geom_hline(yintercept = yint, color = "red", linetype = "dashed") +
  xlab("DI") + ylab("Population [%]") +
  xlim(c(.4, 1))

ggsave(filename = paste0(github, "/plots/3-2c_di_pop_lvp.pdf"),
         plot = p, height = 11.69 / 4.5,  width = 11.69 / 2.5)
