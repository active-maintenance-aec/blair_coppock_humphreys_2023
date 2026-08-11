# blair_coppock_humphreys_2023 — figure_18.15.R
# Output: figure_18.15.pdf
# Output: figure_18.15.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_18.15.R

source(here::here("march_2026_rewrite", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)

library(sf)
library(ggspatial)
library(grid)

fairfax <-
  fairfax |> 
  filter(PREC_IDENT != 700)

g <- 
  ggplot(fairfax) + 
  geom_sf() +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank()
  ) + 
  annotation_scale() + 
  annotation_north_arrow(
    height = unit(0.25, units = "in"),
    width = unit(0.25, units = "in"),
    location = "bl", which_north = "true", 
    pad_x = unit(0.25, "in"),
    pad_y = unit(0.4, "in"),
    style = north_arrow_fancy_orienteering) 

g

ggsave(here::here("march_2026_rewrite", "output", "figure_18.15.pdf"),
       g,
       width = 6.5,
       height = 6.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_18.15.svg"),
       g,
       width = 6.5,
       height = 6.5)
