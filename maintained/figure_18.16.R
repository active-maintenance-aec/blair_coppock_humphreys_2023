# blair_coppock_humphreys_2023 — figure_18.16.R
# Output: figure_18.16.pdf
# Output: figure_18.16.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_18.16.R

source(here::here("maintained", "helpers.R"))
library(sf)
library(spdep)
library(interference)
library(ggspatial)
library(ggdag)
library(ggraph)
library(ggforce)

library(sf)
library(spdep)
library(interference)
library(ggspatial)


set.seed(343)

adj_matrix <-
  fairfax |>
  as("Spatial") |>
  poly2nb(queen = TRUE) |>
  nb2mat(style = "B", zero.policy = TRUE)

ra_declaration <- declare_ra(N = 238, prob = 0.1)

permutatation_matrix <- 
  ra_declaration |>
  obtain_permutation_matrix(maximum_permutations = 10000) |>
  t()

probs_list <-
  make_exposure_prob(permutatation_matrix,
                     adj_matrix,
                     make_exposure_map_AS,
                     list(hop = 1))

fairfax <-
  fairfax |>
  mutate(
    `Direct and Indirect Exposure` = rowMeans(probs_list$I_exposure$dir_ind1),
    `Direct Exposure Only` = rowMeans(probs_list$I_exposure$isol_dir),
    `Indirect Exposure Only` = rowMeans(probs_list$I_exposure$ind1),
    `No Exposure` = rowMeans(probs_list$I_exposure$no)
  )

gg_df <-
  fairfax |>
  gather("condition", "Probability of assignment", contains("Exposure"))

g <-
ggplot(gg_df) +
  geom_sf(aes(fill = `Probability of assignment`), size = 0.1) +
  facet_wrap(~condition) +
  annotation_scale() +
  annotation_north_arrow(
    height = unit(0.25, units = "in"),
    width = unit(0.25, units = "in"),
    location = "bl", which_north = "true",
    pad_x = unit(0.25, "in"),
    pad_y = unit(0.4, "in"),
    style = north_arrow_fancy_orienteering) +
  theme_dd() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_blank())


# g
ggsave(here::here("maintained", "output", "figure_18.16.pdf"),
       g,
       width = 6.5,
       height = 6.5)
ggsave(here::here("maintained", "output", "figure_18.16.svg"),
       g,
       width = 6.5,
       height = 6.5)
