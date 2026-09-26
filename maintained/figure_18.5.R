# blair_coppock_humphreys_2023 — figure_18.5.R
# Output: figure_18.5.pdf
# Output: figure_18.5.svg
# Depends on: original/diagnosis_objects/, original/code/declarations/, helpers.R
# Description: Maintained rewrite of figure_18.5.R

source(here::here("maintained", "helpers.R"))

library(ggforce)


set.seed(343)

diagnosis_18.5 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_18.5.rds"))
source(here::here("original", "code", "declarations", "declaration_18.5.R"))

designs <- redesign(declaration_18.5, ICC = seq(0.1, 0.9, by = 0.4))

names(designs) <-seq(0.1, 0.9, by = 0.4)

dat <-
  designs |> 
  map_df(draw_data, .id = "ICC")

g1 <-
  ggplot(dat, aes(Y_Z_0, Y_Z_1 - Y_Z_0)) +
  geom_point(stroke = 0, color = dd_palette("dd_gray"), alpha = 0.5) +
  geom_mark_ellipse(
    expand = 0.01,
    radius = 0,
    aes(group = cluster),
    color = dd_palette("dd_light_gray")
  ) +
  facet_wrap(~ ICC, nrow = 1, labeller = label_both) +
  theme_dd() +
  labs(x = "Untreated potential outcome",
       y = "Treatment effect")

g2 <- 
  diagnosis_18.5 |> 
  get_simulations() |> 
  ggplot() +
  aes(estimate) +
  geom_histogram(
    aes(y = after_stat(count) / sum(after_stat(count))),
    fill = dd_palette("dd_light_blue_alpha"),
    color = "transparent",
    binwidth = 0.2
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.1, 0.02)) +
  scale_x_continuous(breaks = seq(-2, 2, 1), limits = c(-2, 2)) +
  facet_wrap(~ICC, nrow = 1, labeller = label_both) +
  theme_dd() +
  labs(x = "Simulated effect estimate",
       y = "Percent of simulations")


g <- g1 / g2

ggsave(here::here("maintained", "output", "figure_18.5.pdf"), g, width = 6.5, height = 6.5)
ggsave(here::here("maintained", "output", "figure_18.5.svg"), g, width = 6.5, height = 6.5)

