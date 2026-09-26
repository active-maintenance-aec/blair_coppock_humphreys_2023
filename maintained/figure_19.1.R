# blair_coppock_humphreys_2023 — figure_19.1.R
# Output: figure_19.1.pdf
# Output: figure_19.1.svg
# Depends on: original/code/declarations/, helpers.R
# Description: Maintained rewrite of figure_19.1.R

source(here::here("maintained", "helpers.R"))
library(grf)


set.seed(343)

source(here::here("original", "code", "declarations", "declaration_19.1.R"))

gg_df <- draw_data(declaration_19.1) 

g <-
  gg_df |>
  mutate(set = ifelse(train, "training set", "testing set")) |>
  ggplot() + 
  aes(tau, pred, color = set) + 
  geom_point(alpha = 0.8) + 
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(values = dd_palette("two_color_palette")) +
  xlab("True unit level causal effects")  + 
  ylab("Predicted unit level effects") +
  coord_fixed(xlim = c(-5, 15), ylim = c(-1,8)) + 
  theme_dd() +
  theme(legend.position = "bottom")

ggsave(here::here("maintained", "output", "figure_19.1.pdf"),
       g,
       width = 6.5,
       height = 3.5)
ggsave(here::here("maintained", "output", "figure_19.1.svg"),
       g,
       width = 6.5,
       height = 3.5)
