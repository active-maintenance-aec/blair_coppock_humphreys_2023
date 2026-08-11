# blair_coppock_humphreys_2023 — figure_16.10.R
# Output: figure_16.10.pdf
# Output: figure_16.10.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_16.10.R

source(here::here("march_2026_rewrite", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)

library(geomtextpath)

diagnosis_16.5 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_16.5.rds"))

diangosands_df <-
  diagnosis_16.5 |>
  tidy() |>
  filter(diagnosand %in% c("bias", "sd_estimate", "rmse"),
         estimator == "linear")

labels_df <-
  tibble(
    diagnosand = c("bias", "sd_estimate", "rmse"),
    name = c("Bias", "Standard deviation of the estimate", "Root mean-squared error"),
    offset = c(-0.1, 0.1, 0.1)
  )

gg_df <-
  diangosands_df |>
  left_join(labels_df)

g <-
ggplot(data = gg_df ) +
  aes(bandwidth, estimate, color = diagnosand, group = diagnosand) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = diagnosand, color = NULL), alpha = 0.1) +
  geom_textpath(aes(y = estimate + offset, label = name), text_only = TRUE) +
  scale_color_manual(values = dd_palette("three_color_palette")) +
  scale_fill_manual(values = dd_palette("three_color_palette")) +
  coord_cartesian(xlim = c(0, NA)) +
  theme_dd() +
  labs(x = "Answer strategy parameter: bandwidth", y = "Diagnosand value")

g

ggsave(here::here("march_2026_rewrite", "output", "figure_16.10.pdf"), g, width = 6.5, height = 5)
ggsave(here::here("march_2026_rewrite", "output", "figure_16.10.svg"), g, width = 6.5, height = 5)



