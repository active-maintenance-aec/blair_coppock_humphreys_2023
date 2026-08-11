# blair_coppock_humphreys_2023 — figure_18.11.R
# Output: figure_18.11.pdf
# Output: figure_18.11.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_18.11.R

source(here::here("march_2026_rewrite", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)

library(geomtextpath)


diagnosis_18.10_encouragment <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_18.10_encouragment.rds"))
diagnosis_18.10_placebo <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_18.10_placebo.rds"))

diagnosands_df <-
  bind_rows(tidy(diagnosis_18.10_encouragment),
            tidy(diagnosis_18.10_placebo)) |>
  filter(diagnosand == "sd_estimate")

label_df <-
  tibble(
    estimator = c("2SLS among all units", "OLS among compliers"),
    label = c("Encouragement design", "Placebo-controlled design"),
    offset = c(0.05, -0.03)
  )

gg_df <-
  diagnosands_df |>
  left_join(label_df) |>
  mutate(y_offset = estimate + offset)

g <-
  ggplot(gg_df) +
  aes(compliance_rate,
      estimate,
      color = estimator,
      shape = estimator) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    fill = estimator,
    color = NULL
  ),
  alpha = 0.1) +
  geom_point() +
  geom_line() +
  geom_textpath(aes(y = y_offset, label = label), linewidth = 0) +
  scale_color_manual(values = dd_palette("two_color_palette")) +
  scale_fill_manual(values = dd_palette("two_color_palette")) +
  theme_dd() +
  labs(y = "Standard error", x = "Compliance rate")

ggsave(here::here("march_2026_rewrite", "output", "figure_18.11.pdf"),
       g,
       width = 6.5,
       height = 5)
ggsave(here::here("march_2026_rewrite", "output", "figure_18.11.svg"),
       g,
       width = 6.5,
       height = 5)
