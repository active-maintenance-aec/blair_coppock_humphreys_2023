# blair_coppock_humphreys_2023 — figure_18.13.R
# Output: figure_18.13.pdf
# Output: figure_18.13.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_18.13.R

source(here::here("march_2026_rewrite", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)

library(geomtextpath)


diagnosis_18.11 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_18.11.rds"))


gg_df <- 
  diagnosis_18.11 |> 
  tidy() |> 
  filter(diagnosand == "power") |>
  mutate(design = c(rep("Stepped wedge", 16), rep("Two-arm experiment", 16 *2)),
         design_size = paste0(design, " (N = ", n_units, ")"))

g <-
  ggplot(gg_df) +
  aes(effect_size, estimate, color = design_size, group = design_size) +
  geom_hline(yintercept = 0.8,
             lty = "dashed",
             color = gray(0.2)) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    fill = design_size,
    color = NULL
  ),
  alpha = 0.15) +
  geom_textpath(aes(label = design_size)) +
  annotate(
    "text",
    x = 0,
    y = 0.825,
    hjust = 0,
    label = "Power threshold = 0.8",
    color = gray(0.2)
  ) +
  scale_color_manual(values = dd_palette("three_color_palette")) +
  scale_fill_manual(values = dd_palette("three_color_palette")) +
  labs(x = "Expected effect size", y = "Statistical power") +
  theme_dd()

ggsave(here::here("march_2026_rewrite", "output", "figure_18.13.pdf"),
       g,
       width = 6.5,
       height = 4)
ggsave(here::here("march_2026_rewrite", "output", "figure_18.13.svg"),
       g,
       width = 6.5,
       height = 4)
