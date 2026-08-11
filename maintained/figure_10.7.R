# blair_coppock_humphreys_2023 — figure_10.7.R
# Output: figure_10.7.svg
# Output: figure_10.7.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_10.7.R

source(here::here("march_2026_rewrite", "helpers.R"))



diagnosis_10.4 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_10.4.rds"))

simulations <-
  diagnosis_10.4 |>
  get_simulations() |> 
  mutate(significant = as.numeric(p.value <= 0.05))

label_df <- 
  tibble(
    label = c("Conventional power target: 0.8", "Confidence region depicts\nsimulation error"),
    x = c(0.1, 0.275),
    y = c(0.85, 0.3),
    color = rev(dd_palette("two_color_palette")),
    hjust = c(0.5, 0)
  )

g <-
  ggplot(simulations) + 
  aes(bin, estimate) +
  stat_smooth(aes(estimand, significant), method = 'loess', color = dd_palette("dd_dark_blue"), fill = dd_palette("dd_light_blue_alpha"), formula = 'y ~ x') +
  geom_hline(yintercept = 0.8, color = dd_palette("dd_pink"), linetype = "dashed") +
  geom_text(data = label_df, aes(label = label, x = x, y = y, color = color, hjust = hjust)) + 
  scale_color_identity() + 
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  theme_dd() + 
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 0.5)) +
  labs(x = "Model parameter: true effect size",
       y = "Diagnosand: statistical power")

ggsave(here::here("march_2026_rewrite", "output", "figure_10.7.svg"), g, width = 6.5, height = 3.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_10.7.pdf"), g, width = 6.5, height = 3.5)

