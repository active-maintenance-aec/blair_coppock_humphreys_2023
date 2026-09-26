# blair_coppock_humphreys_2023 — figure_11.1.R
# Output: figure_11.1.svg
# Output: figure_11.1.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_11.1.R

source(here::here("maintained", "helpers.R"))



diagnosis_11.1 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_11.1.rds"))

label_df <- 
  tibble(
    label = c("Conventional power target: 0.8"),
    x = 5,
    y = c(0.85),
    color = dd_palette("dd_pink"),
    hjust = 0
  )

gg_df <- 
  diagnosis_11.1 |>
  get_simulations() |> 
  filter(sim_ID < 500) |> 
  mutate(significant = as.numeric(p.value <= 0.05))

g <- 
  ggplot(gg_df) + 
  aes(N, significant) +
  stat_smooth(method = 'loess', color = dd_palette("dd_dark_blue"), fill = dd_palette("dd_light_blue_alpha"), formula = 'y ~ x') +
  geom_hline(yintercept = 0.8, color = dd_palette("dd_pink"), linetype = "dashed") +
  geom_text(data = label_df, aes(label = label, x = x, y = y, color = color, hjust = hjust)) + 
  scale_color_identity() + 
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  theme_dd() +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 1000)) + 
  labs(x = "Data strategy parameter: sample size",
       y = "Diagnosand: statistical power") 

ggsave(here::here("maintained", "output", "figure_11.1.svg"), g, width = 6.5, height = 3.5)
ggsave(here::here("maintained", "output", "figure_11.1.pdf"), g, width = 6.5, height = 3.5)

