# blair_coppock_humphreys_2023 — figure_16.4.R
# Output: figure_16.4.svg
# Output: figure_16.4.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_16.4.R

source(here::here("maintained", "helpers.R"))



diagnosis_16.2 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_16.2.rds"))

gg_df <-
  diagnosis_16.2 |>
  get_simulations() 

summary_df <-
  gg_df |>
  group_by(estimator) |>
  summarise(estimand = mean(estimand))

g <-
  ggplot(gg_df) +
  aes(estimate) +
  geom_histogram(
    aes(y = after_stat(count) / sum(after_stat(count))),
    fill = dd_palette("dd_light_blue_alpha"),
    color = "transparent",
    binwidth = 0.05
  ) +
  geom_vline(data = summary_df,
             aes(xintercept = estimand),
             linetype = "dashed") +
  facet_grid(~ estimator) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_dd() +
  theme(text = element_text(size = 8))  + 
  labs(x = "Simulated ATT estimates",
       y = "Percent of simulations")

g
ggsave(here::here("maintained", "output", "figure_16.4.svg"),
       g,
       width = 6.5,
       height = 2.5)
ggsave(here::here("maintained", "output", "figure_16.4.pdf"),
       g,
       width = 6.5,
       height = 2.5)
