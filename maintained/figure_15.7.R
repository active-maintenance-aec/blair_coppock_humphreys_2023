# blair_coppock_humphreys_2023 — figure_15.7.R
# Output: figure_15.7.svg
# Output: figure_15.7.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_15.7.R

source(here::here("maintained", "helpers.R"))



diagnosis_15.5 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_15.5.rds"))

gg_df <-
  diagnosis_15.5 |>
  get_simulations() |>
  mutate(outcome = factor(
    outcome,
    levels = c("Y_first_factor", "Y_avg", "Y_avg_rescaled", "Y_avg_adjusted"),
    labels = c(
      "Principal Components",
      "Average",
      "Average (rescaled)",
      "Average (adjusted)"
    )
  ))

summary_df <-
  gg_df |>
  group_by(outcome) |>
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
  facet_grid(~ outcome) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.25, 0.05)) +
  theme_dd() +
  theme(text = element_text(size = 8))  + 
  labs(x = "Simulated estimates of the average outcomes among units with X = 1",
       y = "Percent of simulations")


ggsave(here::here("maintained", "output", "figure_15.7.svg"),
       g,
       width = 6.5,
       height = 2.5)
ggsave(here::here("maintained", "output", "figure_15.7.pdf"),
       g,
       width = 6.5,
       height = 2.5)
