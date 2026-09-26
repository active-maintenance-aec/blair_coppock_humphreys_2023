# blair_coppock_humphreys_2023 — figure_16.5.R
# Output: figure_16.5.pdf
# Output: figure_16.5.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_16.5.R

source(here::here("maintained", "helpers.R"))



diagnosis_16.3 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_16.3.rds"))

mutate_step <-
  . %>%  mutate(
    facet = factor(
      estimator,
      levels = c("twoway-fe", "chaisemartin"),
      labels = c("Two-way fixed-effects", "Chaisemartin and D'Haultfoeuille")
    ),
    inquiry = factor(
      inquiry,
      levels = c("ATT", "ATT_switchers"),
      labels = c("ATT", "ATT among\n'switchers'")
    )
  )

inquiries_df <-
  diagnosis_16.3 |>
  tidy() |>
  filter(diagnosand == "mean_estimand") |>
  mutate_step()


label_df <-
  inquiries_df |>
  filter(estimator == "twoway-fe") |>
  mutate(y = 0.06) 

simulations_df <-
  diagnosis_16.3 |>
  get_simulations() |>
  mutate_step()

g <-
  ggplot(simulations_df) +
  aes(estimate) +
  geom_histogram(
    aes(y = after_stat(count) / sum(after_stat(count))),
    fill = dd_palette("dd_light_blue_alpha"),
    color = "transparent",
    binwidth = 0.15
  ) +
  geom_vline(data = inquiries_df,
             aes(
               xintercept = estimate,
               color = inquiry,
               linetype = inquiry
             )) +
  geom_text(data = label_df, aes(
    x = estimate- 0.05,
    y = y,
    label = inquiry,
    color = inquiry
  ),
  hjust = 1) +
  scale_color_manual(values = dd_palette("two_color_palette")) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.1, 0.02)) +
  facet_grid(facet ~ .) +
  theme_dd() + 
  labs(x = "Simulated effect estimate",
       y = "Percent of simulations")

g
ggsave(here::here("maintained", "output", "figure_16.5.pdf"), g, width = 6.5, height = 4.5)
ggsave(here::here("maintained", "output", "figure_16.5.svg"), g, width = 6.5, height = 4.5)

