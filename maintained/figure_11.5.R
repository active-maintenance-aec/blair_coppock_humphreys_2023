# blair_coppock_humphreys_2023 — figure_11.5.R
# Output: figure_11.5.svg
# Output: figure_11.5.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_11.5.R

source(here::here("march_2026_rewrite", "helpers.R"))



diagnosis_11.4 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_11.4.rds"))

CEF_diagnosands <-
  diagnosis_11.4 |>
  get_simulations() |> 
  group_by(estimator, inquiry) |>
  summarise(
    bias_squared = mean(estimate - estimand) ^ 2 ,
    variance =  var(estimate),
    MSE = mean((estimate - estimand) ^ 2)
  ) |>
  summarise(
    `Bias Squared` = mean(bias_squared),
    Variance = mean(variance),
    `Mean Squared Error (Bias Squared + Variance)` = mean(MSE),
    .groups = "drop"
  ) |>
  mutate(estimator = as.numeric(str_remove(estimator, "A")), ) |>
  pivot_longer(-estimator) |>
  mutate(name = factor(
    name,
    levels = c(
      "Bias Squared",
      "Variance",
      "Mean Squared Error (Bias Squared + Variance)"
    )
  ))

label_df <-
  tibble(
    estimator = c(4.5, 1, 2.5),
    value = c(0.01, 0.015, 0.075),
    name = c(
      "Bias Squared",
      "Variance",
      "Mean Squared Error (Bias Squared + Variance)"
    ),
    label = c(
      "Bias Squared",
      "Variance",
      "Mean Squared Error\n(Bias Squared + Variance)"
    ),
    hjust = c(0, 0, 0)
  )

g <-
  ggplot(CEF_diagnosands) +
  aes(estimator, value, color = name) +
  geom_line() +
  geom_text(data = label_df, aes(label = label, hjust = hjust)) +
  scale_x_continuous(breaks = 1:9) +
  scale_color_manual(values = dd_palette("three_color_palette")) +
  theme_dd() +
  labs(x = "Polynomial order", y = "Diagnosand value")

ggsave(here::here("march_2026_rewrite", "output", "figure_11.5.svg"),
       g,
       width = 6.5,
       height = 3.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_11.5.pdf"),
       g,
       width = 6.5,
       height = 3.5)
