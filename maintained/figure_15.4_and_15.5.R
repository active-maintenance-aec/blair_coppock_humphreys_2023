# blair_coppock_humphreys_2023 — figure_15.4_and_15.5.R
# Output: figure_15.4.svg
# Output: figure_15.4.pdf
# Output: figure_15.5.svg
# Output: figure_15.5.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_15.4_and_15.5.R

source(here::here("maintained", "helpers.R"))

library(lme4)
library(prediction)


source(here::here("original", "code", "declarations", "declaration_15.4.R"))

gg_df <-
  run_design(declaration_15.4) |>
  mutate(state.abb = state.abb)

g <-
  ggplot(gg_df) + 
  aes(estimand, estimate, label = state.abb) + 
  geom_text() +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "State-level estimand",
       y = "Post-stratified estimate") +
  theme_dd()

ggsave(here::here("maintained", "output", "figure_15.4.svg"), g, width = 6.5, height = 6.5)
ggsave(here::here("maintained", "output", "figure_15.4.pdf"), g, width = 6.5, height = 6.5)

source(here::here("original", "code", "declarations", "declaration_15.5.R"))

diagnosis_15.4 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_15.4.rds"))

gg_df <-
  run_design(declaration_15.5) |> 
  mutate(facet = factor(estimator, levels = c("No pooling",  "Partial pooling", "Full pooling"))) |> 
  left_join(states)

g1 <-
  ggplot(gg_df, aes(estimand, estimate)) +
  geom_point(aes(size = prop_of_US), stroke = 0, color = dd_palette("dd_light_blue_alpha")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c(0.0, 0.5, 1.0)) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c(0.0, 0.5, 1.0)) +
  geom_abline(slope = 1,
              intercept = 0,
              linetype = "dashed") +
  facet_wrap(~ facet, ncol = 1)  +
  theme_dd() +
  labs(x = "State-level estimand",y = "Post-stratified estimate")

g1

by_state <-
  diagnosis_15.4 |>
  get_simulations() |> 
  filter(!is.na(estimate)) |>
  group_by(state, estimator) |>
  summarize(
    estimand = mean(estimand),
    Bias = mean(estimate - estimand),
    `Standard deviation` = sd(estimate),
    `RMSE` = sqrt(mean((estimate - estimand) ^2)),
    .groups = "drop"
  ) |>
  pivot_longer(cols = c(Bias, `Standard deviation`, `RMSE`)) |>
  mutate(estimator = factor(
    estimator,
    levels = c("Estimand", "No pooling",  "Partial pooling", "Full pooling")
  ))

by_state <-
  by_state |> 
  left_join(states, by = "state")

g2 <-
  ggplot(by_state, aes(estimand, value))  +
  geom_point(aes(size = prop_of_US), stroke = 0, color = dd_palette("dd_light_blue_alpha")) +
  facet_grid(estimator ~ name) +
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c(0.0, 0.5, 1.0)) +
  theme_dd() +
  labs(x = "State-level estimand", y = "Diagnosand value")

g <- wrap_plots(list(g1, g2), widths = c(1, 3))

ggsave(here::here("maintained", "output", "figure_15.5.svg"), g, width = 6.5, height = 5)
ggsave(here::here("maintained", "output", "figure_15.5.pdf"), g, width = 6.5, height = 5)



