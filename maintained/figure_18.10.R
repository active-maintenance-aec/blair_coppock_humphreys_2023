# blair_coppock_humphreys_2023 — figure_18.10.R
# Output: figure_18.10.pdf
# Output: figure_18.10.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_18.10.R

source(here::here("maintained", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)



diagnosis_18.9 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_18.9.rds"))

inquiries_df <-
  diagnosis_18.9 |>
  get_simulations() |>
  filter(sim_ID == 1) |> 
  mutate(estimator = factor(
    estimator,
    levels = c("Two stage least squares", "As treated", "Per protocol")
  ))

label_df <-
  inquiries_df |>
  filter(estimator == "Two stage least squares") |> 
  mutate(
    x = c(-0.1, 0.8),
    y = 0.075
  )

simulations_df <-
  diagnosis_18.9 |>
  get_simulations() |>
  filter(inquiry == "ATE") |>
  mutate(estimator = factor(
    estimator,
    levels = c("Two stage least squares", "As treated", "Per protocol")
  ))

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
               xintercept = estimand,
               color = inquiry,
               linetype = inquiry
             )) +
  geom_text(data = label_df, aes(
    x = x,
    y = y,
    label = inquiry,
    color = inquiry
  )) +
  scale_color_manual(values = dd_palette("two_color_palette")) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.1, 0.02)) +
  facet_grid(estimator ~ .) +
  theme_dd() + 
  labs(x = "Simulated effect estimate",
       y = "Percent of simulations")

g
ggsave(here::here("maintained", "output", "figure_18.10.pdf"), g, width = 6.5, height = 6.5)
ggsave(here::here("maintained", "output", "figure_18.10.svg"), g, width = 6.5, height = 6.5)

