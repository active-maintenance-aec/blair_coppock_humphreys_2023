# blair_coppock_humphreys_2023 — figure_18.14.R
# Output: figure_18.14.pdf
# Output: figure_18.14.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_18.14.R

source(here::here("maintained", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)



diagnosis_18.12 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_18.12.rds"))

simulations <- 
  diagnosis_18.12 |> 
  get_simulations()

inquiry_df <-
  diagnosis_18.12 |> 
  get_diagnosands()

g <-
  ggplot(simulations) +
  aes(estimate) +
  geom_histogram(
    aes(y = after_stat(count) / sum(after_stat(count))), 
    fill = dd_palette("dd_light_blue_alpha"),
    color = "transparent",
    binwidth = 0.06) +
  geom_vline(
    data = inquiry_df,
    aes(xintercept = mean_estimand),
    linetype = "dashed",
    color = dd_palette("dd_pink")
  ) +
  geom_text(
    data = inquiry_df,
    aes(x = mean_estimand),
    y = 0.09,
    label = "Estimand",
    nudge_x = 0.05,
    hjust = 0,
    color = dd_palette("dd_pink")
  ) + 
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.1, 0.02)) +
  facet_wrap( ~ estimator) +
  theme_dd() + 
  labs(x = "Simulated effect estimate",
       y = "Percent of simulations")

g

ggsave(here::here("maintained", "output", "figure_18.14.pdf"),
       g,
       width = 6.5,
       height = 3)
ggsave(here::here("maintained", "output", "figure_18.14.svg"),
       g,
       width = 6.5,
       height = 3)
