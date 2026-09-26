# blair_coppock_humphreys_2023 — figure_18.17.R
# Output: figure_18.17.pdf
# Output: figure_18.17.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_18.17.R

source(here::here("maintained", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)



diagnosis_18.13 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_18.13.rds"))

label_mutate <-
  . %>% 
  mutate(
    estimator = paste0("Estimator: ", estimator),
    inquiry = factor(inquiry, 
                     levels = c("direct_ATE", "indirect_ATE", "total_ATE"),
                     labels = c("Inquiry:\nATE for direct effects", "Inquiry:\nATE for indirect effects", "Inquiry:\nDirect + indirect ATE"))
  )

gg_df <-
  diagnosis_18.13 |> 
  get_simulations() |> 
  label_mutate()

inquiry_df <-
  diagnosis_18.13 |>
  tidy() |>
  filter(diagnosand == "mean_estimand") |>
  label_mutate()

g <-
  ggplot(gg_df) +
  aes(estimate) +
  geom_histogram(
    aes(y = after_stat(count) / sum(after_stat(count))), 
    fill = dd_palette("dd_light_blue_alpha"),
    color = "transparent",
    binwidth = 0.02) +
  geom_vline(
    data = inquiry_df,
    aes(xintercept = estimate),
    linetype = "dashed",
    color = dd_palette("dd_pink")
  ) +
  geom_text(
    data = filter(inquiry_df, inquiry == "Inquiry:\nATE for direct effects"),
    aes(x = estimate),
    y = 0.05,
    label = "Estimand",
    nudge_x = 0.05,
    hjust = 0,
    color = dd_palette("dd_pink")
  ) + 
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.1, 0.02)) +
  facet_grid(inquiry ~ estimator) +
  theme_dd() + 
  labs(x = "Simulated effect estimate",
       y = "Percent of simulations")

g

ggsave(here::here("maintained", "output", "figure_18.17.pdf"),
       g,
       width = 6.5,
       height = 6.5)
ggsave(here::here("maintained", "output", "figure_18.17.svg"),
       g,
       width = 6.5,
       height = 6.5)
