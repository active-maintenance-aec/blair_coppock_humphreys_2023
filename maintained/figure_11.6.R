# blair_coppock_humphreys_2023 — figure_11.6.R
# Output: figure_11.6.svg
# Output: figure_11.6.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_11.6.R

source(here::here("march_2026_rewrite", "helpers.R"))

library(ggridges)
library(GGally)


diagnosis_11.5 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_11.5.rds"))

gg_df <-
  diagnosis_11.5 |>
  get_simulations() |>
  mutate(estimate_pp = estimate*100,
         permissible = if_else(between(estimate_pp, -100, 100), "Yes", "No"),
         estimator = factor(estimator, levels = c("OLS", "logit", "probit")))

g <-
  ggplot(gg_df) +
  aes(estimate_pp) +
  geom_histogram(
    aes(y = after_stat(count) / sum(after_stat(count))),
    fill = dd_palette("dd_light_blue_alpha"),
    color = "transparent",
    bins = 30
  ) +
  geom_vline(xintercept = c(20), linetype = "dashed", color = "red") +
  facet_grid(~ estimator) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_dd() +
  theme(text = element_text(size = 8))  + 
  labs(x = "Simulated ATE estimates",
       y = "Percent of simulations")

g
ggsave(here::here("march_2026_rewrite", "output", "figure_11.6.svg"),
       g,
       width = 6.5,
       height = 3.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_11.6.pdf"),
       g,
       width = 6.5,
       height = 3.5)
