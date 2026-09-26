# blair_coppock_humphreys_2023 — figure_13.1.R
# Output: figure_13.1.pdf
# Output: figure_13.1.svg
# Depends on: helpers.R
# Description: Maintained rewrite of figure_13.1.R

source(here::here("maintained", "helpers.R"))
library(geomtextpath)


set.seed(343)

M <-
  declare_model(
    N = 1000,
    X1 = rnorm(N, mean = 5, sd = 2),
    X2 = runif(N, min = 0, max = 5),
    X3 = rbinom(N, size = 1, prob = 0.5),
    X4 = rbinom(N, size = 5, prob = 0.5),
    X5 = rlnorm(N, meanlog = 0, sdlog = 1),
    X6 = sample(c(1, 2, 3, 4, 5), N, replace = TRUE)
  ) 

gg_df <-
  M() |>
  pivot_longer(cols = starts_with("X")) |>
  mutate(facet_label = factor(
    name,
    levels = paste0("X", 1:6),
    labels = c(
      "rnorm(N, mean = 5, sd = 2)",
      "runif(N, min = 0, max = 5)",
      "rbinom(N, size = 1, prob = 0.5)",
      "rbinom(N, size = 5, prob = 0.5)",
      "rlnorm(N, meanlog = 0, sdlog = 1)",
      "sample(c(1, 2, 3, 4, 5), N, replace = TRUE)"
    )
  ))

g <- 
  ggplot(gg_df) +
  aes(value) +
  geom_histogram(
    aes(y = after_stat(count) / sum(after_stat(count))),
    fill = dd_palette("dd_light_blue_alpha"),
    color = "transparent",
    bins = 30
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.1, 0.02)) +
  theme_dd() +
  theme(strip.text = element_text(size = 7)) +
  facet_wrap( ~ facet_label, scales = "free_x") +
  labs(x = "Characteristics of units",
       y = "Percent of units")

ggsave(here::here("maintained", "output", "figure_13.1.pdf"), g, width = 6.5, height = 6.5)
ggsave(here::here("maintained", "output", "figure_13.1.svg"), g, width = 6.5, height = 6.5)
