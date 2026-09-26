# blair_coppock_humphreys_2023 — figure_18.4.R
# Output: figure_18.4.pdf
# Output: figure_18.4.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_18.4.R

source(here::here("maintained", "helpers.R"))

 library(DeclareDesign)


diagnosis_18.4 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_18.4.rds"))

gg_df <-
  diagnosis_18.4 |>
  select(sim_ID, estimator, estimate) |>
  pivot_wider(names_from = estimator, values_from = estimate) |>
  mutate(balanced = balance == 0)

g <-
  ggplot(gg_df) +
  aes(DIM, fill = balanced) +
  geom_histogram(
    aes(y = after_stat(count) / sum(after_stat(count))),
    position = "identity",
    binwidth = 0.11,
    alpha = 0.7
  ) +
  annotate(
    "text",
    x = 0.4,
    y = .04,
    label = "Assignments that\nexactly balance X",
    color = dd_palette("dd_dark_blue"),
    hjust = 0
  ) +
  annotate(
    "text",
    x = -0.1,
    y = .06,
    label = "Assignments that do not\nexactly balance X",
    color = dd_palette("dd_gray"),
    hjust = 1
  ) +
  scale_fill_manual(values = rev(dd_palette("two_color_gray"))) +
  # scale_color_manual(values = dd_palette("two_color_gray")) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.1, 0.02)) +
  theme_dd() +
  theme(axis.title = element_text(size = 9)) +
  labs(x = "Simulated effect estimate",
       y = "Percent of possible random assignments")

g

ggsave(here::here("maintained", "output", "figure_18.4.pdf"), g, width = 6.5, height = 3)
ggsave(here::here("maintained", "output", "figure_18.4.svg"), g, width = 6.5, height = 3)
