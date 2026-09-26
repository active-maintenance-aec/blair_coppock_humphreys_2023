# blair_coppock_humphreys_2023 — figure_21.2.R
# Output: figure_21.2.pdf
# Output: figure_21.2.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_21.2.R

source(here::here("maintained", "helpers.R"))



diagnosis_21a <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_21a.rds"))

gg_df <-
  diagnosis_21a |>
  get_simulations() |>
  group_by(prob) |>
  summarise(
    Power = mean(p.value <= 0.05),
    Utility = first(prob),
    acceptable_power = Power >= 0.80,
    acceptable_utility = Utility >= 2 / 3,
    .groups = "drop"
  ) |>
  pivot_longer(c(Power, Utility),
               names_to = "diagnosand")

rect_df <- 
  gg_df |>
  filter(acceptable_power, acceptable_utility) |>
  summarise(min_prob = min(prob),
            max_prob = max(prob))

lines_df <-
  tibble(
    diagnosand = c("Power", "Utility"),
    threshold = c(0.8, 2/3)
  )

annotations_df <-
  tibble(
    diagnosand = c("Power", "Utility"),
    label = c("Researcher constraint: Power must be above 0.80.",
              "Partner constraint: At least 2/3 of subjects must be treated."),
    prob = c(0.2, 0.1),
    value = c(0.725, 0.725)
  )

g <- 
ggplot(gg_df, aes(prob, value)) +
  geom_point() +
  geom_line(alpha = 0.6) +
  geom_text(data = annotations_df,
            aes(label = label),
            hjust = 0) +
  geom_rect(
    data = rect_df,
    aes(
      xmin = min_prob,
      xmax = max_prob,
      ymin = 0,
      ymax = 1
    ),
    alpha = 0.1,
    inherit.aes = FALSE
  ) +
  geom_hline(data = lines_df, aes(yintercept = threshold)) +
  annotate(
    "text",
    x = 17/24,
    y = 0.60,
    label = "Zone of agreement",
    angle = 270,
    hjust = 0
  ) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 6)) +
  scale_x_continuous(breaks = seq(0, 1, length.out = 7),
                     labels = c("0/6", "1/6", "2/6", "3/6", "4/6", "5/6", "6/6")) +
  facet_grid(diagnosand ~ ., scales = "free") +
  labs(x = "Proportion treated") +
  theme_dd() +
  theme(axis.title.y = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave(here::here("maintained", "output", "figure_21.2.pdf"),
       g,
       width = 6.5,
       height = 6.5)
ggsave(here::here("maintained", "output", "figure_21.2.svg"),
       g,
       width = 6.5,
       height = 6.5)
