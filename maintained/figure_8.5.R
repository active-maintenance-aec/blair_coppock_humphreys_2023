# blair_coppock_humphreys_2023 — figure_8.5.R
# Output: figure_8.5.pdf
# Output: figure_8.5.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_8.5.R

source(here::here("march_2026_rewrite", "helpers.R"))

set.seed(343)
gg_df <-
  fabricate(
    villages = add_level(N = 4, village_num = 1:4 + 1:4 * 0.1),
    households = add_level(N = 4, household_num = 1:4 + 1:4 * 0.1),
    individuals = add_level(
      N = 4,
      X = village_num + c(0.25, -0.25, 0.25, -0.25),
      Y = household_num + c(0.25, 0.25, -0.25, -0.25),
      a = block_ra(households, conditions = c("C", "T1", "T2")),
      b = block_ra(households, conditions = c("C", "T1", "T2", "T1\nT2")),
      c = if_else(b == "T1\nT2", "T3", as.character(b))
    )
  ) |>
  pivot_longer(cols = letters[1:3],
               names_to = "procedure",
               values_to = "assignment") |>
  mutate(procedure = factor(
    procedure,
    levels = letters[1:3],
    labels = c("Three-arm", "Factorial", "Four-arm")
  ))

g <- 
  ggplot(gg_df) +
  aes(X, Y) +
  geom_tile(aes(fill = assignment), width = 0.46, height = 0.46) +
  geom_text(aes(label = assignment), size = 2, color = "white") +
  coord_fixed() +
  facet_grid(~ procedure, switch = "y") +
  scale_fill_manual(values = dd_palette("quilt_palette")) +
  theme_dd() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

ggsave(here::here("march_2026_rewrite", "output", "figure_8.5.pdf"), g, width = 6.5, height = 2.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_8.5.svg"), g, width = 6.5, height = 2.5)
