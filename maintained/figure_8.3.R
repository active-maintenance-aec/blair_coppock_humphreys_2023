# blair_coppock_humphreys_2023 — figure_8.3.R
# Output: figure_8.3.pdf
# Output: figure_8.3.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_8.3.R

source(here::here("march_2026_rewrite", "helpers.R"))


convenience <-
  c(1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 0, 0, 0,
    1, 1, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0)

quota <-
  c(1, 1, 1, 1, 1, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 1, 1, 1, 1, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 0, 1, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 1, 0, 0, 0,
    0, 0, 0, 0, 1, 1, 1, 0)

rds <-
  c(0, 0, 0, 1, 1, 1, 0, 1, 
    0, 0, 0, 0, 0, 1, 0, 0, 
    0, 0, 0, 0, 1, 0, 0, 0, 
    0, 1, 1, 0, 1, 1, 1, 0, 
    0, 0, 0, 0, 0, 0, 1, 0,
    1, 1, 1, 0, 0, 1, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 1, 0, 1, 1, 1, 0)


gg_df <-
  fabricate(
    villages = add_level(N = 4, village_num = 1:4 + 1:4 * 0.1),
    households = add_level(N = 4,
                           household_num = 1:4 + 1:4 * 0.1),
    individuals = add_level(
      N = 4,
      X = village_num + c(-0.25, 0.25,-0.25, 0.25),
      Y = household_num + c(-0.25,-0.25, 0.25, 0.25),
      a = convenience,
      b = quota,
      c = rds
    )
  ) |>
  pivot_longer(cols = letters[1:3],
               names_to = "procedure",
               values_to = "sampled") |>
  mutate(
    procedure = factor(
      procedure,
      levels = letters[1:3],
      labels = c("Convenience",
                 "Quota",
                 "Respondent-driven")
    ),
    sampled = as.factor(sampled)
  )

g <- 
ggplot(gg_df, aes(X, Y)) +
  geom_tile(aes(fill = sampled), color = NA, width = 0.46, height = 0.46) +
  coord_fixed() +
  facet_wrap(~procedure) +
  scale_fill_manual(values = c(gray(0.95), dd_palette("dd_light_blue"))) +
  scale_x_continuous(name = "Stratum (e.g., locality)", breaks = 1:4 + 1:4 * 0.1, labels = LETTERS[1:4]) +
  theme_dd() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

ggsave(here::here("march_2026_rewrite", "output", "figure_8.3.pdf"), g, width = 6.5, height = 2.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_8.3.svg"), g, width = 6.5, height = 2.5)
