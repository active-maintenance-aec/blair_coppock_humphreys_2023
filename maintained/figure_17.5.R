# blair_coppock_humphreys_2023 — figure_17.5.R
# Output: figure_17.5.pdf
# Output: figure_17.5.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_17.5.R

source(here::here("march_2026_rewrite", "helpers.R"))



diagnosis_17.5 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_17.5.rds"))

gg_df <-
  diagnosis_17.5 |>
  tidy() |>
  filter(diagnosand == "bias") |>
  mutate(deceive = factor(
    deceive,
    c(TRUE, FALSE),
    c("With deception", "Without deception")
  ))

g <-
  ggplot(gg_df) + 
  aes(deceive, estimate) +
  geom_point() + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  labs(y = "Diagnosand: Bias") +
  theme_dd() +
  theme(axis.title.x = element_blank()) +
  facet_grid(.~inquiry) 

g
ggsave(here::here("march_2026_rewrite", "output", "figure_17.5.pdf"), g, width = 6.5, height = 3.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_17.5.svg"), g, width = 6.5, height = 3.5)



