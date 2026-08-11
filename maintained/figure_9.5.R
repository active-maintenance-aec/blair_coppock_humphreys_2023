# blair_coppock_humphreys_2023 — figure_9.5.R
# Output: figure_9.5.svg
# Output: figure_9.5.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_9.5.R

source(here::here("march_2026_rewrite", "helpers.R"))



diagnosis_9.6 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_9.6.rds"))

gg_df <- 
  diagnosis_9.6 |>
  get_diagnosands() |>
  mutate(
    `Data Strategy` = rep(c("D1: Equal probabilities", "D2: Differential probabilities"), each = 2),
    `Answer Strategy` = rep(c("A1: Plug-in estimator", "A2: IPW estimator\nby D2 probabilities"), 2),
    label = if_else(design == "design_2",
                    paste0("Bias (simulation error)\n", make_se_entry(bias, `se(bias)`)),
                    make_se_entry(bias, `se(bias)`))
  ) 

g <- 
  ggplot(gg_df) +
  aes(`Data Strategy`, `Answer Strategy`) + 
  geom_tile(aes(fill = bias), alpha = 0.9) +
  geom_text(aes(label = label)) +
  scale_fill_gradient2(limits = c(-0.2,0.2), low = dd_palette("dd_pink"), high = dd_palette("dd_dark_blue"), mid = "white") +
  theme_dd() +
  theme(panel.grid.major = element_blank()) + 
  labs(x = "Data Strategy",
       y = "Answer Strategy")

ggsave(here::here("march_2026_rewrite", "output", "figure_9.5.svg"), g, width = 6.5, height = 3.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_9.5.pdf"), g, width = 6.5, height = 3.5)
