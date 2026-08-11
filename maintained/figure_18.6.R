# blair_coppock_humphreys_2023 — figure_18.6.R
# Output: figure_18.6.pdf
# Output: figure_18.6.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_18.6.R

source(here::here("march_2026_rewrite", "helpers.R"))



diagnosis_18.6 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_18.6.rds"))

simulations <- 
  diagnosis_18.6 |> 
  get_simulations()

downsample <- nrow(simulations) * 0.2

gg_df <-
  simulations |> 
  sample_n(size = downsample)

g1 <-
  ggplot(gg_df) +
  aes(n_x1, estimate, group = n_x1) +
  geom_point(alpha = 0.15,
             stroke = 0,
             position = position_jitter(width = 10)) +
  theme_dd() +
  labs(x = "Out of 1000 total subjects, the number of subjects with X = 1",
       y = "Difference-in-CATEs estimate")

gg_df2 <- 
  diagnosis_18.6 |> 
  tidy() |> 
  filter(inquiry == "diff_in_CATEs") |> 
  mutate(
    diagnosand = factor(diagnosand, 
                        levels = c("bias", "sd_estimate", "power"),
                        labels = c("Bias", "Standard Error", "Power")
    )
  ) |> 
  filter(!is.na(diagnosand))

labels_df <- 
  tibble(
    n_x1 = c(50, 500, 850),
    estimate = c(0.05, 0.1, 0.8),
    diagnosand = c("Bias", "Standard Error", "Power")
  )

g2 <- 
  ggplot(gg_df2, aes(n_x1, estimate, group = diagnosand, color = diagnosand)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, color = NULL, fill = diagnosand), alpha = 0.2) +
  geom_line() +
  geom_text(data = labels_df, aes(label = diagnosand), nudge_y = 0.05) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_manual(values = dd_palette("three_color_palette")) + 
  scale_fill_manual(values = dd_palette("three_color_palette")) + 
  labs(x = "Out of 1000 total subjects, the number of subjects with X = 1",
       y = "Diagnosand estimate") + 
  theme_dd() + 
  theme(panel.grid.minor.y = element_blank(),
        legend.position = "none") 

g <- g1 / g2

g

ggsave(here::here("march_2026_rewrite", "output", "figure_18.6.pdf"), g, width = 6.5, height = 6.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_18.6.svg"), g, width = 6.5, height = 6.5)

