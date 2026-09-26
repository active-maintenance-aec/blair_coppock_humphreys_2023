# blair_coppock_humphreys_2023 — figure_9.2.R
# Output: figure_9.2.svg
# Output: figure_9.2.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_9.2.R

source(here::here("maintained", "helpers.R"))



diagnosis_9.2 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_9.2.rds"))

gg_df <- 
  diagnosis_9.2 |> 
  get_simulations() |> 
  select(-term) |> 
  pivot_wider(id_cols = c(sim_ID, inquiry, estimand),
              names_from = estimator,
              values_from = c(estimate, p.value)) |> 
  mutate(test_sig = as.numeric(p.value_test <= 0.05)) 

g <- 
  ggplot(data = gg_df) +
  aes(estimate_estimator, test_sig) +
  stat_smooth(method = 'loess', color = dd_palette("dd_dark_blue"), fill = dd_palette("dd_light_blue_alpha"), formula = 'y ~ x') +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = seq(0, 80, 20), limits = c(0, 80)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Sample mean age estimate",
       y = "Frequency of rejecting the null hypothesis\nthat the average age in the population is 20") + 
  theme_dd()

ggsave(here::here("maintained", "output", "figure_9.2.svg"), g, height = 4, width = 6.5)
ggsave(here::here("maintained", "output", "figure_9.2.pdf"), g, height = 4, width = 6.5)

  


