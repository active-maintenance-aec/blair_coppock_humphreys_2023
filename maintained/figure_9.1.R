# blair_coppock_humphreys_2023 — figure_9.1.R
# Output: figure_9.1.svg
# Output: figure_9.1.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_9.1.R

source(here::here("maintained", "helpers.R"))



diagnosis_9.1 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_9.1.rds"))

simulations <- 
  diagnosis_9.1 |> 
  get_simulations()

diagnosands <-
  diagnosis_9.1 |> 
  tidy()

vlines_df <-
  diagnosands |> 
  filter(diagnosand %in% c("mean_estimate", "mean_estimand")) |>
  mutate(x_pos = estimate + c(2, -2),
         hjust = c(0, 1),
         label = c("Mean estimate", "Estimand"))

g <-
  ggplot(simulations) +
  aes(estimate) +
  geom_histogram(
    aes(y = after_stat(count) / sum(after_stat(count))), 
    fill = dd_palette("dd_light_blue_alpha"),
    color = "transparent",
    binwidth = 4) +
  geom_vline(data = vlines_df,
             size = 1,
             alpha = 0.5,
             aes(xintercept = estimate,
                 linetype = diagnosand),
             color = dd_palette("dd_gray")) +
  geom_text(
    data = vlines_df,
    aes(x = x_pos, label = label, hjust = hjust),
    y = 0.11,
    color = dd_palette("dd_gray")
  ) + 
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.1, 0.02)) +
  scale_x_continuous(breaks = seq(0, 80, 20), limits = c(0, 80)) +
  labs(x = "Simulated effect estimate",
       y = "Percent of simulations") + 
  theme_dd()

g

ggsave(here::here("maintained", "output", "figure_9.1.svg"), g, height = 3.5, width = 5.5)
ggsave(here::here("maintained", "output", "figure_9.1.pdf"), g, height = 3.5, width = 5.5)

  


