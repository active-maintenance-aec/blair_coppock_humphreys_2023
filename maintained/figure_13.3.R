# blair_coppock_humphreys_2023 — figure_13.3.R
# Output: figure_13.3.pdf
# Output: figure_13.3.svg
# Depends on: helpers.R
# Description: Maintained rewrite of figure_13.3.R

source(here::here("maintained", "helpers.R"))
library(geomtextpath)


set.seed(343)

design <-
  declare_model(
    N = 200,
    U = rnorm(N),
    potential_outcomes(Y ~ runif(1, 0.0, 0.5) * Z + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE") 

simulations_df <- 
  diagnose_design(design) |> 
  get_simulations() |> 
  mutate(significant = if_else(p.value <= 0.05, 1, 0))

g <- 
  ggplot(simulations_df) + 
  stat_smooth(aes(estimand, significant), method = 'loess', color = "#3564ED", fill = "#72B4F3", formula = 'y ~ x') +
  geom_hline(yintercept = 0.8, color = "#C6227F", linetype = "dashed") +
  annotate("text", x = 0, y = 0.85, label = "Conventional power threshold = 0.8", hjust = 0, color = "#C6227F") + 
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "none") +
  labs(x = "Model parameter: true effect size",
       y = "Diagnosand: statistical power") +
  theme_minimal()

ggsave(here::here("maintained", "output", "figure_13.3.pdf"), g, width = 6.5, height = 3.5)
ggsave(here::here("maintained", "output", "figure_13.3.svg"), g, width = 6.5, height = 3.5)
