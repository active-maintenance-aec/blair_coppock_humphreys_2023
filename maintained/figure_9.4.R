# blair_coppock_humphreys_2023 — figure_9.4.R
# Output: figure_9.4.svg
# Output: figure_9.4.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_9.4.R

source(here::here("maintained", "helpers.R"))



diagnosis_9.4 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_9.4.rds"))


simulations <- 
  diagnosis_9.4 |> 
  get_simulations() |> 
  filter(sim_ID < 500) |> 
  mutate(fail_to_reject = as.numeric(p.value > 0.05),
         true_mean = as.numeric(true_mean))

g <- 
ggplot(simulations) +
  aes(true_mean, fail_to_reject) + 
  stat_smooth(method = 'loess', color = dd_palette("dd_dark_blue"), fill = dd_palette("dd_light_blue_alpha"), formula = 'y ~ x') +
  scale_y_continuous(labels = percent) +
  labs(x = "True population mean under many non-null models",
       y = "Type II error rate\n(Rate of failure to reject the false null model)") + 
  theme_dd()

ggsave(here::here("maintained", "output", "figure_9.4.svg"), g, width = 7, height = 5)
ggsave(here::here("maintained", "output", "figure_9.4.pdf"), g, width = 7, height = 5)

  


