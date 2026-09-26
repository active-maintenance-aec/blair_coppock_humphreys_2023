# blair_coppock_humphreys_2023 — figure_15.3.R
# Output: figure_15.3.svg
# Output: figure_15.3.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_15.3.R

source(here::here("maintained", "helpers.R"))



diagnosis_15.3 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_15.3.rds"))

gg_df <- 
  diagnosis_15.3 |> 
  tidy() |> 
  filter(diagnosand == "sd_estimate")

g <- 
  ggplot(gg_df, aes(cluster_prob, estimate)) +
  geom_line(col = dd_palette("dd_dark_blue")) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = dd_palette("dd_light_blue_alpha")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, NA)) +
  labs(x = "Probability of sampling a cluster",
       y = "Standard deviation of sampling distribution") + 
  theme_dd()

ggsave(here::here("maintained", "output", "figure_15.3.svg"), g, width = 6.5, height = 3.5)
ggsave(here::here("maintained", "output", "figure_15.3.pdf"), g, width = 6.5, height = 3.5)
