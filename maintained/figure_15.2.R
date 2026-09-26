# blair_coppock_humphreys_2023 — figure_15.2.R
# Output: figure_15.2.svg
# Output: figure_15.2.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_15.2.R

source(here::here("maintained", "helpers.R"))



diagnosis_15.2 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_15.2.rds"))

gg_df <- 
  diagnosis_15.2 |> 
  tidy() 

bias_df <- 
  gg_df |> 
  filter(inquiry == "Y_bar") 

rr_df <- 
  gg_df |>
  filter(estimator == "Response Rate")

g1 <-
  ggplot(rr_df |> filter(diagnosand == "mean_estimate"), aes(effort, estimate)) +
  geom_line(col = dd_palette("dd_dark_blue")) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = dd_palette("dd_light_blue_alpha")) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(y = "Diagnosand:\nresponse rate", x = "Effort (e.g., recontacts or financial incentives)") +
  theme_dd()

g2 <-
  ggplot(bias_df |> filter(diagnosand == "bias"), aes(effort, estimate)) +
  geom_line(col = dd_palette("dd_dark_blue")) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = dd_palette("dd_light_blue_alpha")) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(y = "Diagnosand:\nbias", x = "Effort (e.g., recontacts or financial incentives)") +
  theme_dd()

g <- g1 / g2

ggsave(here::here("maintained", "output", "figure_15.2.svg"), g, width = 6.5, height = 3.5)
ggsave(here::here("maintained", "output", "figure_15.2.pdf"), g, width = 6.5, height = 3.5)
