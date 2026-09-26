# blair_coppock_humphreys_2023 — figure_19.6.R
# Output: figure_19.6.pdf
# Output: figure_19.6.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_19.6.R

source(here::here("maintained", "helpers.R"))



diagnosis_19.4 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_19.4.rds"))

diagnosands_df <- 
  diagnosis_19.4 |> 
  tidy() |> 
  filter(inquiry != "ATE_best") |> 
  mutate(hjust = if_else(inquiry == "ATE_implemented", "left", "right")) |>
  mutate(inquiry = factor(
    inquiry,
    c( "ATE_common", "ATE_implemented"),
    c("Common\ntreatment", "Implemented\ntreatment")
  ),
  design = factor(
    design,
    c("design_high_coordination", "design_low_coordination"),
    c("Strong coordination", "Weak coordination")
  )) |> 
  filter(diagnosand == "mean_estimand") 

simulations_df <-
  diagnosis_19.4 |> 
  get_simulations() |> 
  mutate(inquiry = factor(
    inquiry,
    c("ATE_common",  "ATE_implemented"),
    c(
      "Average ATE across sites for the common (coordinated) treatment",
      "Average ATE across sites for the treatment actually implemented"
    )
  ),
  design = factor(
    design,
    c("design_high_coordination", "design_low_coordination"),
    c("Strong coordination", "Weak coordination")
  ))

g <- 
  simulations_df |> 
  ggplot(aes(estimate)) + 
  geom_histogram(fill = dd_palette("dd_light_blue_alpha"), color = NA) + 
  geom_vline(data = diagnosands_df, aes(xintercept = estimate), color = dd_palette("dd_pink")) + 
  facet_grid(design ~ . ) + 
  geom_text(data = diagnosands_df, aes(x = estimate + 0.01 * ifelse(hjust == "left", 1, -1), y = 600, label = inquiry, hjust = hjust), color = dd_palette("dd_pink")) + 
  labs(y = "Number of estimates", x = "Sampling distribution of estimates") + 
  theme_dd()

ggsave(here::here("maintained", "output", "figure_19.6.pdf"),
       g,
       width = 6.5,
       height = 4)
ggsave(here::here("maintained", "output", "figure_19.6.svg"),
       g,
       width = 6.5,
       height = 4)

