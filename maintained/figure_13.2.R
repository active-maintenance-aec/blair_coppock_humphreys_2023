# blair_coppock_humphreys_2023 — figure_13.2.R
# Output: figure_13.2.pdf
# Output: figure_13.2.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_13.2.R

source(here::here("maintained", "helpers.R"))



diagnosis_13.1 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_13.1.rds"))

simulations_df <- get_simulations(diagnosis_13.1)

# first create summary for vertical lines
summary_df <- 
  simulations_df |> 
  group_by(estimator) |> 
  summarize(estimand = mean(estimand))

# then plot simulations
g <- 
  ggplot(simulations_df) +
  geom_histogram(aes(estimate),
                 bins = 40, fill = "#72B4F3") +
  geom_vline(data = summary_df,
             aes(xintercept = estimand),
             lty = "dashed", color = "#C6227F") +
  annotate("text", y = 300, x = 0.18, label = "Estimand",
           color = "#C6227F", hjust = 1) +
  facet_wrap(~ estimator) +
  labs(x = "Estimate", y = "Count of simulations") +
  theme_minimal()

ggsave(here::here("maintained", "output", "figure_13.2.pdf"), g, width = 6.5, height = 3.5)
ggsave(here::here("maintained", "output", "figure_13.2.svg"), g, width = 6.5, height = 3.5)

