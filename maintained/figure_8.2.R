# blair_coppock_humphreys_2023 — figure_8.2.R
# Output: figure_8.2.pdf
# Output: figure_8.2.svg
# Depends on: helpers.R
# Description: Maintained rewrite of figure_8.2.R

source(here::here("maintained", "helpers.R"))


set.seed(343)
gg_df <-
  fabricate(
    villages = add_level(N = 4, village_num = 1:4 + 1:4 * 0.1),
    households = add_level(N = 4,
                           household_num = 1:4 + 1:4 * 0.1),
    individuals = add_level(
      N = 4,
      X = village_num + c(0.25, -0.25, 0.25, -0.25),
      Y = household_num + c(0.25, 0.25, -0.25, -0.25),
      a = simple_rs(N),
      b = complete_rs(N),
      c = strata_rs(strata = villages),
      d = cluster_rs(clusters = households, simple = TRUE),
      e = cluster_rs(clusters = households),
      f = strata_and_cluster_rs(clusters = households, strata = villages),
      g = d * simple_rs(N),
      h = e * strata_rs(strata = households),
      i = f * strata_rs(strata = paste0(households, Y))
    )
  ) |>
  pivot_longer(cols = letters[1:9],
               names_to = "procedure",
               values_to = "sampled") |>
  mutate(
    procedure = factor(
      procedure,
      levels = letters[1:9],
      labels = c(
        "Individual Random Sampling (simple)",
        "Individual Random Sampling (complete)",
        "Individual Random Sampling (stratified)",
        "Cluster Random Sampling (simple)",
        "Cluster Random Sampling (complete)",
        "Cluster Random Sampling (stratified)",
        "Multistage Random Sampling (simple)",
        "Multistage Random Sampling (complete)",
        "Multistage Random Sampling (stratified)"
      )
    ),
    sampled = as.factor(sampled),
    unit = case_when(
      str_detect(procedure, "Individual") ~ "Individual",
      str_detect(procedure, "Cluster") ~ "Cluster",
      str_detect(procedure, "Multistage") ~ "Multistage"
    ),
    sampling_type = case_when(
      str_detect(procedure, "simple") ~ "Simple",
      str_detect(procedure, "complete") ~ "Complete",
      str_detect(procedure, "stratified") ~ "Stratified"
    ),
    unit = factor(unit, levels = c("Individual", "Cluster", "Multistage")),
    sampling_type = factor(sampling_type, levels = c("Simple", "Complete", "Stratified"))
  )

cluster_df <-
  gg_df |>
  group_by(households, unit, sampling_type) |>
  summarize(hh_sampled = any(sampled == 1),
            X = mean(X),
            Y = mean(Y)) |>
  filter(hh_sampled == TRUE, unit != "Individual")

g <- 
  ggplot(gg_df, aes(X, Y)) +
  geom_tile(aes(fill = sampled), color = NA, width = 0.46, height = 0.46) +
  geom_tile(data = cluster_df, fill = NA, color = gray(0.6), size = 0.25, width = 1.03, height = 1.03) +
  coord_fixed() +
  facet_grid(unit  ~ sampling_type, switch = "y") +
  scale_fill_manual(values = c(gray(0.95), dd_palette("dd_light_blue"))) +
  scale_x_continuous(name = "Stratum (e.g., locality)", breaks = 1:4 + 1:4 * 0.1, labels = LETTERS[1:4]) +
  theme_dd() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

ggsave(here::here("maintained", "output", "figure_8.2.pdf"), g, width = 6.5, height = 7)
ggsave(here::here("maintained", "output", "figure_8.2.svg"), g, width = 6.5, height = 7)


# gg_df |> group_by(unit, sampling_type) |> summarize(n = n(), sum(sampled == 1))

