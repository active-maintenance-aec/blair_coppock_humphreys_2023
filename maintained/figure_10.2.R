# blair_coppock_humphreys_2023 — figure_10.2.R
# Output: figure_10.2.svg
# Output: figure_10.2.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_10.2.R

source(here::here("march_2026_rewrite", "helpers.R"))



diagnosis_10a <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_10a.rds"))

simulations <-
  diagnosis_10a |>
  get_simulations() |> 
  mutate(
    var_hat = std.error ^ 2,
    facet_label = factor(
      heteroskedasticity,
      levels = c(-0.4, 0, 0.4),
      labels = c(
        "Control outcomes have higher variance\nthan treatment outcomes",
        "Potential outcomes have same variance\n(homoskedasticity)",
        "Treatment outcomes have higher variance\nthan control outcomes"
      )
    )
  ) 

downsampled_df <-
  simulations |>
  mutate(S = strata_rs(
    strata = paste0(heteroskedasticity, prob_treated),
    n = 50
  )) |>
  filter(S == 1)

gg_df <-
  diagnosis_10a |> 
  tidy() |> 
  mutate(
    facet_label = factor(
      heteroskedasticity,
      levels = c(-0.4, 0, 0.4),
      labels = c(
        "Control outcomes have higher variance\nthan treatment outcomes",
        "Potential outcomes have same variance\n(homoskedasticity)",
        "Treatment outcomes have higher variance\nthan control outcomes"
      )
    )
  ) 
  

labels_df <-
  tibble(
    estimator = c("Classical standard error",
                  "HC2 robust standard error"),
    facet_label = c("Potential outcomes have same variance\n(homoskedasticity)"),
    prob_treated = 0.5,
    y = 0.20,
    label = c(
      "Classical SEs can over- or under-\nestimate the diagnosand depending on\nvariances and the fraction treated",
      "Robust SEs are closer\nto the diagnosand on average\nregardless of those parameters"
    )
  )

labels2_df <-
  tibble(
    estimator = "Classical standard error",
    facet_label =
      "Control outcomes have higher variance\nthan treatment outcomes",
    prob_treated = 0.60,
    y = 0.15,
    label = c(
      "True value of\nthe diagnosand"
    )
  )

g <-
  ggplot(gg_df) +
  aes(prob_treated) +
  geom_line(aes(y = estimate, group = diagnosand, color = diagnosand)) +
  geom_point(
    data = downsampled_df,
    aes(y = var_hat),
    stroke = 0,
    alpha = 0.2,
    position = position_jitter(width = 0.01),
    color = dd_palette("dd_dark_blue") 
  ) +
  scale_color_manual(values = dd_palette("two_color_palette")) +
  geom_text(data = labels_df,
            aes(y = y, label = label),
            size = 2.5) +
  geom_text(data = labels2_df,
            aes(y = y, label = label),
            color = dd_palette("dd_pink"), 
            size = 2.5) +
  coord_cartesian(ylim = c(0, 0.25), xlim = c(0, 1)) + 
  facet_grid(estimator ~ facet_label) +
  theme_dd() +
  theme(text = element_text(size = 8)) +
  labs(x = "Proportion treated", y = "Standard error estimate (squared)")

ggsave(here::here("march_2026_rewrite", "output", "figure_10.2.svg"), g, height = 6.5, width = 6.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_10.2.pdf"), g, height = 6.5, width = 6.5)

