# blair_coppock_humphreys_2023 — figure_16.6.R
# Output: figure_16.6.svg
# Output: figure_16.6.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_16.6.R

source(here::here("march_2026_rewrite", "helpers.R"))

library(DIDmultiplegt)


N_units <- 20
N_time_periods <- 20

M_units <- declare_model(
  units = add_level(
    N = N_units, 
    U_unit = rnorm(N), 
    D_unit = if_else(U_unit > median(U_unit), 1, 0),
    D_time = sample(1:N_time_periods, N, replace = TRUE)
  ),
  periods = add_level(
    N = N_time_periods,
    U_time = rnorm(N),
    nest = FALSE
  ),
  unit_period = cross_levels(
    by = join_using(units, periods), 
    U = rnorm(N)
  )
) 

M_PO_homogenous <-
  declare_model(potential_outcomes(Y ~ U + U_unit + U_time + D * 0.2, conditions = list(D = c(0, 1))))

M_PO_later_lower <-
  declare_model(potential_outcomes(Y ~ U + U_unit + U_time + D * (0.2 + 1 * (
    D_time - as.numeric(periods)
  )), conditions = list(D = c(0, 1))))

M_PO_later_higher <-
  declare_model(potential_outcomes(Y ~ U + U_unit + U_time + D * (0.2 - 1 * (
    D_time - as.numeric(periods)
  )), conditions = list(D = c(0, 1))))

M_D_staggered <- 
  declare_model(
    D = if_else(D_unit == 1 & as.numeric(periods) >= D_time, 1, 0),
    D_lag = lag_by_group(D, groups = units, n = 1, order_by = periods)
  )

I <-   
  declare_inquiry(
    ATT = mean(Y_D_1 - Y_D_0), 
    subset = D == 1
  ) + 
  declare_inquiry(
    ATT_switchers = mean(Y_D_1 - Y_D_0), 
    subset = D == 1 & D_lag == 0 & !is.na(D_lag)
  ) 

D <- declare_measurement(Y = reveal_outcomes(Y ~ D))

A <- 
  declare_estimator(
    Y ~ D, fixed_effects = ~ units + periods,
    .method = lm_robust,
    inquiry = c("ATT", "ATT_switchers"),
    label = "twoway-fe"
  ) +
  
  declare_estimator(
    Y = "Y", 
    G = "units", 
    T = "periods", 
    D = "D",
    handler = label_estimator(did_multiplegt_tidy),
    inquiry = c("ATT", "ATT_switchers"),
    label = "chaisemartin"
  ) 

D_homogenous <- M_units + M_PO_homogenous + M_D_staggered + I + D + A
D_later_lower <- M_units + M_PO_later_lower + M_D_staggered + I + D + A
D_later_higher <- M_units + M_PO_later_higher + M_D_staggered + I + D + A

diagnosis_16.4 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_16.4.rds"))


po_summary_df <-
  map(list(
    PO_homogenous = D_homogenous,
    PO_later_lower = D_later_lower,
    PO_later_higher = D_later_higher
  ),
  draw_data) |>
  bind_rows(.id = "PO_type") |>
  group_by(PO_type, time = as.numeric(periods)) |>
  summarize(ATT = mean(Y_D_1[D == 1] - Y_D_0[D == 1]), .groups = "drop") |>
  mutate(
    PO_type = case_when(
      PO_type == "PO_homogenous" ~ "Effects: Homogenous",
      PO_type == "PO_later_higher" ~ "Effects: Higher Later",
      PO_type == "PO_later_lower" ~ "Effects: Lower Later"
    )
  )

gg_pos <-
  ggplot(data = po_summary_df, aes(time, ATT)) +
  geom_line() +
  facet_grid( ~ PO_type) +
  theme_dd() +
  labs(x = "Time period", y = "True ATT")

sampling_distribution_df <- 
  get_simulations(diagnosis_16.4) |> 
  mutate(design = case_when(
    design == "PO_homogenous" ~ "Effects: Homogenous",
    design == "PO_later_higher" ~ "Effects: Higher Later",
    design == "PO_later_lower" ~ "Effects: Lower Later"
  ),
  estimator = if_else(estimator == "chaisemartin", "Estimator:\nChaisemartin-d'Haultfoeuille", "Estimator:\nTwo-way Fixed Effects")) 

summaries_df <-
  diagnosis_16.4 |>
  get_simulations() |>
  group_by(estimator, design, inquiry) |>
  summarize(estimand = mean(estimand),
            .groups = "drop") |>
  mutate(
    x = estimand + if_else(inquiry == "ATT", -1, -1),
    y = 0.1,
    label = if_else(inquiry == "ATT", "ATT", "ATT for\nSwitchers"),
    label = replace(
      label,
      design != "PO_later_higher" | estimator != "twoway-fe",
      ""
    ),
    design = case_when(
      design == "PO_homogenous" ~ "Effects: Homogenous",
      design == "PO_later_higher" ~ "Effects: Higher Later",
      design == "PO_later_lower" ~ "Effects: Lower Later"
    ),
    estimator = if_else(
      estimator == "chaisemartin",
      "Estimator:\nChaisemartin-d'Haultfoeuille",
      "Estimator:\nTwo-way Fixed Effects"
    ),
    hjust = rep(c(1, 1), times = 6)
  )

gg_sampling_distribution <-
  ggplot(sampling_distribution_df) +
  aes(estimate) +
  geom_histogram(
    aes(y = after_stat(count) / sum(after_stat(count))),
    fill = dd_palette("dd_light_blue_alpha"),
    color = "transparent",
    binwidth = 0.7
  ) +
  geom_vline(data = summaries_df,
             aes(
               xintercept = estimand,
               linetype = inquiry,
               color = inquiry
             )) +
  geom_text(
    data = summaries_df,
    aes(x, y, label = label, color = inquiry, hjust = hjust),
    vjust = 1
  ) +
  facet_grid(factor(estimator, levels = c("Estimator:\nTwo-way Fixed Effects",
                                          "Estimator:\nChaisemartin-d'Haultfoeuille")) ~ design) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.1, 0.02)) +
  scale_color_manual(values = dd_palette("two_color_palette")) +
  theme_dd() +
  labs(x = "Simulated effect estimate",
       y = "Percent of simulations") 

g <- wrap_plots(gg_pos, gg_sampling_distribution, heights = c(0.33, 0.66), nrow = 2, ncol = 1) 

ggsave(here::here("march_2026_rewrite", "output", "figure_16.6.svg"),
       g,
       width = 6.5,
       height = 7.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_16.6.pdf"),
       g,
       width = 6.5,
       height = 7.5)
