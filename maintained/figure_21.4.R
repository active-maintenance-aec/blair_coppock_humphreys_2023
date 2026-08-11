# blair_coppock_humphreys_2023 — figure_21.4.R
# Output: figure_21.4.pdf
# Output: figure_21.4.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_21.4.R

source(here::here("march_2026_rewrite", "helpers.R"))



prior <- 0.3
prior_sd <- 0.1
calculate_n_required <- function(delta, sd = 1, power = 0.8) {
  sapply(
    delta,
    FUN = function(x)
      power.t.test(
        delta = x,
        sd = sd,
        power = power
      )$n * 2
  )
}

diagnosis_21b <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_21b.rds"))

pilot_sims <-
  diagnosis_21b |>
  get_simulations() |> 
  mutate(
    prior_w = 1/(prior_sd^2),
    pilot_w = 1/(std.error^2),
    sum_w = prior_w + pilot_w,
    posterior = prior * prior_w/sum_w + estimate * pilot_w/sum_w,
    posterior_sd = sqrt(1/sum_w),
    conservative_guess = qnorm(0.1, posterior, posterior_sd),
    N_required = calculate_n_required(conservative_guess)
  )

pilot_sims <-
  pilot_sims |> 
  filter(N_required <= 7500, !is.na(N_required))

choices_df <-
  tibble(
    xintercept = c(calculate_n_required(qnorm(0.1, prior, prior_sd)),
                   calculate_n_required(0.1),
                   mean(pilot_sims$N_required)),
    label = c("Choice of N without pilot",
              "Best choice of N",
              "Average choice of N with pilot"),
    x = xintercept + 100,
    y = c(.11, .055, .09)
  )


g <-
  ggplot(pilot_sims) +
  aes(N_required) +
  geom_histogram(
    aes(y = after_stat(count) / sum(after_stat(count))),
    fill = dd_palette("dd_light_blue_alpha"),
    color = "transparent",
    binwidth = 160) +
  geom_segment(
    data = choices_df, 
    aes(x = xintercept, xend = xintercept, y = 0, yend = y - .005), 
    color = dd_palette("dd_gray")) +
  geom_text(
    data = choices_df, 
    aes(x = x, y = y, label = label), 
    color = dd_palette("dd_gray"), 
    hjust = 0.2) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.1, 0.02)) +
  theme_dd() +
  labs(x = "Distribution of choice of N, depending on results of pilot",
       y = "Percent of simulations")
g

ggsave(here::here("march_2026_rewrite", "output", "figure_21.4.pdf"),
       g,
       width = 6.5,
       height = 4)
ggsave(here::here("march_2026_rewrite", "output", "figure_21.4.svg"),
       g,
       width = 6.5,
       height = 4)
