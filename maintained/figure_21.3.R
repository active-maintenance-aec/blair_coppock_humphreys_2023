# blair_coppock_humphreys_2023 — figure_21.3.R
# Output: figure_21.3.pdf
# Output: figure_21.3.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_21.3.R

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

full_dist <-
  tibble(
    effect_size = c(seq(-0.25,-0.01, by = 0.01),
                    seq(0.01, 0.75, by = 0.01)),
    N_required = sapply(
      effect_size,
      FUN = function(x)
        power.t.test(
          delta = abs(x),
          sd = 1,
          power = 0.8
        )$n * 2
    ),
    density = dnorm(effect_size, 0.3, 0.1)
  )

label_df <-
  full_dist |>
  filter(round(effect_size, 2) %in% c(0.10, 0.17, 0.3)) |>
  mutate(label = c("3200 subjects required at 0.10",
                   "1100 subjects required at 0.17",
                   "350 subjects required at 0.30"),
         label2 = c("True effect size",
                    "10th percentile guess",
                    "Average (best) guess"))

g1 <- 
  ggplot(label_df, aes(effect_size, N_required)) +
  geom_line(data = full_dist) +
  geom_point(aes(color = label2, shape = label2)) +
  geom_segment(aes(xend = effect_size, yend = 0, color = label2)) +
  geom_text(aes(label = label, color = label2), hjust = 0, nudge_x = 0.02) +
  scale_color_manual(values = dd_palette("three_color_palette")) + 
  coord_cartesian(xlim = c(-0.25, 0.75),
                  ylim = c(0, 5000)) +
  theme_dd() + 
  theme(legend.position = "none") +
  labs(x = "Effect size in standard units",
       y = "Sample size needed for 80% power",
       title = "Minimum sample sizes for 80% power, by effect size")

g2 <-
  ggplot(label_df, aes(effect_size, density)) +
  geom_point(aes(color = label2, shape = label2)) +
  geom_segment(aes(xend = effect_size, yend = 0, color = label2)) +
  geom_text(aes(label = label2, color = label2), hjust = 1, nudge_x = -0.02) +
  geom_line(data = full_dist) +
  scale_color_manual(values = dd_palette("three_color_palette")) + 
  coord_cartesian(xlim = c(-0.25, 0.75)) +
  theme_dd() + 
  theme(legend.position = "none") +
  labs(x = "Effect size in standard units",
       y = "Density of prior belief distribution",
       title = "Researcher's prior beliefs about effect size")

g <- g1 / g2

ggsave(here::here("march_2026_rewrite", "output", "figure_21.3.pdf"),
       g,
       width = 6.5,
       height = 6.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_21.3.svg"),
       g,
       width = 6.5,
       height = 6.5)
