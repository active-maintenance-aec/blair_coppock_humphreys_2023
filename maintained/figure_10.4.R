# blair_coppock_humphreys_2023 — figure_10.4.R
# Output: figure_10.4.svg
# Output: figure_10.4.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_10.4.R

source(here::here("march_2026_rewrite", "helpers.R"))
library(ggridges)

library(ggridges)


simulation_10.1 <- read_rds(here::here("original", "diagnosis_objects", "simulation_10.1.rds"))

estimation_df <-
  simulation_10.1 |>
  group_by(sim_ID) |>
  nest() |>
  mutate(densities = lapply(data, function(dat) {
    with(dat, tibble(
      x = seq(-2, 2, 0.001),
      density = dnorm(x, mean = estimate, sd = std.error)
    ))
  })) |>
  unnest(cols = c(data, densities)) |>
  group_by(sim_ID)

hypothesis_df <- 
  simulation_10.1 |>
  group_by(sim_ID) |>
  nest() |>
  mutate(densities = lapply(data, function(dat) {
    with(dat, tibble(
      x = seq(-3.4, 3.4, 0.001),
      density = dt(x = x, df = df)
    ))
  })) |>
  unnest(cols = c(data, densities)) |>
  group_by(sim_ID) |>
  mutate(
    low_cut = case_when(
      statistic < 0 ~ x < statistic,
      statistic > 0 ~ x <  -1 *statistic),
    high_cut = case_when(statistic < 0 ~ x >= -1 *statistic,
                         statistic > 0 ~ x >=  statistic),
    area_under = case_when(low_cut ~ "low",
                           high_cut ~ "high",
                           TRUE ~ "middle")
  ) 

g_estimation <- 
  ggplot(simulation_10.1, aes(x = estimate, y = as.factor(sim_ID)), color = dd_palette("dd_light_blue")) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.075,
                 size = 0.25,
                 color = gray(0.5)) +
  geom_point(size = 1.5, color = gray(0.5)) +
  geom_point(
    aes(x = estimand),
    size = 1.5,
    fill = NA,
    color = gray(0.5),
    pch = 24,
    position = position_nudge(y = -0.15)
  ) +
  geom_ridgeline(
    data = estimation_df,
    size = 0.1,
    aes(x = x, height = density, fill = NA),
    scale = 0.25,
    min_height = 0.01,
    alpha = 0,
    color = dd_palette("dd_light_blue"),
  ) +
  theme_void() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 7)
  ) +
  coord_cartesian(xlim = c(-0.75, 1.25)) +
  geom_vline(
    xintercept = 0,
    color = gray(0.5),
    linetype = "dashed",
    alpha = 0.5
  ) +
  geom_vline(
    xintercept = mean(simulation_10.1$estimate),
    color = dd_palette("dd_light_blue"),
    linetype = "dotted",
    alpha = 0.5
  ) +
  scale_fill_manual(name = "Probability",
                    values = c(dd_palette("dd_light_blue_alpha"), dd_palette("dd_light_blue_alpha"), hex_add_alpha(dd_palette("dd_light_gray"), alpha = 0.5))) +
  ggtitle("Estimates, sampling distributions, and estimands")



g_hypothesis <-
  ggplot(simulation_10.1, aes(x = statistic, y = as.factor(sim_ID)), color = dd_palette("dd_light_blue")) +
  # first plot the density for p-values
  geom_ridgeline(
    data = hypothesis_df,
    size = 0,
    scale = 1.25,
    aes(x = x, height = density, fill = area_under),
    min_height = 0.01
  ) +
  # then the density lines (to make it plot correctly and not be cut off)
  geom_ridgeline(
    data = hypothesis_df,
    size = 0.1,
    scale = 1.25,
    aes(x = x, height = density),
    fill = NA,
    min_height = 0.01,
    # alpha = 0,
    color = dd_palette("dd_light_blue"),
  ) +
  geom_point(size = 1.5, color = gray(0.5)) +
  theme_void() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 7)
  ) +
  geom_vline(
    xintercept = 0,
    color = gray(0.5),
    linetype = "dashed",
    alpha = 0.5
  ) +
  scale_fill_manual(name = "Probability",
                    values = c(dd_palette("dd_light_blue_alpha"), dd_palette("dd_light_blue_alpha"), hex_add_alpha(dd_palette("dd_light_gray"), alpha = 0.5))) +
  ggtitle("Null hypothesis tests")

g <- g_estimation + g_hypothesis

g

ggsave(here::here("march_2026_rewrite", "output", "figure_10.4.svg"), g, width = 5, height = 3.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_10.4.pdf"), g, width = 5, height = 3.5)

  


