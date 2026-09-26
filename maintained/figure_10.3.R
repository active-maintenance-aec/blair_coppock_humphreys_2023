# blair_coppock_humphreys_2023 — figure_10.3.R
# Output: figure_10.3.svg
# Output: figure_10.3.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_10.3.R

source(here::here("maintained", "helpers.R"))




simulation_10.1 <- read_rds(here::here("original", "diagnosis_objects", "simulation_10.1.rds"))

estimation_df <-
  simulation_10.1 |>
  group_by(sim_ID) |>
  nest() |>
  mutate(densities = lapply(data, function(dat) {
    with(dat, tibble(
      x = seq(-2, 2, 0.0001),
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
      x = seq(-3.4, 3.4, 0.0001),
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

simulation_1 <- simulation_10.1 |> filter(sim_ID == 1) 
estimation_df_1 <- estimation_df |> filter(sim_ID == 1)
hypothesis_df_1 <- hypothesis_df |> filter(sim_ID == 1)

g_hypothesis_1 <-
  ggplot(simulation_1, aes(y = 0),
         color = dd_palette("dd_light_blue")) +
  geom_ribbon(data = hypothesis_df_1,
              size = 0,
              aes(
                x = x,
                ymin = 0,
                ymax = density,
                fill = area_under
              )) +
  # then the density lines (to make it plot correctly and not be cut off)
  geom_line(data = hypothesis_df_1,
            size = 0.1,
            aes(x = x, y = density),
            color = dd_palette("dd_light_blue")) +
  geom_point(aes(x = statistic), size = 1.5, color = gray(0.5)) +
  geom_vline(
    xintercept = 0,
    color = gray(0.5),
    linetype = "dashed",
    alpha = 0.5
  )  +
  coord_cartesian(ylim = c(-0.03, NA)) +
  theme_void() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_manual(name = "Probability",
                    values = c(dd_palette("dd_light_blue_alpha"), dd_palette("dd_light_blue_alpha"), hex_add_alpha(dd_palette("dd_light_gray"), alpha = 0.5))) +
  geom_richtext(
    aes(x = 3.5, y = 0.35, label = paste0("Hypothesized<br>null distribution:<br>t(df = ", format_num(df, 0), ")")),
    size = 2.5,
    fill = NA,
    hjust = 1,
    label.colour = NA
  ) + 
  geom_text(
    aes(x = 0, y = -0.031, label = paste0("t-statistic: ",format_num(statistic, digits = 2))),
    size = 2, hjust = 0, nudge_x = 0.5
  ) +
  geom_text(
    aes(x = 0, y = -0.031, label = paste0("p-value: 2*",format_num(p.value/2, digits = 2), " = ", format_num(p.value, digits = 2))),
    size = 2, hjust = 1, nudge_x = -0.5
  )

g_estimation_1 <-
  ggplot(simulation_1, aes(y = 0), color = dd_palette("dd_light_blue")) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.075,
    size = 0.25,
    color = gray(0.5)
  ) +
  geom_point(aes(x = estimate),
             size = 1.5, color = gray(0.5)) +
  geom_point(
    aes(x = estimand, y = 0.1),
    size = 1.5,
    fill = NA,
    color = gray(0.5),
    pch = 24
  ) +
  geom_line(
    data = estimation_df_1,
    size = 0.1,
    aes(x = x, y = density),
    color = dd_palette("dd_light_blue"),
  ) +
  geom_vline(
    xintercept = 0,
    color = gray(0.5),
    linetype = "dashed",
    alpha = 0.5
  ) +
  geom_text(
    aes(x = estimand + 0.08, y = 0.35, label = paste0("Estimand: ",format_num(estimand, digits = 2))),
    hjust = 0.5,
    size = 2,
  ) +
  geom_text(
    aes(x = 0 , y = -0.2, label = paste0("Estimate: ",format_num(estimate, digits = 2))),
    size = 2, hjust = 1, nudge_x = -0.1
  ) +
  geom_text(
    aes(x = 1.05, y = 2, label = paste0("Estimated\nsampling distribution:\nN(", format_num(estimate, digits = 2), ", ", format_num(std.error, digits = 2), ")")),
    size = 2.5, hjust = 1
  ) +
  geom_text(
    aes(x = 0, y = -0.2, label = paste0("95% CI: ", make_interval_entry(conf.low, conf.high, digits = 2))),
    hjust = 0, nudge_x = 0.1,
    size = 2
  ) +
  coord_cartesian(xlim = c(-0.5, 1), ylim = c(-0.18, NA)) +
  theme_void() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  ) 

g <- g_estimation_1 + g_hypothesis_1

ggsave(here::here("maintained", "output", "figure_10.3.svg"), g, width = 5, height = 1.5)
ggsave(here::here("maintained", "output", "figure_10.3.pdf"), g, width = 5, height = 1.5)

  



