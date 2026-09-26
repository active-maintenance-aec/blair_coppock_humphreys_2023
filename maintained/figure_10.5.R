# blair_coppock_humphreys_2023 — figure_10.5.R
# Output: figure_10.5.svg
# Output: figure_10.5.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_10.5.R

source(here::here("maintained", "helpers.R"))

library(ggforce)


set.seed(343)

n <- 50000

points_df_1 <-
  tibble(
    rho = sqrt(runif(n)),
    theta = runif(n, 0, 2 * pi),
    x = rho * cos(theta),
    y = rho * sin(theta),
    x_center = 0,
    y_center = 0
  )

points_df_2 <-
  tibble(
    rho = sqrt(runif(n, min = 0, max = 0.015)),
    theta = runif(n, 0, 2 * pi),
    x = rho * cos(theta) + 2.5,
    y = rho * sin(theta),
    x_center = 2.5,
    y_center = 0
  )

points_df_3 <-
  tibble(
    rho = sqrt(runif(n, min = 0, max = 0.015)),
    theta = runif(n, 0, 2 * pi),
    x = rho * cos(theta) + 5.5,
    y = rho * sin(theta) - 0.2,
    x_center = 5,
    y_center = 0
  )

lotsa_points_df <-
  bind_rows(points_df_1, points_df_2, points_df_3, .id = "circle")

summary_df <- 
  lotsa_points_df |>
  group_by(circle) |>
  summarize(
    x_center_bar = mean(x_center),
    y_center_bar = mean(y_center),
    bias = sqrt((mean(x) - x_center_bar)^2 + (mean(y) - y_center_bar)^2),
    variance = var(sqrt((x - mean(x))^2 + (y - mean(y))^2)),
    mse = bias^2 + variance,
    rmse = sqrt(mse),
    std_dev = sqrt(variance),
    label =  glue::glue("SD = {format_num(std_dev, 2)}\nBias = {format_num(bias, 2)}\nRMSE = {format_num(rmse, 2)}")
  ) |>
  mutate(
    label_x = c(0, 2.5, 5),
    title = c("Unbiased, imprecise", "Unbiased, precise", "Biased, precise")
  )

points_df <- 
  lotsa_points_df |> 
  mutate(S = strata_rs(strata = circle, n = 50)) |> 
  filter(S == 1) |> 
  select(-S)

g <-
  ggplot() +
  geom_circle(
    data = tibble(
      x0 = rep(c(0, 2.5, 5), each = 4),
      y0 = rep(0, each = 12),
      r = rev(rep(seq(0.1, 1, length.out = 4), 3))
    ),
    aes(
      x0 = x0,
      y0 = y0,
      r = r,
      fill = fct_rev(as.factor(r))
    ),
    col = gray(0.25),
    lwd = 0,
    alpha = 0.5
  ) +
  geom_point(data = points_df, aes(x, y), size = 0.5) +
  scale_fill_manual(values = rev(c("yellow", "red", "cyan", "black"))) +
  coord_fixed(ylim = c(-1.25, 2.2)) +
  xlab("") + ylab("") +
  # top labeling
  geom_text(
    data = summary_df,
    aes(x = label_x, label = label),
    y = 1.5,
    size = 3,
    hjust = 0.5,
    lineheight = 0.9
  ) +
  geom_text(
    data = summary_df,
    aes(x = label_x, label = title),
    y = 2,
    size = 4,
    hjust = 0.5
  ) +
  # legend
  geom_point(data = tibble(
    x = runif(7, min = 1.5, max = 1.65),
    y = runif(7, min = -1.3, max = -1.15)
  ),
  aes(x = x, y = y),
  size = 0.5) +
  geom_circle(
    aes(x0 = 2.85, y0 = -1.25, r = 0.1),
    fill = "#C4A125",
    alpha = 0.6,
    col = gray(0.25)
  ) +
  geom_text(
    data = tibble(
      x = c(1.75, 3),
      label = c("Estimates", "Estimand")
    ),
    aes(x = x, y = -1.25, label = label),
    size = 3,
    hjust = 0
  ) +
  theme_dd() + 
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

ggsave(here::here("maintained", "output", "figure_10.5.svg"),
       g,
       width = 6.5,
       height = 3)
ggsave(here::here("maintained", "output", "figure_10.5.pdf"),
       g,
       width = 6.5,
       height = 3)
