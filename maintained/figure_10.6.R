# blair_coppock_humphreys_2023 — figure_10.6.R
# Output: figure_10.6.svg
# Output: figure_10.6.pdf
# Depends on: helpers.R
# Description: Maintained rewrite of figure_10.6.R

source(here::here("maintained", "helpers.R"))

library(ggforce)


set.seed(343)

n <- 5000

lotsa_points_df <- 
  tibble(
  rho = sqrt(runif(n, min = 0, max = 0.015)),
  theta = runif(n, 0, 2*pi),
  x = rho * cos(theta),
  y = rho * sin(theta)
)

left_df <-
  lotsa_points_df |> 
  mutate(
    x_center = 0,
    y_center = 0
  )

right_df <-
  lotsa_points_df |> 
  mutate(
    x_center = 0,
    y_center = 0.35
  )

summary_df <- 
  bind_rows(left_df, right_df, .id = "circle") |>
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
  mutate(x = c(-1.15, 2.5),
         y = c(0, 0),
         hjust = c(1, 0),
         title_y =  c(0.5, 0.5),
         title_label = c("Left Inquiry", "Right Inquiry")
  )

points_df <- sample_n(lotsa_points_df, 50)

circle_df <- 
  bind_rows(
    "left" = tibble(
      x0 = rep(0, 4),
      y0 = rep(0, 4),
      r = rev(seq(0.1, 1, length.out = 4))
    ),
    "right" = tibble(
      x0 = rep(0.35, 4),
      y0 = rep(0, 4),
      r = rev(seq(0.1, 1, length.out = 4))
    ),
    .id = "side"
  ) |> 
  arrange(desc(r), side)

g <- 
  ggplot() + 
  geom_circle(
    data = circle_df, 
    aes(x0 = x0, y0 = y0, r = r, fill = fct_rev(as.factor(r))),
    col = gray(0.25), lwd = 0, alpha = 0.5) +
  geom_point(data = points_df, aes(x, y), size = 0.5) + 
  geom_point(data = tibble(x = runif(7, min = -0.75, max = -0.75 + 0.15), y = runif(7, min = -1.3, max = -1.15)), aes(x = x, y = y), size = 0.5) + 
  geom_circle(aes(x0 = 0.5, y0 = -1.25, r = 0.1), fill = "#C4A125", alpha = 0.6, col = gray(0.25)) +
  geom_text(data = tibble(x = c(-0.5, 0.75), label = c("Estimates", "Estimand")), aes(x = x, y = -1.25, label = label), size = 3, hjust = 0) + 
  geom_text(data = summary_df, aes(label = label, x = x, y = y), hjust = 1, size = 3) +
  geom_text(data = summary_df, aes(label = title_label, x = x, y = title_y), hjust = 1, size = 4) +
  scale_fill_manual(values = rev(c("yellow", "red", "cyan", "black"))) + 
  coord_fixed(xlim = c(-2.5, 2.5), ylim = c(-1.5,1.5)) +
  theme_dd() + 
  labs(x = "", y = "") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

ggsave(here::here("maintained", "output", "figure_10.6.svg"),
       g,
       width = 5.5,
       height = 3)
ggsave(here::here("maintained", "output", "figure_10.6.pdf"),
       g,
       width = 5.5,
       height = 3)

