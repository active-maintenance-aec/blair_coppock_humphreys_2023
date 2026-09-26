# blair_coppock_humphreys_2023 — figure_11.3.R
# Output: figure_11.3.svg
# Output: figure_11.3.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_11.3.R

source(here::here("maintained", "helpers.R"))



diagnosis_11.3 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_11.3.rds"))

gg_df <- 
  diagnosis_11.3 |>
  get_diagnosands() |>
  mutate(
    is_over = factor(cost >= 5000),
    is_under = factor(cost <= 5000),
    prob = as.factor(prob)
  ) 

g1 <- 
  ggplot(data = NULL) +
  aes(N, cost, group = prob, color = prob) + 
  geom_line(data = filter(gg_df, is_over == TRUE), linetype = "dashed") + 
  geom_line(data = filter(gg_df, is_under == TRUE)) + 
  scale_color_manual(values = dd_palette("three_color_palette")) + 
  scale_y_continuous(labels = dollar_format()) +
  labs(x = "Sample size", y = "Diagnosand: cost") + 
  geom_hline(yintercept = 5000, color = dd_palette("dd_gray")) 

y_range <- layer_scales(g1)$y$range$range
x_range <- layer_scales(g1)$x$range$range
x_to_y <- (x_range[2] - x_range[1])/(y_range[2] - y_range[1])*0.5

labels_df <- 
  tibble(
    prob = seq(0.1, 0.5, 0.2),
    x = c(750, 750, 750),
    y = x * 2 + prob * x * 20
  ) |> 
  mutate(
    slope = 2 + prob * 20,
    angle = atan(slope * x_to_y) * 180 / pi,
    prob = as.factor(prob),
    color = dd_palette("three_color_palette"),
    label = paste0("Probability of treatment: ", prob)
  )

g1 <- g1 + 
  coord_equal(ratio = x_to_y) +
  annotate("text", x = 0, y = 5500, label = "Budget: $5,000", hjust = 0) + 
  geom_text(data = labels_df, 
            aes(linetype = NULL, x = x, y = y, angle = angle, color = prob, label = label),
            nudge_y = 500) + 
  theme_dd()

g2 <- 
  ggplot(data = NULL) +
  aes(N, rmse, group = prob, color = prob) + 
  geom_line(data = filter(gg_df, is_over == TRUE), linetype = "dashed") + 
  geom_line(data = filter(gg_df, is_under == TRUE)) + 
  labs(x = "Sample size", y = "Diagnosand: root mean-squared error") + 
  coord_cartesian(ylim = c(0, 0.3)) + 
  scale_color_manual(values = dd_palette("three_color_palette")) + 
  annotate("text", y = 0.2, x = 700, hjust = 0, label = "Feasible design with\nlowest RMSE") + 
  annotate(
    geom = "curve", x = 680, y = 0.2, xend = 640, yend = 0.09, 
    curvature = .3, arrow = arrow(length = unit(2, "mm")),
    lty = "dashed", color = dd_palette("dd_gray")
  ) + 
  theme_dd()

y_range_2 <- layer_scales(g2)$y$range$range
x_range_2 <- layer_scales(g2)$x$range$range
x_to_y_2 <- (x_range_2[2] - x_range_2[1])/(y_range_2[2] - y_range_2[1])*0.5

g2 <- g2 + 
  coord_equal(ratio = x_to_y_2) 

g <- g1 / g2

ggsave(here::here("maintained", "output", "figure_11.3.svg"), g, width = 6.5, height = 6.5)
ggsave(here::here("maintained", "output", "figure_11.3.pdf"), g, width = 6.5, height = 6.5)

