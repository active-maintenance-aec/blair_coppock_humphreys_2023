# blair_coppock_humphreys_2023 — figure_16.9.R
# Output: figure_16.9.pdf
# Output: figure_16.9.svg
# Depends on: original/code/declarations/, helpers.R
# Description: Maintained rewrite of figure_16.9.R

source(here::here("maintained", "helpers.R"))
library(rdrobust)


set.seed(343)

source(here::here("original", "code", "declarations", "declaration_16.5.R"))

mock_data <- draw_data(declaration_16.5)

X <- seq(-0.2, 1.2, .005)

treatment_frame <-
  data.frame(
    X = X,
    Y = treatment(X),
    observed = ifelse(X > cutoff, "a", "b"),
    D = 1
  )
control_frame <-
  data.frame(
    X = X,
    Y = control(X),
    observed = ifelse(X <= cutoff, "a", "b"),
    D = 0
  )

plot_frame <- rbind(treatment_frame, control_frame)

line_df <- tibble(
  x = cutoff,
  xend = cutoff,
  y = control(cutoff),
  yend = treatment(cutoff)
)

g <-
  ggplot(plot_frame) +
  aes(x = X, y = Y, color = as.factor(D)) +
  geom_line(aes(linetype = observed)) +
  geom_point(data = mock_data,
             stroke = 0,
             alpha = 0.25) +
  geom_vline(xintercept = cutoff, linewidth = 0.2) +
  geom_segment(
    data = line_df,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ), color = "black") +
  scale_linetype_discrete(name = "",
                          labels = c("Observable", "Unobservable")) +
  scale_color_manual(
    name = "",
    values = dd_palette("two_color_palette"),
    labels = c("Untreated", "Treated")
  ) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.25)) +
  labs(x = "Running variable (X)", y = "Outcome variable (Y)") +
  theme_dd()

ggsave(here::here("maintained", "output", "figure_16.9.pdf"), g, width = 6.5, height = 5)
ggsave(here::here("maintained", "output", "figure_16.9.svg"), g, width = 6.5, height = 5)
