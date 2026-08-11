# blair_coppock_humphreys_2023 — figure_18.3.R
# Output: figure_18.3.pdf
# Output: figure_18.3.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_18.3.R

source(here::here("march_2026_rewrite", "helpers.R"))



diagnosis_18.3 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_18.3.rds"))

diagnosands_df <- 
  diagnosis_18.3 |> 
  tidy() |> 
  filter(diagnosand == "sd_estimate") |> 
  mutate(facet_label = paste0("Control Slope: ", control_slope),
         facet_label = factor(facet_label, levels = paste0("Control Slope: ", seq(-1, 1, 0.5)))) |> 
  filter(control_slope %in% c(-1, 0, 1))

g1 <- 
  ggplot(diagnosands_df, aes(prob, estimate, color = estimator, shape = estimator)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = estimator, color = NULL), alpha = 0.1, show.legend = FALSE) + 
  geom_point() +
  geom_line(alpha = 0.8) + 
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2), limits = c(0, 1)) +
  ylim(0, NA) +
  facet_wrap(~facet_label, nrow = 1) +
  scale_color_manual(values = dd_palette("three_color_palette")) +
  scale_fill_manual(values = dd_palette("three_color_palette")) +
  theme_dd() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(x = "Fraction assigned to the treatment group",
       y = "Standard deviation of sampling distribution")

source(here::here("original", "code", "declarations", "declaration_18.3.R"))

designs <- redesign(declaration_18.3, control_slope = c(-1, 0, 1))

names(designs) <- c(-1, 0, 1)

dat <- 
  designs |> 
  map_df(draw_data, .id = "control_slope") |> 
  pivot_longer(c(Y_Z_1, Y_Z_0)) |> 
  mutate(
    name = if_else(name == "Y_Z_0", "Control potential outcome", "Treated potential outcome"),
    facet_label = paste0("Control Slope: ", control_slope),
    facet_label = factor(facet_label, levels = paste0("Control Slope: ", c(-1, 0, 1))))

g2 <- 
  ggplot(dat, aes(X, value, color = name, group = name)) +
  geom_point(alpha = 0.5, stroke = 0) + 
  facet_wrap(~facet_label, nrow = 1) +
  scale_color_manual(values = dd_palette("two_color_palette")) +
  theme_dd() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.spacing = unit(1, "lines")) +
  labs(x = "Covariate X", y = "Potential outcomes Y(Z)")

g <- g2 / g1

ggsave(here::here("march_2026_rewrite", "output", "figure_18.3.pdf"), g, width = 6.5, height = 6.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_18.3.svg"), g, width = 6.5, height = 6.5)

