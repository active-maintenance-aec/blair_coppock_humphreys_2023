# blair_coppock_humphreys_2023 — figure_11.2.R
# Output: figure_11.2.svg
# Output: figure_11.2.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_11.2.R

source(here::here("march_2026_rewrite", "helpers.R"))

library(RColorBrewer)


diagnosis_11.2 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_11.2.rds"))

gg_df <-
  diagnosis_11.2 |>
  get_simulations() |> 
  mutate(significant = as.numeric(p.value <= 0.05),
         N = as.factor(N))

label_df <-
  tibble(N = as.factor(c(100, 500, 1000)),
         estimand = c(0.33, 0.23, 0.12),
         significant = c(0.2, 0.48, 0.85),
         label = paste0("N = ", N)
  )

gradient_color <- colorRampPalette(colors = c(dd_palette("dd_light_blue"), dd_palette("dd_dark_blue")))


g <-
  ggplot(gg_df, aes(estimand, significant, color = N, fill = N,  group = N)) +
  geom_smooth(method = 'loess',  formula = 'y ~ x', alpha = 0.1) +
  geom_text(data = label_df, aes(label = label)) +
  geom_hline(yintercept = 0.8,
             color = dd_palette("dd_light_gray"),
             linetype = "dashed") +
  theme_dd() +
  scale_color_manual(values = gradient_color(3)) +
  scale_fill_manual(values = gradient_color(3)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 0.5)) +
  labs(x = "True effect size",
       y = "Statistical power")

ggsave(here::here("march_2026_rewrite", "output", "figure_11.2.svg"),
       g,
       width = 6.5,
       height = 3.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_11.2.pdf"),
       g,
       width = 6.5,
       height = 3.5)
