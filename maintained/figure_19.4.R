# blair_coppock_humphreys_2023 — figure_19.4.R
# Output: figure_19.4.pdf
# Output: figure_19.4.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_19.4.R

source(here::here("march_2026_rewrite", "helpers.R"))



diagnosis_19a <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_19a.rds"))

gg_df <- 
  diagnosis_19a |>
  tidy() |>
  filter(inquiry == "delta" & diagnosand == "rmse") |>
  mutate(n = factor(n)) 

label_df <- 
  gg_df |> 
  filter(alpha == 0.5) |> 
  mutate(x = c(0.6, 0.6),
         y = c(0.035, 0.06))

g <- 
  ggplot(gg_df) +
  aes(alpha, estimate, color = n, fill = n) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.25, linetype = 0) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_text(data = label_df, aes(x, y, label = paste("Rounds:", n))) +
  scale_color_manual(values = dd_palette("two_color_palette")) +
  scale_fill_manual(values = dd_palette("two_color_palette")) +
  theme_dd() +
  ylab("RMSE on discount factor") +
  xlab("Share playing non strategically") 
g

# ggsave(here::here("march_2026_rewrite", "output", "figure_19.4.pdf"),
#        g,
#        width = 6.5,
#        height = 4)
# ggsave(here::here("march_2026_rewrite", "output", "figure_19.4.svg"),
#        g,
#        width = 6.5,
#        height = 4)
