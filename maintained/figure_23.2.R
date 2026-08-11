# blair_coppock_humphreys_2023 — figure_23.2.R
# Output: figure_22.3.pdf
# Output: figure_23.2.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_23.2.R

source(here::here("march_2026_rewrite", "helpers.R"))

library(geomtextpath)


simulations <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_23a.rds"))

gg_df <-
  simulations |>
  pivot_longer(
    cols = c(replication_outside, original_outide, dis_sig, dis_nonequiv),
    names_to = "diagnosand",
    values_to = "diagnosand_value"
  ) |>
  group_by(N, diagnosand) |>
  summarize(tidy(lm_robust(diagnosand_value ~ 1))) |>
  mutate(diagnosand = factor(
    diagnosand,
    levels = c(
      "replication_outside",
      "original_outide",
      "dis_sig",
      "dis_nonequiv"
    ),
    labels = c(
      "Replication outside of original CI",
      "Original outside of replication CI",
      "Difference-in-SATEs significant",
      "SATEs not equivalent"
    )
  ))



g <-
ggplot(gg_df, aes(N, estimate, group = diagnosand)) +
  geom_point(color = dd_palette("dd_dark_blue")) +
  geom_line(color = dd_palette("dd_dark_blue")) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = dd_palette("dd_dark_blue"), alpha = 0.05) +
  geom_textline(data = filter(gg_df, diagnosand != "Difference-in-SATEs significant"), 
                mapping = aes(label = diagnosand), 
                offset = unit(5, "pt"), color = dd_palette("dd_dark_blue"), size = 4) +
  geom_textline(data = filter(gg_df, diagnosand == "Difference-in-SATEs significant"), 
                mapping = aes(label = diagnosand), 
                offset = unit(-13, "pt"), color = dd_palette("dd_dark_blue"), size = 4) +
  theme_dd() +
  ylim(0, 1.0) +
  labs(x = "Replication Study Sample Size",
       y = "Fraction of 'failures to replicate'")

g

grDevices::cairo_pdf("figures/figure_23.2.pdf" ,width = 6.5, height = 6.5)
print(g)
dev.off()

# some mystery bug
# ggsave(here::here("march_2026_rewrite", "output", "figure_22.3.pdf"),
#        g,
#        width = 6.5,
#        height = 6.5)

ggsave(here::here("march_2026_rewrite", "output", "figure_23.2.svg"),
       g,
       width = 6.5,
       height = 6.5)

