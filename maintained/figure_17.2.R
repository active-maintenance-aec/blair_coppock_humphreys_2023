# blair_coppock_humphreys_2023 — figure_17.2.R
# Output: figure_17.2.pdf
# Output: figure_17.2.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_17.2.R

source(here::here("maintained", "helpers.R"))

library(geomtextpath)


diagnosis_17.3 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_17.3.rds"))

label_df <- 
  tibble(
    proportion_hiding = 0,
    estimator_lbl = c("List experiment", "Direct question"),
    label = c("List experiment", "Direct question")
  )

gg_df <- 
  diagnosis_17.3 |>
  get_diagnosands()|>
  mutate(estimator_lbl = factor(estimator, levels = c("direct", "list"), labels = c("Direct question", "List experiment"))) |> 
  mutate(facet_label = paste0("Proportion 'liars': ", format_num(proportion_hiding, 1)))

gg_df <-
  left_join(gg_df, label_df)

g <-
  ggplot(gg_df, aes(N, rmse, group = estimator_lbl, color = estimator_lbl)) +
  geom_line() +
  geom_textpath(aes(label = label), linewidth = 0, offset = unit(0.15, "cm")) +
  scale_color_manual("Question type", values = dd_palette("two_color_palette")) +
  labs(y = "Diagnosand: RMSE", x = "Sample size") +
  facet_wrap( ~ facet_label, ncol = 4) +
  theme_dd() +
  theme(axis.text = element_text(size = 7))

ggsave(here::here("maintained", "output", "figure_17.2.pdf"), g, width = 7, height = 5.5)
ggsave(here::here("maintained", "output", "figure_17.2.svg"), g, width = 7, height = 5.5)

