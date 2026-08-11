# blair_coppock_humphreys_2023 — figure_18.7.R
# Output: figure_18.7.pdf
# Output: figure_18.7.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_18.7.R

source(here::here("march_2026_rewrite", "helpers.R"))


diagnosis_18.7 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_18.7.rds"))

labeller <- 
  . %>% 
  mutate(
  inquiry_fac = factor(inquiry, levels = c(
    "ATE_Z1",
    "CATE_Z1_Z2_0",
    "CATE_Z1_Z2_1",
    "diff_in_CATEs_Z1",
    "ATE_Z2",
    "CATE_Z2_Z1_0",
    "CATE_Z2_Z1_1",
    "diff_in_CATEs_Z2"
  ),
  labels = c(
    "ATE of Z1",
    "CATE of Z1 when Z2 = 0",
    "CATE of Z1 when Z2 = 1",
    "Difference-in-CATEs of Z1",
    "ATE of Z2",
    "CATE of Z2 when Z1 = 0",
    "CATE of Z2 when Z1 = 1",
    "Difference-in-CATEs of Z2"
  )
  )
)

simulations <-
  diagnosis_18.7 |> 
  get_simulations() |> 
  labeller()

diagnosands_df <-
  diagnosis_18.7 |>
  tidy() |>
  filter(diagnosand == "power") |> 
  labeller()
  
g <- 
ggplot(diagnosands_df, aes(N, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = dd_palette("two_color_palette")[1], alpha = 0.1) + 
  geom_point(color = dd_palette("two_color_palette")[1]) +
  geom_line(color = dd_palette("two_color_palette")[1]) +
  geom_hline(yintercept = 0.80, linetype = "dashed") +
  theme_dd() +
  theme(strip.text = element_text(size = 8)) + 
  facet_wrap(~inquiry_fac, nrow = 2) +
  labs(x = "Sample size", y = "Statistical power")

g

ggsave(here::here("march_2026_rewrite", "output", "figure_18.7.pdf"), g, width = 6.5, height = 6.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_18.7.svg"), g, width = 6.5, height = 6.5)

