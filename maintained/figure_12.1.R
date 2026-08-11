# blair_coppock_humphreys_2023 — figure_12.1.R
# Output: figure_12.1.svg
# Output: figure_12.1.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_12.1.R

source(here::here("march_2026_rewrite", "helpers.R"))



diagnosis_12.2 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_12.2.rds"))

gg_df <-
  diagnosis_12.2 |>
  get_diagnosands() |>
  filter(inquiry == "ATE_social") |>
  pivot_longer(cols = c(bias, rmse, power, cost), names_to =  "diagnosand") |>
  mutate(n_villages = factor(n_villages))

g_base <-
  ggplot(data = NULL,
         aes(
           citizens_per_village,
           value,
           group = n_villages,
           color = n_villages
         )) +
  geom_line() +
  scale_color_manual(values = dd_palette("two_color_palette")) +
  coord_cartesian(xlim = c(25, 100)) +
  theme(legend.key.height = unit(1.75, units = "cm"))

g1 <-
  g_base %+% filter(gg_df, diagnosand == "bias") +
  labs(x = "Citizens per village", y = "Bias", color = "Number of\nvillages") +
  scale_y_continuous(limits = c(-0.025, 0.025)) +
  theme_dd()

g2 <-
  g_base %+%
  filter(gg_df, diagnosand == "power") +
  labs(x = "Citizens per village", y = "Statistical power", color = "Number of\nvillages")  +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(
    yintercept = 0.80,
    color = dd_palette("dd_gray"),
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = 75,
    y = 0.72,
    label = "Conventional power target: 0.8",
    size = 3,
    color = dd_palette("dd_gray")
  ) +
  theme_dd()

g3 <-
  g_base %+%
  filter(gg_df, diagnosand == "rmse") +
  labs(x = "Citizens per village", y = "Root mean-squared error", color = "Number of\nvillages") +
  scale_y_continuous(limits = c(0, 0.05)) +
  theme_dd()

label_df <-
  tibble(
    x = c(75, 30),
    y = c(10000, 40000),
    label = c("192 villages\nsampled", "All 500 villages\nsampled"),
    n_villages = factor(c(192, 500)),
    hjust = c(0, 0)
  )

g4 <-
  g_base %+%
  filter(gg_df, diagnosand == "cost") +
  labs(x = "Citizens per village", y = "Cost", color = "Number of\nvillages") +
  geom_text(data = label_df, aes(
    x = x,
    y = y,
    label = label,
    hjust = hjust
  )) +
  theme_dd()

g <- wrap_plots(g1, g2, g3, g4, guides = "collect")

ggsave(here::here("march_2026_rewrite", "output", "figure_12.1.svg"),
       g,
       width = 7,
       height = 5)
ggsave(here::here("march_2026_rewrite", "output", "figure_12.1.pdf"),
       g,
       width = 7,
       height = 5)
