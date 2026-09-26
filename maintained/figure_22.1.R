# blair_coppock_humphreys_2023 — figure_22.1.R
# Output: figure_22.1.svg
# Output: figure_22.1.pdf
# Depends on: helpers.R
# Description: Maintained rewrite of figure_22.1.R

source(here::here("maintained", "helpers.R"))

library(coefplot)

long_df <-
  bonilla_tillery |> 
  pivot_longer(cols = c(female, lgbtq))

levels_df <-
  long_df |>
  group_by(name, value) |>
  do(tidy(lm_robust(blm_support ~ Z, data = .))) |>
  filter(term != "(Intercept)")

differences_df <-
  long_df |>
  group_by(name) |>
  do(tidy(lm_robust(blm_support ~ Z*value, data = .))) |>
  filter(str_detect(term, ":value")) |> 
  mutate(term = str_remove(term, ":value"))

gg_df <- 
  bind_rows(`Conditional ATEs (CATEs)` = levels_df,
            `Differences-in-CATEs` = differences_df,
            .id = "inquiry_type") |> 
  mutate(coef_color = paste0(inquiry_type, value),
         covariate_facet = if_else(name == "female", "Gender", "Sexuality"),
         term_label = str_remove(term, "Z"),
         term_label = str_to_sentence(term_label),
         term_label = paste0(term_label, "\ntreatment"))


label_df <- 
  gg_df |> 
  filter(term == "Znationalism") |> 
  mutate(
    label = 
      case_when(name == "female" & value == "1" ~ "CATE | Women",
                name == "female" & value == "0" ~ "CATE | Men",
                name == "lgbtq" & value == "1" ~ "CATE | LGTBQ",
                name == "lgbtq" & value == "0" ~ "CATE | Non-LGBTQ",
                is.na(value) ~ "Difference-in-CATEs")
  )

g <-
  ggplot(gg_df,
         aes(
           estimate,
           term_label,
           group = value,
           color = coef_color,
           shape = coef_color
         )) +
  geom_point(position = position_dodgev(height = 0.2)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodgev(height = 0.2)) +
  geom_text(
    data = filter(label_df, inquiry_type == "Conditional ATEs (CATEs)"),
    aes(label = label),
    position = position_dodgev(height = 0.8),
    size = 2
  ) +
  geom_text(
    data = filter(label_df, inquiry_type != "Conditional ATEs (CATEs)"),
    aes(label = label),
    nudge_y = 0.2,
    size = 2
  ) +
  geom_vline(xintercept = 0,
             color = gray(0.5),
             linetype = "dashed") +
  scale_color_manual(values = dd_palette("three_color_palette")) +
  facet_grid(covariate_facet ~ inquiry_type) +
  labs(x = "Estimate") + 
  theme_dd() +
  theme(axis.title.y = element_blank())

ggsave(here::here("maintained", "output", "figure_22.1.svg"), g, height = 6.5, width = 6.5)
ggsave(here::here("maintained", "output", "figure_22.1.pdf"), g, height = 6.5, width = 6.5)

