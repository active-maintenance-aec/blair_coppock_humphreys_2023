# blair_coppock_humphreys_2023 — figure_18.12.R
# Output: figure_18.12.pdf
# Output: figure_18.12.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_18.12.R

source(here::here("march_2026_rewrite", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)
library(vayr)
library(geomtextpath)

library(geomtextpath)


source(here::here("original", "code", "declarations", "declaration_18.10.R"))

dat <- draw_data(declaration_18.10)

dat <-
  dat |>
  as_tibble() |>
  readr::type_convert() |>
  group_by(periods, Z) |>
  mutate(
    y = vayr::sunflower(y = Z, density = 0.5, aspect_ratio = 0.667),
    x = vayr::sunflower(x = periods, density = 0.5, aspect_ratio = 0.667)
  ) |>
  ungroup()

line_length <- 0.1

line_df <- 
  dat |>
  pivot_wider(
    id_cols = units,
    names_from = periods,
    values_from = c(Z, x, y)
  ) |> 
  left_join(expand_grid(distinct(dat, units), q = seq(from = 0, to = 0.9, by = line_length))) |> 
  mutate(
    x1_start = x_1 + q * (x_2 - x_1),
    x1_end = x_1 + (q + line_length) * (x_2 - x_1),
    y1_start = y_1 + q * (y_2 - y_1),
    y1_end = y_1 + (q + line_length) * (y_2 - y_1),
    x2_start = x_2 + q * (x_3 - x_2),
    x2_end = x_2 + (q + line_length) * (x_3 - x_2),
    y2_start = y_2 + q * (y_3 - y_2),
    y2_end = y_2 + (q + line_length) * (y_3 - y_2)
  )  |> 
  mutate(color1 = case_when(Z_1 == 1 & Z_2 == 1 ~ 1, Z_1 == 0 & Z_2 == 0 ~ 0, TRUE ~ q),
         color2 = case_when(Z_2 == 1 & Z_3 == 1 ~ 1, TRUE ~ q))

count_df <- 
  dat |> 
  count(periods, Z) 

g <- 
ggplot(dat |> mutate(q = Z)) +
  geom_segment(data = line_df, aes(x = x1_start, xend = x1_end, y = y1_start, yend = y1_end, color = color1), alpha = 0.1) +
  geom_segment(data = line_df, aes(x = x2_start, xend = x2_end, y = y2_start, yend = y2_end, color = color2), alpha = 0.1) +
  geom_point(aes(x, y, color = q), stroke = 0) + 
  geom_text(data = count_df, aes(x = periods, y = Z + 0.3, label = paste0("n = ", n)), size = 3) + 
  scale_color_gradient(low = dd_palette("dd_dark_blue"), high = dd_palette("dd_pink"))  +
  scale_y_continuous(breaks = c(0, 1), labels = c("Control", "Treated")) +
  labs(x = "Time period", y = "") +
  scale_x_continuous(breaks = 1:3, labels = 1:3) + 
  theme_dd() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

ggsave(here::here("march_2026_rewrite", "output", "figure_18.12.pdf"),
       g,
       width = 6.5,
       height = 3)
ggsave(here::here("march_2026_rewrite", "output", "figure_18.12.svg"),
       g,
       width = 6.5,
       height = 3)
