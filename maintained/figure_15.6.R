# blair_coppock_humphreys_2023 — figure_15.6.R
# Output: figure_15.6.svg
# Output: figure_15.6.pdf
# Depends on: original/code/declarations/, helpers.R
# Description: Maintained rewrite of figure_15.6.R

source(here::here("maintained", "helpers.R"))


set.seed(343)

source(here::here("original", "code", "declarations", "declaration_15.6.R"))

correlation_mat <- 
  draw_data(declaration_15.6) |> 
  select(starts_with("Y_")) |> 
  cor()

gg_df <- 
  correlation_mat |>
  as_tibble(rownames = "x") |> 
  pivot_longer(cols = -x, names_to = "y") |> 
  mutate(
    x = factor(x, levels = rownames(correlation_mat)),
    y = factor(y, levels = rownames(correlation_mat)),
    label = format_num(value, digits = 1)
  ) |> 
  filter(as.numeric(x) <= as.numeric(y))

g <-
  ggplot(gg_df, aes(x, y, fill = value, color = value)) +
  geom_point(aes(size = value), stroke = 0) +
  geom_text(aes(label = label), color = dd_palette("dd_gray"))  +
  scale_size_continuous(range = c(10, 20)) +
  scale_color_gradient2(
    low = dd_palette("dd_pink"),
    mid = "white",
    high = dd_palette("dd_light_blue")
  ) +
  theme_dd() +
  theme(
    axis.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

ggsave(here::here("maintained", "output", "figure_15.6.svg"),
       g,
       width = 6.5,
       height = 6.5)
ggsave(here::here("maintained", "output", "figure_15.6.pdf"),
       g,
       width = 6.5,
       height = 6.5)
