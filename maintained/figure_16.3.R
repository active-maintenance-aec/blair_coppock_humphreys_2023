# blair_coppock_humphreys_2023 — figure_16.3.R
# Output: figure_16.3.svg
# Output: figure_16.3.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_16.3.R

source(here::here("maintained", "helpers.R"))

library(ggdag)
library(dagitty)

dag1 <- dagitty("dag{
                D <- X -> Y; D -> Y
                }")

dag2 <- dagitty("dag{
                D -> X <- Y ; D -> Y
                }")

dag3 <- dagitty("dag{
                D -> X -> Y; D -> Y
                }")

dag4 <- dagitty("dag{
                X -> Y; D -> Y
                }")

points_df_small <-
  tibble(name = c("D", "X", "Y", "U"),
         x = c(0, 1, 1, 0),
         y = c(0, 1, 0, 1))
ends_df_small <-
  tibble(to = c("D", "X", "Y", "U"),
         xend = c(0, 1, 1, 0),
         yend = c(0, 1, 0, 1))

gg_df_small <-
  list(dag1, dag2, dag3, dag4) |>
  map_df( ~ as_tibble(tidy_dagitty(.)), .id = "dag") |>
  mutate(dag = factor(
    dag,
    levels = 1:4,
    labels = c("X is a confounder", "X is a collider", "X is a mediator", "X is a predictor of Y")
  )) |>
  select(-x, -y, -xend, -yend) |>
  left_join(points_df_small) |>
  left_join(ends_df_small)

g <- 
  ggplot(gg_df_small, aes(x, y)) +
  geom_text(data = points_df_small, aes(label = name)) +
  geom_dag_edges(aes(xend = xend, yend = yend)) +
  coord_fixed(ylim = c(0, 1.1), xlim = c(-.1, 1.1)) +
  facet_grid(~dag) +
  theme_void()

ggsave(here::here("maintained", "output", "figure_16.3.svg"),
       g,
       width = 6.5,
       height = 2)
ggsave(here::here("maintained", "output", "figure_16.3.pdf"),
       g,
       width = 6.5,
       height = 2)
