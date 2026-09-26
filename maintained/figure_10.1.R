# blair_coppock_humphreys_2023 — figure_10.1.R
# Output: figure_10.1.svg
# Output: figure_10.1.pdf
# Depends on: helpers.R
# Description: DAG showing MIDA framework across multiple simulations.
# Fix: tidy_dagitty() |> as_tibble() |> select() instead of select() |> as_tibble()
# to avoid x/y column conflict with nodes_df in left_join.

source(here::here("maintained", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)
library(ggtext)
library(latex2exp)

dag <-
  dagify(amstar ~ mstar + I,
         dstar ~ mstar + D,
         adstar ~ dstar + A,
         mstar ~ M,
         m1 ~ M,
         am1 ~ m1 + I,
         d1 ~ D + m1,
         ad1 ~ A + d1,
         m2 ~ M,
         am2 ~ m2 + I,
         d2 ~ D + m2,
         ad2 ~ A + d2,
         mk ~ M,
         amk ~ mk + I,
         dk ~ D + mk,
         adk ~ A + dk)

dag_base <-
  tidy_dagitty(dag) |>
  as_tibble() |>
  select(name, direction, to, circular)

nodes_df <-
  tibble(
    name = c(
      "mstar", "amstar", "dstar", "adstar",
      "M", "I", "D", "A",
      "m1", "am1", "d1", "ad1",
      "m2", "am2", "d2", "ad2",
      "mk", "amk", "dk", "adk"),

    label =
      c("m<sup>*</sup>", "a<sub>m<sup>*</sup></sub>", "d<sup>*</sup>", "a<sub>d<sup>*</sup></sub>",
        "M", "I", "D", "A",
        "m<sub>1</sub>", "a<sub>m<sub>1</sub></sub>", "d<sub>1</sub>", "a<sub>d<sub>1</sub></sub>",
        "m<sub>2</sub>", "a<sub>m<sub>2</sub></sub>", "d<sub>2</sub>", "a<sub>d<sub>2</sub></sub>",
        "m<sub>k</sub>", "a<sub>m<sub>k</sub></sub>", "d<sub>k</sub>", "a<sub>d<sub>k</sub></sub>"),
    x = rep(c(0.5, 1.75, 3.25, 4.5), 5),
    y = rep(c(5, 4, 3, 2, 0), each = 4)
  )

endnodes_df <-
  nodes_df |>
  transmute(to = name, xend = x, yend = y)

gg_df <-
  dag_base |>
  left_join(nodes_df, by = "name") |>
  left_join(endnodes_df, by = "to")

gg_df <-
  gg_df |>
  mutate(
    arced_up = (name == "mstar" & to == "dstar"),
    arced_down = (name %in% c("m1", "m2", "mk") &
                    to %in% c("d1", "d2", "dk")),
    arced_left = (name %in% c("M", "I", "D", "A") &
                    to %in% c("m1", "am1", "d1", "ad1",
                              "m2", "am2", "d2", "ad2",
                              "mk", "amk", "dk", "adk"))) |>
  arrange(name)


gb_gray <- gray(0.2)

ellipses_df <- tibble(
  x = rep(c(0.5, 1.75, 3.25, 4.5), 3),
  y = rep(seq(0.85, 1.15, length.out = 3), each = 4)
)

g2 <-
  ggplot(data = gg_df) +
  aes(x = x,
      y = y,
      xend = xend,
      yend = yend) +
  geom_point(
    data = ellipses_df,
    aes(x = x, y = y),
    inherit.aes = FALSE,
    color = gb_gray,
    size = 1,
    stroke = 0
  ) +
  geom_point(
    data = (. %>% distinct(name, .keep_all = TRUE)),
    color = gray(.1),
    fill = NA,
    size = 14,
    stroke = 0.5,
    pch = 1
  ) +
  geom_dag_edges_arc(
    data = filter(gg_df, arced_up),
    curvature = 0.45,
    edge_width = 0.35,
    edge_color = gb_gray
  ) +
  geom_dag_edges_arc(
    data = filter(gg_df, arced_down),
    curvature = -0.45,
    edge_width = 0.35,
    edge_color = gb_gray
  ) +
  geom_dag_edges_arc(
    data = filter(gg_df, arced_left),
    curvature = -0.35,
    edge_width = 0.35,
    edge_color = gb_gray
  ) +
  geom_dag_edges(data_directed = filter(gg_df, !arced_up, !arced_down, !arced_left),
                 edge_width = 0.35,
                 edge_color = gb_gray) +
  geom_richtext(
    data = (. %>% distinct(name, .keep_all = TRUE)),
    color = "black",
    fill = "transparent",
    aes(label = label),
    size = 4,
    label.size  = NA) +
  annotate("text", x = -0.5, y = 5, angle = 90, size = 3, label = "Reality") +
  annotate("text", x = -0.5, y = 4, angle = 90, size = 3, label = "Design") +
  annotate("text", x = -0.5, y = 3, angle = 90, size = 3, label = "Simulation 1") +
  annotate("text", x = -0.5, y = 2, angle = 90, size = 3, label = "Simulation 2") +
  annotate("text", x = -0.5, y = 0, angle = 90, size = 3, label = "Simulation k") +
  coord_fixed(clip = "off") +
  theme_dag()

ggsave(here::here("maintained", "output", "figure_10.1.svg"), plot = g2, width = 6.5, height = 6.5)
ggsave(here::here("maintained", "output", "figure_10.1.pdf"), plot = g2, width = 6.5, height = 6.5)
