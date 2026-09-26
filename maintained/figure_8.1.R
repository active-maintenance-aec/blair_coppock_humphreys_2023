# blair_coppock_humphreys_2023 — figure_8.1.R
# Output: figure_8.1.pdf, figure_8.1.svg
# Depends on: helpers.R, utilities/make_dag_df.R
# Description: Experimental design DAG with MIDA components.
# Fix: tidy_dagitty() |> as_tibble() |> select() to avoid x/y column conflict.

source(here::here("maintained", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)
library(ggtext)

source(here::here("maintained", "utilities", "make_dag_df.R"))

dag <- dagify(Y ~ Ystar + Q + R,
              R ~ S + U,
              Ystar ~ D + U,
              D ~ Z + U,
              Z ~ S)
nodes <-
  tibble(
    name = c("S", "R", "Z", "Q", "D", "Ystar", "Y", "U"),
    label = c("S", "R", "Z", "Q", "D", "Y<sup>*</sup>", "Y", "U"),
    annotation = c(
      "**Sampling**",
      "**Response**",
      "**Treatment assignment**",
      "**Measurement tool**",
      "**Treatment received**",
      "**Latent outcome**",
      "**Observed outcome**",
      "**Unobserved**<br>**heterogeneity**"
    ),
    x = c(1, 3, 1, 5, 3, 4, 5, 2.5),
    y = c(4, 4, 1, 4, 1, 2.5, 2.5, 2.5),
    nudge_direction = c("N", "N", "S", "N", "S", "N", "S", "W"),
    answer_strategy = "uncontrolled"
  )

endnodes <-
  nodes |>
  transmute(to = name, xend = x, yend = y)

ggdd_df <-
  dag |>
  tidy_dagitty() |>
  as_tibble() |>
  select(name, direction, to, circular) |>
  mutate(
    data_strategy = case_when(
      name == "D" ~ "unmanipulated",
      name == "Q" ~ "measurement",
      name == "S" ~ "sampling",
      name == "Ystar" ~ "unmanipulated",
      name == "Z" ~ "assignment",
      name == "Y" ~ "unmanipulated",
      name == "U" ~ "unmanipulated",
      name == "R" ~ "unmanipulated"
    ),
    exclusion_restriction = "no"
  ) |>
  left_join(nodes, by = "name") |>
  left_join(endnodes, by = "to") |>
  left_join(nudges_df, by = c("x", "y", "nudge_direction")) |>
  left_join(aes_df, by = c("data_strategy", "answer_strategy")) |>
  mutate(shape_y = y + shape_nudge_y) |>
  mutate(text_x = if_else(name == "Ystar", text_x - 0.3, text_x),
         text_y = if_else(name == "Ystar", text_y - 0.1, text_y),
         hjust = 0.5)

g <- base_dag_plot + ggdd_df

ggsave(here::here("maintained", "output", "figure_8.1.pdf"), g, width = 7, height = 6.5)
ggsave(here::here("maintained", "output", "figure_8.1.svg"), g, width = 7, height = 6.5)
