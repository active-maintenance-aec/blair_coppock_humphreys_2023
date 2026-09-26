# blair_coppock_humphreys_2023 — figure_6.2.R
# Output: figure_6.2.pdf
# Output: figure_6.2.svg
# Depends on: helpers.R
# Description: Maintained rewrite of figure_6.2.R

source(here::here("maintained", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)
library(ggtext)
source(here::here("maintained", "utilities", "make_dag_df.R"))
dag <- dagify(
  Y ~ X2 + X1 + M + U,
  M ~ D,
  K ~ D + U,
  D ~ Z + X1
)
nodes <-
  tibble(
    name = c("Z", "X1", "U", "Y", "X2", "K", "M", "D"),
    label = c("Z", "X1", "U", "Y", "X2", "K", "M", "D"),
    annotation = c(
      "**Instrument**",
      "**Confounder**",
      "**Unknown heterogeneity**",
      "**Outcome**",
      "**Moderator**",
      "**Collider**",
      "**Mediator**",
      "**Explanatory<br>variable**"
    ),
    x = c(1, 3.5, 5, 5, 5, 3.5, 3.5, 2),
    y = c(2.5, 4, 1, 2.5, 4, 1, 2.5, 2.5), 
    nudge_direction = c("N", "N", "S", "E", "N", "S", "N", "S"),
    data_strategy = c("assignment", "unmanipulated", "unmanipulated", "unmanipulated", "unmanipulated", "unmanipulated", "unmanipulated", "unmanipulated"),
    answer_strategy = "uncontrolled",
    hjust = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1)
  )
ggdd_df <- make_dag_df(dag, nodes)
ggdd_df <- 
  ggdd_df |> 
  mutate(text_x = if_else(name == "D", text_x + .1, text_x))
g <- base_dag_plot %+% ggdd_df
ggsave(here::here("maintained", "output", "figure_6.2.pdf"), g, width = 7, height = 4)
ggsave(here::here("maintained", "output", "figure_6.2.svg"), g, width = 7, height = 4)
