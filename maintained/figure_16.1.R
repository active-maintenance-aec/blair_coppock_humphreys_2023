# blair_coppock_humphreys_2023 — figure_16.1.R
# Output: figure_16.1.pdf
# Output: figure_16.1.svg
# Depends on: helpers.R
# Description: Maintained rewrite of figure_16.1.R

source(here::here("maintained", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)
library(ggtext)
source(here::here("maintained", "utilities", "make_dag_df.R"))
dag <- dagify(Y ~ W + M,
              M ~ X + W)
nodes <-
  tibble(
    name = c("X", "M", "W", "Y"),
    label = c("X", "M", "W", "Y"),
    annotation = c(
      "**Causal variable**",
      "**Hoop clue**",
      "**Smoking gun clue**",
      "**Outcome variable**"
    ),
    x = c(1, 3, 3, 5),
    y = c(1.5, 1.5, 3.5, 1.5), 
    nudge_direction = c("S", "S", "N", "S"),
    data_strategy = "unmanipulated",
    answer_strategy = "uncontrolled"
  )
ggdd_df <- make_dag_df(dag, nodes)
g <- base_dag_plot %+% ggdd_df
g
ggsave(here::here("maintained", "output", "figure_16.1.pdf"), g, width = 6.5, height = 4)
ggsave(here::here("maintained", "output", "figure_16.1.svg"), g, width = 6.5, height = 4)
