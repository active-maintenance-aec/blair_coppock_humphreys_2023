# blair_coppock_humphreys_2023 — figure_18.9.R
# Output: figure_18.9.pdf
# Output: figure_18.9.svg
# Depends on: helpers.R
# Description: Maintained rewrite of figure_18.9.R

source(here::here("maintained", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)
library(ggtext)
source(here::here("maintained", "utilities", "make_dag_df.R"))
dag <- dagify(Y ~ D + type + U,
              D ~ Z + type + U,
              type ~ U)
nodes <-
  tibble(
    name = c("Z", "D", "U", "Y", "type"),
    label = c("Z", "D", "U", "Y", "C"),
    annotation = c(
      "**Random assignment**",
      "**Treatment received**",
      "**Unknown heterogeneity**",
      "**Outcome**",
      "**Principal stratum**<br>Compliance type"
    ),
    x = c(1, 3, 4, 5, 2),
    y = c(1, 1, 4, 1, 4),
    nudge_direction = c("S", "S", "N", "S", "N"),
    data_strategy = c("assignment", "unmanipulated", "unmanipulated", "unmanipulated", "unmanipulated"),
    answer_strategy = "uncontrolled", 
  )
ggdd_df <- make_dag_df(dag, nodes)
base_dag_plot + ggdd_df
g <- base_dag_plot + ggdd_df
ggsave(here::here("maintained", "output", "figure_18.9.pdf"), g, width = 6.5, height = 4.5)
ggsave(here::here("maintained", "output", "figure_18.9.svg"), g, width = 6.5, height = 4.5)
