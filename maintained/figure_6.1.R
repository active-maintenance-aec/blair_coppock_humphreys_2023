# blair_coppock_humphreys_2023 — figure_6.1.R
# Output: figure_6.1.pdf
# Output: figure_6.1.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_6.1.R

source(here::here("march_2026_rewrite", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)
library(ggtext)
source(here::here("march_2026_rewrite", "utilities", "make_dag_df.R"))
dag <- dagify(Y ~ Z + U)
nodes <-
  tibble(
    name = c("Z", "U", "Y"),
    label = c("Z", "U", "Y"),
    annotation = c(
      "**Treatment assignment**",
      "**Unknown heterogeneity**",
      "**Outcome**"
    ),
    x = c(1, 5, 5),
    y = c(1.5, 3.5,  1.5), 
    nudge_direction = c("S", "N", "S"),
    data_strategy = c("assignment", "unmanipulated", "unmanipulated"),
    answer_strategy = "uncontrolled"
  )
ggdd_df <- make_dag_df(dag, nodes)
g <- base_dag_plot %+% ggdd_df
ggsave(here::here("march_2026_rewrite", "output", "figure_6.1.pdf"), g, width = 7, height = 3.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_6.1.svg"), g, width = 7, height = 3.5)
