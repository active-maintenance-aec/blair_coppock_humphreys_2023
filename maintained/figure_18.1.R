# blair_coppock_humphreys_2023 — figure_18.1.R
# Output: figure_18.1.pdf
# Output: figure_18.1.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_18.1.R

source(here::here("march_2026_rewrite", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)
library(ggtext)
source(here::here("march_2026_rewrite", "utilities", "make_dag_df.R"))
dag <- dagify(Y ~ Z + Q + U)
nodes <-
  tibble(
    name = c("Y", "Z", "U", "Q"),
    label = c("Y", "Z", "U", "Q"),
    annotation = c(
      "**Outcome**",
      "**Random assignment**",
      "**Unknown heterogeneity**",
      "**Measurement procedure**"),
    x = c(5, 1, 5, 1),
    y = c(2.5, 2.5, 4, 4),
    nudge_direction = c("S", "S", "N", "N"),
    data_strategy = c("unmanipulated", "assignment","unmanipulated", "measurement"),
    answer_strategy = "uncontrolled"
  )
ggdd_df <- make_dag_df(dag, nodes)
g <- base_dag_plot %+% ggdd_df + coord_fixed(ylim = c(2, 4.5), xlim = c(0.5, 5.5))
ggsave(here::here("march_2026_rewrite", "output", "figure_18.1.pdf"), g, width = 7, height = 3.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_18.1.svg"), g, width = 7, height = 3.5)
