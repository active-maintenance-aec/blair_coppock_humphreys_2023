# blair_coppock_humphreys_2023 — figure_16.8.R
# Output: figure_16.8.pdf
# Output: figure_16.8.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_16.8.R

source(here::here("maintained", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)
library(ggtext)
source(here::here("maintained", "utilities", "make_dag_df.R"))
dag <- dagify(Y ~ D + X + U,
              D ~ X,
              X ~ U)
nodes <-
  tibble(
    name = c("U", "X", "D", "Y"),
    label = c("U", "X", "D", "Y"),
    annotation = c(
      "**Unknown heterogeneity**",
      "**Running variable**",
      "**Treatment**<br>X > cutoff",
      "**Outcome variable**"
    ),
    x = c(5, 1, 4, 5),
    y = c(4, 2.5, 2.5, 1), 
    nudge_direction = c("N", "S", "N", "S"),
    data_strategy = "unmanipulated",
    answer_strategy = c("uncontrolled", "controlled", "uncontrolled", "uncontrolled")
  )
ggdd_df <- make_dag_df(dag, nodes)
g <- base_dag_plot %+% ggdd_df
g
ggsave(here::here("maintained", "output", "figure_16.8.pdf"), g, width = 6.5, height = 4)
ggsave(here::here("maintained", "output", "figure_16.8.svg"), g, width = 6.5, height = 4)
