# blair_coppock_humphreys_2023 — figure_17.1.R
# Output: figure_17.1.pdf, figure_17.1.svg
# Depends on: helpers.R, utilities/make_dag_df.R
# Description: List experiment DAG.

source(here::here("maintained", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)
library(ggtext)

source(here::here("maintained", "utilities", "make_dag_df.R"))

dag <- dagify(Y_direct ~ Y_star + S,
              Y_list ~ Y_star + X + Z,
              S ~ U,
              X ~ U,
              Y_star ~ U)

nodes <-
  tibble(
    name = c("U", "S", "Y_star", "X", "Y_direct", "Y_list", "Z"),
    label = c(
      "U",
      "S",
      "Y<sup>*</sup>",
      "X",
      "Y<sup>D</sup>",
      "Y<sup>L</sup>",
      "Z"
    ),
    annotation = c(
      "Unknown<br>heterogeneity",
      "**Sensitivity bias**",
      "**Latent**<br> Sensitive trait",
      "Control item count",
      "**Outcome 1**<br> Direct question",
      "**Outcome 2**<br> List question",
      "**Random assignment**<br>List experiment condition"
    ),
    x = c(1, 7/3, 7/3, 7/3, 11/3, 11/3, 5),
    y = c(2.5, 4, 2.5, 1, 4, 2.5, 2.5),
    nudge_direction = c("N", "N", "S", "S", "N", "N", "S"),
    hjust = c(1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
    data_strategy = c("unmanipulated", "unmanipulated", "unmanipulated", "unmanipulated", "unmanipulated", "unmanipulated", "assignment"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes)

g <- base_dag_plot + ggdd_df

ggsave(here::here("maintained", "output", "figure_17.1.pdf"), g, width = 7, height = 5.5)
ggsave(here::here("maintained", "output", "figure_17.1.svg"), g, width = 7, height = 5.5)
