# blair_coppock_humphreys_2023 — figure_16.7.R
# Output: figure_16.7.pdf
# Output: figure_16.7.svg
# Depends on: helpers.R
# Description: Maintained rewrite of figure_16.7.R

source(here::here("maintained", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)
library(ggtext)
source(here::here("maintained", "utilities", "make_dag_df.R"))
dag <- dagify(Y ~ D + U,
              D ~ Z + U)
nodes <-
  tibble(
    name = c("Z", "D", "U", "Y"),
    label = c("Z", "D", "U", "Y"),
    annotation = c(
      "**Exogenous variable**<br>Instrument",
      "**Endogenous variable**<br>Treatment",
      "**Unknown heterogeneity**",
      "**Outcome**"
    ),
    x = c(1, 3, 5, 5),
    y = c(1.5, 1.5, 3.5, 1.5), 
    nudge_direction = c("S", "S", "N", "S"),
    data_strategy = "unmanipulated",
    answer_strategy = "uncontrolled"
  )
ggdd_df <- make_dag_df(dag, nodes)
g <- base_dag_plot + ggdd_df
g
ggsave(here::here("maintained", "output", "figure_16.7.pdf"), g, width = 6.5, height = 4)
ggsave(here::here("maintained", "output", "figure_16.7.svg"), g, width = 6.5, height = 4)
