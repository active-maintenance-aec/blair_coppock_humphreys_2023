# blair_coppock_humphreys_2023 — figure_15.1.R
# Output: figure_15.1.pdf
# Output: figure_15.1.svg
# Depends on: helpers.R
# Description: Maintained rewrite of figure_15.1.R

source(here::here("maintained", "helpers.R"))
library(ggdag)
library(ggraph)
library(ggforce)
library(ggtext)
source(here::here("maintained", "utilities", "make_dag_df.R"))
dag <- dagify(Y ~ Q + Ystar + S)
nodes <-
  tibble(
    name = c("Y", "S", "Q", "Ystar"),
    label = c("Y", "S", "Q", "Y^*"),
    annotation = c("**Measured outcome**<br>observed only for sampled units",
                   "**Sampling indicator**<br>randomly set by designer",
                   "**Survey Question**",
                   "**Latent outcome**"),
    x = c(5, 1, 5, 1),
    y = c(1.5, 3.5, 3.5, 1.5), 
    nudge_direction = c("S", "N", "N", "S"),
    data_strategy = c("unmanipulated", "sampling", "measurement", "unmanipulated"),
    answer_strategy = "uncontrolled"
  )
ggdd_df <- make_dag_df(dag, nodes)
g <- base_dag_plot + ggdd_df
ggsave(here::here("maintained", "output", "figure_15.1.pdf"), g, width = 7, height = 5)
ggsave(here::here("maintained", "output", "figure_15.1.svg"), g, width = 7, height = 5)
