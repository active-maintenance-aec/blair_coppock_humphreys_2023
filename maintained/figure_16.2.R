# blair_coppock_humphreys_2023 — figure_16.2.R
# Output: figure_16.2.svg
# Output: figure_16.2.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_16.2.R

source(here::here("march_2026_rewrite", "helpers.R"))



diagnosis_16.1 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_16.1.rds"))

gg_df <- 
  diagnosis_16.1 |> 
  tidy() |> 
  filter(diagnosand == "rmse")

g <- 
  ggplot(gg_df) + 
  aes(term, estimate) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_point() +  
  facet_wrap(~XY) + 
  theme_dd() + 
  labs(x = "Strategy", y = "Root mean squared error")

ggsave(here::here("march_2026_rewrite", "output", "figure_16.2.svg"),
       g,
       width = 6.5,
       height = 2.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_16.2.pdf"),
       g,
       width = 6.5,
       height = 2.5)
