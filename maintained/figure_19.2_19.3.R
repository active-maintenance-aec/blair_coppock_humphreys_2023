# blair_coppock_humphreys_2023 — figure_19.2_19.3.R
# Output: figure_19.2.pdf
# Output: figure_19.2.svg
# Output: figure_19.3.pdf
# Output: figure_19.3.svg
# Depends on: original/diagnosis_objects/, original/code/declarations/, helpers.R
# Description: Maintained rewrite of figure_19.2_19.3.R

source(here::here("maintained", "helpers.R"))
library(grf)


library(ggrepel)

set.seed(343)

source(here::here("original", "code", "declarations", "declaration_19.1.R"))

diagnosis_19.1 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_19.1.rds"))

rsq_function <-
  function(data, covariate_names, cuts = 20) {
    sapply(covariate_names, function(j) {
      lm_robust(tau ~ cut(data[[j]], cuts), data = data)$adj.r.squared
    })
  }

r_sq_df <- 
  map(1:500, ~draw_data(declaration_19.1)) |> 
  map(~rsq_function(., covariate_names, 20)) |> 
  bind_rows() |> 
  mutate(sim_ID = seq_len(n()))

r_sq_df_long <- 
  r_sq_df |> 
  pivot_longer(c(everything(), -sim_ID)) |> 
  mutate(
    sim_ID = fct_rev(as.factor(sim_ID)),
    name = factor(name, levels = paste0("X.", 1:10))
  )

ranks_df <- 
  r_sq_df |> 
  select(-sim_ID) |> 
  rowwise() |> 
  do(data.frame(t(rank(-unlist(.))))) |> 
  ungroup() |> 
  bind_cols(r_sq_df |> select(sim_ID)) |> 
  pivot_longer(cols = c(everything(), -sim_ID)) |> 
  mutate(
    sim_ID = fct_rev(as.factor(sim_ID)),
    name = factor(name, levels = paste0("X.", 1:10))
  )

sims_df <- 
  diagnosis_19.1 |> 
  get_simulations() |> 
  filter(inquiry != "best_predictor") |> 
  mutate(inquiry = str_to_sentence(str_replace_all(inquiry, "_", " ")),
         inquiry = str_replace(inquiry, "Ate", "ATE"))

summary_df <- 
  diagnosis_19.1 |> 
  get_diagnosands() |> 
  filter(inquiry != "best_predictor") |> 
  mutate(inquiry = str_to_sentence(str_replace_all(inquiry, "_", " ")),
         inquiry = str_replace(inquiry, "Ate", "ATE"))
  


gg_df1 <- 
  r_sq_df_long |> 
  filter(as.numeric(as.character(sim_ID)) < 10) |> 
  rename(rsq = value) |> 
  left_join(ranks_df |> filter(as.numeric(as.character(sim_ID)) < 10) |> rename(rank = value)) |> 
  group_by(name) |> 
  summarize(rsq = mean(rsq),
            rank = mean(rank))

g1 <-
  ggplot(gg_df1) + 
  aes(rsq, rank) + 
  geom_label_repel(aes(label = name)) + 
  labs(x = "Adjusted r-squared from regression predicting treatment effects", y = "Rank of variable in predicting treatment effects") + 
  theme_dd()

g1

g2 <- 
  ggplot(sims_df) + 
  geom_histogram(aes(x = estimate, y = ..count.. / sum(..count..)), fill = dd_palette("dd_light_blue")) + 
  geom_vline(data = summary_df, aes(xintercept = mean_estimand)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 0.08, 0.02)) +
  facet_grid(~inquiry) + 
  labs(x = "Simulated estimates", y = "Percent of simulations") + 
  theme_dd()

g2

ggsave(here::here("maintained", "output", "figure_19.2.pdf"), g1, width = 6.5, height = 4)
ggsave(here::here("maintained", "output", "figure_19.2.svg"), g1, width = 6.5, height = 4)

ggsave(here::here("maintained", "output", "figure_19.3.pdf"), g2, width = 6.5, height = 3.5)
ggsave(here::here("maintained", "output", "figure_19.3.svg"), g2, width = 6.5, height = 3.5)

