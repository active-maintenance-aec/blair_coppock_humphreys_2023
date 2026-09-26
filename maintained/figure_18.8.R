# blair_coppock_humphreys_2023 — figure_18.8.R
# Output: figure_18.8.pdf
# Output: figure_18.8.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_18.8.R

source(here::here("maintained", "helpers.R"))



diagnosis_18.8 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_18.8.rds"))

gg_df <-
  diagnosis_18.8 |>
  get_simulations() |> 
  filter(inquiry %in% c("CATE_Z1_Z2_0",
                        "CATE_Z1_Z2_1",
                        "diff_in_CATEs_Z1")) |>
  mutate(significant = p.value < 0.05) |>
  pivot_wider(
    id_cols = c(sim_ID, CATE_Z1_Z2_0),
    names_from = inquiry,
    values_from = c("estimate", "conf.high", "conf.low", "significant")
  ) |>
  group_by(CATE_Z1_Z2_0) |>
  summarize(
    `CATE of Z1 is significant in one Z2 condition but not the other` = mean(xor(
      significant_CATE_Z1_Z2_0, significant_CATE_Z1_Z2_1
    )),
    `Difference in CATEs of Z1 is significant` = mean(significant_diff_in_CATEs_Z1)
  ) |>
  gather(condition, power,-CATE_Z1_Z2_0)

g <-
  ggplot(gg_df, aes(CATE_Z1_Z2_0, power, color = condition)) +
  geom_point() +
  geom_line() +
  geom_text(data = (. %>% filter(CATE_Z1_Z2_0 == 0.2)),
            aes(label = condition),
            nudge_y = 0.02) +
  scale_color_manual(values = dd_palette("two_color_palette")) +
  theme_dd() + 
  theme(legend.position = "none") +
  labs(
    x = "True constant effect of Z1",
    y = "Probability of result (akin to statistical power)"
  )

ggsave(here::here("maintained", "output", "figure_18.8.pdf"), g, width = 6.5, height = 4)
ggsave(here::here("maintained", "output", "figure_18.8.svg"), g, width = 6.5, height = 4)

