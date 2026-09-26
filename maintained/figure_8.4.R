# blair_coppock_humphreys_2023 — figure_8.4.R
# Output: figure_8.4.pdf
# Output: figure_8.4.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_8.4.R

source(here::here("maintained", "helpers.R"))

set.seed(343)

gg_df <- fabricate(
  villages = add_level(N = 4, village_num = 1:4 + 1:4 * 0.1),
  households = add_level(N = 4,
                         household_num = 1:4 + 1:4 * 0.1),
  individuals = add_level(
    N = 4,
    X = village_num + c(0.25,-0.25, 0.25,-0.25),
    Y = household_num + c(0.25, 0.25,-0.25,-0.25),
    a = simple_ra(N),
    b = complete_ra(N),
    c = block_ra(blocks = villages),
    d = cluster_ra(clusters = households, simple = TRUE),
    e = cluster_ra(clusters = households),
    f = block_and_cluster_ra(clusters = households, blocks = villages),
    g = d * simple_ra(N),
    h = e * block_ra(blocks = households),
    i = f * block_ra(blocks = paste0(households, Y))
  )) |>
  pivot_longer(cols = letters[1:9], names_to = "procedure", values_to = "sampled") |>
  mutate(procedure = factor(
    procedure,
    levels = letters[1:9],
    labels = c(
      "Individual Random Assignment (simple)",
      "Individual Random Assignment (complete)",
      "Individual Random Assignment (blocked)",
      "Cluster Random Assignment (simple)",
      "Cluster Random Assignment (complete)",
      "Cluster Random Assignment (blocked)",
      "Saturation Design (simple)",
      "Saturation Design (complete)",
      "Saturation Design (blocked)"
    )
  ),
  unit = case_when(
    str_detect(procedure, "Individual") ~ "Individual",
    str_detect(procedure, "Cluster") ~ "Cluster",
    str_detect(procedure, "Saturation") ~ "Saturation"
  ),
  sampling_type = case_when(
    str_detect(procedure, "simple") ~ "Simple",
    str_detect(procedure, "complete") ~ "Complete",
    str_detect(procedure, "blocked") ~ "Blocked"
  ),
  unit = factor(unit, levels = c("Individual", "Cluster", "Saturation")),
  sampling_type = factor(sampling_type, levels = c("Simple", "Complete", "Blocked"))
  )

gg_df <-
  gg_df |>
  group_by(households, unit, sampling_type) |>
  mutate(treated_cluster = any(sampled == 1),
         condition = case_when(
           sampled == 1 & unit == "Individual" ~ "T",
           sampled == 0 & unit == "Individual" ~ "C",
           sampled == 1 & unit == "Cluster" ~ "T",
           sampled == 0 & unit == "Cluster" ~ "C",
           sampled == 1 & unit == "Saturation" ~ "T",
           sampled == 0 & unit == "Saturation" & treated_cluster  ~ "Sp",
           sampled == 0 & unit == "Saturation" & !treated_cluster  ~ "C"
         ))

cluster_df <- gg_df |>
  group_by(households, unit, sampling_type) |>
  summarize(hh_sampled = any(sampled == 1), X = mean(X), Y = mean(Y)) |>
  filter(hh_sampled == TRUE, unit != "Individual")

g <- 
ggplot(gg_df, aes(X, Y)) +
  geom_tile(aes(fill = condition), color = NA, width = 0.46, height = 0.46) +
  geom_tile(data = cluster_df, fill = NA, color = gray(0.6), size = 0.25, width = 1.03, height = 1.03) +
  geom_text(aes(label = condition), size = 2, color = "white") +
  coord_fixed() +
  facet_grid(unit  ~ sampling_type, switch = "y") +
  scale_fill_manual(values = dd_palette("quilt_three_color_palette")) +
  scale_x_continuous(name = "Block (e.g., locality)", breaks = 1:4 + 1:4 * 0.1, labels = LETTERS[1:4]) +
  theme_dd() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

ggsave(here::here("maintained", "output", "figure_8.4.pdf"), g, width = 6.5, height = 7)
ggsave(here::here("maintained", "output", "figure_8.4.svg"), g, width = 6.5, height = 7)
