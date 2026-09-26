# blair_coppock_humphreys_2023 — figure_8.6.R
# Output: figure_8.6.pdf
# Output: figure_8.6.svg
# Depends on: helpers.R
# Description: Maintained rewrite of figure_8.6.R

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
    assignment = block_ra(blocks = households, conditions = c("P1", "P2", "P3")),
    assignment1 = assignment == "P1",
    assignment2 = assignment %in% c("P1", "P2"),
    assignment3 = 1
  )) |>
  select(-assignment) |>
  pivot_longer(cols = contains("assignment"), names_to = "procedure", values_to = "assignment") |>
  mutate(procedure = factor(procedure, levels = paste0("assignment", 1:3), labels = c("Period 1", "Period 2", "Period 3")),
         assignment = as.factor(assignment))

g <-
  ggplot(gg_df) +
  aes(X, Y) +
  geom_tile(aes(fill = assignment), color = NA, width = 0.46, height = 0.46) +
  coord_fixed() +
  facet_grid(~ procedure, switch = "y") +
  scale_fill_manual(values = c(dd_palette("dd_light_gray"), dd_palette("dd_light_blue"))) +
  theme_dd() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

g

ggsave(here::here("maintained", "output", "figure_8.6.pdf"), g, width = 6.5, height = 2.5)
ggsave(here::here("maintained", "output", "figure_8.6.svg"), g, width = 6.5, height = 2.5)
