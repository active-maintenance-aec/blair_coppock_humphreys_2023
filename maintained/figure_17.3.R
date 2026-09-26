# blair_coppock_humphreys_2023 — figure_17.3.R
# Output: figure_17.3.pdf
# Output: figure_17.3.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_17.3.R

source(here::here("maintained", "helpers.R"))



diagnosis_17.4 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_17.4.rds"))

diagnosands <- 
  diagnosis_17.4 |>
  get_simulations() |> 
  mutate(inquiry = factor(inquiry, levels = c("genderWoman", "partyRight", "regionEast", "regionSouth", "regionWest"),
                          labels = c("Gender: Woman", "Party: Right", "Region: East", "Region: South", "Region: West"))) |> 
  group_by(inquiry) |>
  summarize(estimand = mean(estimand),
            bias = mean(estimate - estimand),
            power = mean(p.value <= 0.05),
            entry = paste0("AMCE: ", format_num(estimand), "\nBias: ", format_num(bias), "\nPower: ", format_num(power))) |> 
  mutate(y = c(400, 400, 100, 100, 100),
         x = c(-0.25, -0.25, 0.05, 0.05, 0.05))

simulations <-
  diagnosis_17.4 |>
  get_simulations() |> 
  filter(sim_ID <= 500) %>% 
  mutate(inquiry = factor(inquiry, levels = c("genderWoman", "partyRight", "regionEast", "regionSouth", "regionWest"),
                          labels = c("Gender: Woman", "Party: Right", "Region: East", "Region: South", "Region: West"))) |> 
  mutate(inquiry_sim_ID = fct_reorder(paste0(inquiry, sim_ID), estimate),
         significant = p.value <= 0.05)

g <- 
  ggplot(simulations) +
  aes(x = estimate, y = inquiry_sim_ID) +
  geom_point(alpha = 0.1) + 
  geom_linerange(aes(color = significant, xmin = conf.low, xmax = conf.high), alpha = 0.5) +
  geom_vline(data = diagnosands, aes(xintercept = estimand)) + 
  geom_text(data = diagnosands, 
            aes(x = x, y = y, label = entry), 
            hjust = 0, size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = dd_palette("two_color_palette")) +
  scale_x_continuous(limits = c(-0.3, 0.3)) +
  facet_wrap(~inquiry, scales = "free") +
  theme_dd() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none") +
  labs(x = "AMCE estimate and 95% confidence interval")

ggsave(here::here("maintained", "output", "figure_17.3.pdf"), g, width = 6.5, height = 6.5)
ggsave(here::here("maintained", "output", "figure_17.3.svg"), g, width = 6.5, height = 6.5)



