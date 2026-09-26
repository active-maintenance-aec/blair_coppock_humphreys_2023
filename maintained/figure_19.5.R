# blair_coppock_humphreys_2023 — figure_19.5.R
# Output: figure_19.5.pdf
# Output: figure_19.5.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_19.5.R

source(here::here("maintained", "helpers.R"))



diagnosis_19.3 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_19.3.rds"))

simulations <- 
  diagnosis_19.3 |> 
  get_simulations() |> 
  mutate(
    inquiry = factor(inquiry, levels = c("mu", "tau_sq"), labels = c("mu", "tau^2")),
    estimator = factor(estimator, levels = c("fixed-effects", "random-effects"), 
                       labels = c("Fixed~effects", "Random~effects"))
  )

gg_df <- 
  simulations |> 
  filter(sim_ID < 100) |> 
  mutate(covers = conf.low <= estimand & estimand <= conf.high)

inquiries_df <- 
  simulations |> 
  group_by(inquiry, estimator, tau) |> 
  summarize(estimand = mean(estimand), .groups = "drop")

g0 <- 
ggplot(filter(gg_df, tau == 0)) +
  aes(estimate, sim_ID, color = covers) +
  geom_point(alpha = 0.3, stroke = 0) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), alpha = 0.1) +
  geom_vline(data = filter(inquiries_df, tau == 0), aes(xintercept = estimand)) +
  scale_color_manual(values = dd_palette("two_color_palette")) +
  theme_dd() +
  theme(axis.title.y = element_blank(), text = element_text(size = 8)) +
  facet_grid(estimator ~ inquiry, labeller = label_parsed) +
  labs(x = "Estimate and 95% confidence interval",
       title = expression ("True effects do not vary across sites ("~tau^2~" = 0)"))

g1 <- ggplot(filter(gg_df, tau == 1)) +
  aes(estimate, sim_ID, color = covers) +
  geom_point(alpha = 0.3, stroke = 0) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), alpha = 0.1) +
  geom_vline(data = filter(inquiries_df, tau == 1), aes(xintercept = estimand)) +
  scale_color_manual(values = dd_palette("two_color_palette")) +
  theme_dd() +
  theme(axis.title.y = element_blank(), text = element_text(size = 8)) +
  facet_grid(estimator ~ inquiry, labeller = label_parsed) +
  labs(x = "Estimate and 95% confidence interval",
       title = expression ("True effects do vary across sites ("~tau^2~" = 1)"),
       legend.position = "bottom")


g <- g0 + g1

g

ggsave(here::here("maintained", "output", "figure_19.5.pdf"),
       g,
       width = 6.5,
       height = 4)
ggsave(here::here("maintained", "output", "figure_19.5.svg"),
       g,
       width = 6.5,
       height = 4)
