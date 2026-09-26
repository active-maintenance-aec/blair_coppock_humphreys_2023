# blair_coppock_humphreys_2023 — figure_18.2.R
# Output: figure_18.2.pdf
# Output: figure_18.2.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_18.2.R

source(here::here("maintained", "helpers.R"))


diagnosis_18.2 <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_18.2.rds"))

gg_df <- 
  diagnosis_18.2 |> 
  tidy() |> 
  filter(diagnosand %in% c("sd_estimate", "power")) |> 
  mutate(diagnosand_label = 
           case_when(diagnosand == "sd_estimate" ~ "True standard error",
                     diagnosand == "power" ~ "Statistical power"))

label_df <-
  gg_df |> 
  filter(N == 1100) |> 
  mutate(label1 = paste0("R^2 ==", DeclareDesign:::format_num(r_sq, 1)),
         label2 = DeclareDesign:::format_num(r_sq, 1))


g <- 
  ggplot(gg_df, aes(N, estimate, group = r_sq, color = r_sq, fill = r_sq)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, color = NULL), alpha = 0.2) +
  geom_text(data = label_df, aes(label = label1), nudge_x = 20, hjust = 0, parse=TRUE) +
  scale_x_continuous(limits = c(0, 1500), breaks = seq(0, 1000, length.out = 6)) +
  theme_dd() +
  theme(strip.background = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~diagnosand_label, scales = "free_y") +
  labs(y = "Diagnosand estimate",
       x = "Sample size")
g

ggsave(here::here("maintained", "output", "figure_18.2.pdf"),
       g,
       width = 6.5,
       height = 3.5)
ggsave(here::here("maintained", "output", "figure_18.2.svg"),
       g,
       width = 6.5,
       height = 3.5)
