# blair_coppock_humphreys_2023 — figure_2.2.R
# Output: figure_2.2.pdf
# Output: figure_2.2.svg
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_2.2.R

source(here::here("march_2026_rewrite", "helpers.R"))


diagnosis <- read_rds(here::here("original", "diagnosis_objects", "diagnosis_2.1.rds"))

gg_df <- 
tidy(diagnosis) |> 
  mutate(design = if_else(str_detect(design, "twoarm"), "twoarm", "blocked"),
         diagnosand_label = factor(diagnosand, levels = c("success", "failure"),
                                   labels = c("Success: implementing the program\nwhen the true effect is greater than 0.2",
                                              "Failure: implementing the program\nwhen the true effect is less than 0.2"))) 

label_df <- 
  tibble(
    b = c(2.5, 2.5),
    estimate = c(0.1, 0.33),
    design = c("twoarm", "blocked"),
    label = c("Design 1: N = 150\ncomplete random assignment", "Design 2: N = 100\nblock random assignment"),
    diagnosand_label = factor("Success: implementing the program\nwhen the true effect is greater than 0.2")
  )

# I checked success and failure rates. the failure rates are very close for the two designs over the whole range, so focusing on success:

gg_df <- 
  gg_df |> 
  filter(diagnosand == "success")

g <-
  ggplot(gg_df, aes(b, estimate, group = design, color = design)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax =  conf.high, fill = design), color = NA,  size = 0, alpha = 0.2) +
  geom_text(data = label_df, aes(label = label)) +
  scale_fill_manual(values = dd_palette("two_color_palette")) +
  scale_color_manual(values = dd_palette("two_color_palette")) +
  ylim(0, NA) +
  labs(x = "Importance of background factor (subject history)", 
       y = "Diagnosand: Success rate\n(Implementation of a strong program)") +
  theme_dd() 

g

ggsave(here::here("march_2026_rewrite", "output", "figure_2.2.pdf"), g, width = 6.5, height = 3.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_2.2.svg"), g, width = 6.5, height = 3.5)
