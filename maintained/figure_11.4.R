# blair_coppock_humphreys_2023 — figure_11.4.R
# Output: figure_11.4.svg
# Output: figure_11.4.pdf
# Depends on: original/diagnosis_objects/, helpers.R
# Description: Maintained rewrite of figure_11.4.R

source(here::here("march_2026_rewrite", "helpers.R"))


source(here::here("original", "code", "declarations", "declaration_11.4.R"))

dat <- draw_data(declaration_11.4)

estimates_dat <-
  get_estimates(declaration_11.4, dat) |>
  mutate(
    inquiry = as.numeric(str_remove(inquiry, "X_")),
    facet = str_replace(estimator, "A", "Polynomial order: "),
    estimator = as.numeric(str_remove(estimator, "A"))
  )

inquiries_dat <-
  declaration_11.4 |>
  draw_estimands() |>
  mutate(inquiry = as.numeric(str_remove(inquiry, "X_")))

g <- 
  ggplot(data = NULL) +
  geom_point(data = dat, aes(X, Y), stroke = 0, alpha = 0.22) +
  geom_line(data = inquiries_dat,
            aes(inquiry, estimand)) +
  geom_line(data = estimates_dat,
            aes(inquiry, estimate,
                group = estimator), color = dd_palette("dd_dark_blue")) +
  coord_cartesian(xlim = c(0, 3), ylim = c(-1, 2)) +
  facet_wrap( ~ facet) +
  theme_dd() +
  theme(axis.title.y = element_text(size = 8)) +
  labs(x = "Predictor X",
       y = "Inquiry: conditional expectation of outcome Y.\nAnswer strategy: predicted value of Y from linear regression")
g

ggsave(here::here("march_2026_rewrite", "output", "figure_11.4.svg"), g, width = 6.5, height = 3.5)
ggsave(here::here("march_2026_rewrite", "output", "figure_11.4.pdf"), g, width = 6.5, height = 3.5)

