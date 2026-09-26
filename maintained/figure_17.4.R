# blair_coppock_humphreys_2023 — figure_17.4.R
# Output: figure_17.4.pdf
# Output: figure_17.4.svg
# Depends on: helpers.R
# Description: Maintained rewrite of figure_17.4.R

source(here::here("maintained", "helpers.R"))


returned <- function(x1, a_2 = 1/3) 
  ((2*a_2*x1 - (1-a_2)*(1-x1))/(2*x1))*(x1  > (1-a_2)/(1+a_2))

invested <- function(a_1, a_2){
  u_a = (1-a_1) * log(1 - a_1) + a_1 * log(2*a_1)   # give a1
  u_b = (1-a_1) * log(2*a_2) + a_1 * log(2*(1-a_2)) # give 1
  ifelse(u_a > u_b, a_1, 1)}

u1 <- function(x, a_1 = 1/3, a_2 = 1/3) 
  (1-a_1)*log(1-x + returned(x, a_2)*x*2) + 
  a_1*log((1-returned(x, a_2))*x*2)

df <-
  expand_grid(
    x = seq(.05, .95, .05),
    a_1 = seq(0, .6, .2),
    a_2 = seq(0, .6, .2)
  ) |> 
  mutate(u1 = u1(x = x, a_1 = a_1, a_2 = a_2))

g <- 
  ggplot(df) + 
  aes(x, u1) + 
  geom_line() + 
  facet_grid(a_1 ~ a_2, labeller = label_both) + 
  theme_dd() + 
  labs(x = "Amount given", y = "Offerer's utility") 

g

ggsave(here::here("maintained", "output", "figure_17.4.pdf"), g, width = 6.5, height = 7)
ggsave(here::here("maintained", "output", "figure_17.4.svg"), g, width = 6.5, height = 7)



