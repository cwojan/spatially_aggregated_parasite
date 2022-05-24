####
## project: parasite_burden_modeling
## file: pb_detachment_prob_viz.R
## function: visualize the probability of detachment over time under different models
## author: chris wojan
####

## Load libraries
library(ggplot2)

## detachment rate probability

x <- 10:100 / 10
y <- 1 - 0.8^x

data <- data.frame(x = x, y = y)

ggplot(data = data) +
  geom_line(aes(x = x, y = y)) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Timestep", y = "Cumulative Probability Individual Tick Detachment") +
  theme_bw()


## attachment length probability

l1 <- pnorm(x, mean = 5, sd = 0.1)
l2 <- pnorm(x, mean = 5, sd = 1)
l3 <- pnorm(x, mean = 5, sd = 5)

l <- unlist(lapply(X = c(0.1, 1, 5), FUN = function(x) pnorm(10:100/10, mean = 5, sd = x)))





data2 <- data.frame(x = rep(x, 3), l = l, sd = rep(c(0.1,1,5), each = 91))

ggplot(data = data2[data2$sd == 1,]) +
  geom_line(aes(x = x, y = l)) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Timestep", y = "Cumulative Probability Individual Tick Detachment") +
  theme_bw()

ggplot(data = data2) +
  geom_line(aes(x = x, y = l)) +
  facet_wrap(vars(sd)) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Timestep", y = "Cumulative Probability Individual Tick Detachment") +
  theme_bw()

