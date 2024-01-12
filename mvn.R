# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(viridis)

library(mnormt)

x     <- seq(-5, 5, 0.1) 
y     <- seq(-5, 5, 0.1)
mu    <- c(0, 0)
sigma <- matrix(c(2, -1, -1, 2), nrow = 2)
f     <- function(x, y) dmnorm(cbind(x, y), mu, sigma)
z     <- outer(x, y, f)

bvn <- tibble(x = x, y = y) %>% 
  expand.grid() %>% 
  mutate(z = dmnorm(cbind(x, y), mu, sigma))

ggplot(bvn, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  scale_fill_viridis() +
  labs(title = "Force Plot",
       x = "x",
       y = "y",
       fill = "Force (f)") +
  theme_minimal() +
  theme(legend.position = "right")

ggplot(bvn, aes(x = x, y = y, z = z)) +
  geom_contour_filled() +
  labs(title = "Force Plot",
       x = "x",
       y = "y",
       fill = "Force (f)") +
  theme_minimal() +
  theme(legend.position = "right")
