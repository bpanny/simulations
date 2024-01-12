

# Slope Field -------------------------------------------------------------

# Load the deSolve package
library(deSolve)

# Define the differential equation function
# In this case, y' = x^2
equation <- function(x, y, params) {
  return(list(2*x))
}

# Generate a grid of points where slopes will be calculated
x <- seq(-3, 3, by=0.2)
y <- seq(-3, 3, by=0.2)

# Create an empty plot for the slope field
plot(0, type="n", xlim=c(min(x), max(x)), ylim=c(min(y), max(y)), xlab="x", ylab="y")

# Add slopes to the plot at each grid point
for (i in x) {
  for (j in y) {
    slope <- equation(i, j, NULL)
    lines(c(i, i + 0.1), c(j, j + 0.1 * slope[[1]]), col="blue")
  }
}

# Display the plot
title("Slope Field for y' = x^2")


# Vector Fields -----------------------------------------------------------

# Load required libraries
library(ggplot2)
library(dplyr)

# Define the x and y ranges for the grid
x_range <- seq(-2, 2, by=0.2)
y_range <- seq(-2, 2, by=0.2)

# Create a data frame of grid points
grid_data <- expand.grid(x = x_range, y = y_range)

# Calculate the slope (dy/dx) at each point
grid_data <- grid_data %>%
  mutate(dx = 0.1, dy = 0.1 * 2 * x)

# Create the Vector field plot
ggplot(grid_data, aes(x = x, y = y)) +
  geom_segment(aes(xend = x + dx, yend = y + dy), 
               arrow = arrow(length = unit(0.2, "cm"))) +
  theme_minimal() +
  labs(title = "Vector Field for y' = x^2", x = "x", y = "y")
