library(ggplot2)
library(dplyr)

# Parameters
n_steps <- 2000    # Number of steps in the simulation
n_particles <- 100  # Number of particles

# Initialize the particles
particles <- data.frame(
  id = rep(1:n_particles, each = n_steps),
  x = rep(0, n_particles * n_steps),
  y = rep(0, n_particles * n_steps)
)

particle_starts <- seq(0, n_steps * n_particles - 1, by = n_steps)
# Simulate the random walk
set.seed(123)  # For reproducibility
for(i in 2:n_steps) {
  particles$x[i + particle_starts] <- particles$x[i - 1 + particle_starts] + sample(c(-1, 0, 1), size = n_particles, replace = T)
  particles$y[i + particle_starts] <- particles$y[i - 1 + particle_starts] + c(sample(c(-1, 0, 1), 
                                               size = ifelse(
                                                 n_particles %% 2 == 0, 
                                                 n_particles / 2, 
                                                 n_particles / 2 + .5), 
                                               replace = T),
                                        sample(c(-1, 0, 1), 
                                               size = ifelse(
                                                 n_particles %% 2 == 0, 
                                                 n_particles / 2, 
                                                 n_particles / 2 - .5), 
                                               replace = T))
                                        
  }

# Visualization with ggplot2
ggplot(particles, aes(x = x, y = y, color = factor(id))) +
  geom_path(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Simulation of 2D particle random walks",
       x = "X Coordinate",
       y = "Y Coordinate")

particles <- data.frame(
  id = rep(1:n_particles, each = n_steps),
  x = rep(0, n_particles * n_steps),
  y = rep(0, n_particles * n_steps)
)

# Parameters
n_steps <- 2000    # Number of steps in the simulation
n_particles <- 20  # Number of particles

# Initialize the particles
particles <- data.frame(
  id = rep(1:n_particles, each = n_steps),
  x = rep(0, n_particles * n_steps),
  y = rep(0, n_particles * n_steps),
  z = rep(0, n_particles * n_steps)  # Adding z-coordinate
)

particle_starts <- seq(0, n_steps * n_particles - 1, by = n_steps)

# Simulate the random walk
set.seed(123)  # For reproducibility
for(i in 2:n_steps) {
  particles$x[i + particle_starts] <- particles$x[i - 1 + particle_starts] + sample(c(-1, 0, 1), size = n_particles, replace = TRUE)
  particles$y[i + particle_starts] <- particles$y[i - 1 + particle_starts] + sample(c(-1, 0, 1), size = n_particles, replace = TRUE)
  particles$z[i + particle_starts] <- particles$z[i - 1 + particle_starts] + sample(c(-1, 0, 1), size = n_particles, replace = TRUE)
}

# Visualization with plotly
library(plotly)

plot_ly(data = particles, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines', color = ~factor(id), line = list(width = 2)) %>%
  layout(title = "Simulation of 3D Particle Random Walks",
         scene = list(xaxis = list(title = 'X Coordinate'),
                      yaxis = list(title = 'Y Coordinate'),
                      zaxis = list(title = 'Z Coordinate')))
