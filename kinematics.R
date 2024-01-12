# Install and load necessary packages
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")

# Load necessary libraries
library(ggplot2)
library(dplyr)


# Declare Functions -------------------------------------------------------

# Kinematics Equations
velocity <- function(v0, a, t) {
  return(v0 + a * t)
}

velocity_by_time <- function(a) {
  return(a)
}

position <- function(x0, v0, t, a) {
  return(x0 + v0 * t + 0.5 * a * t^2)
}

position_by_acceleration <- function(t) {
  return(0.5 * t^2)
}

# Velocity Components
velocity_x <- function(v, angle) {
  return(v * cos(angle))
}

velocity_y <- function(v, angle) {
  return(v * sin(angle))
}

# Projectile Motion Equation
projectile_motion <- function(v0, angle_degrees) {
  g = 9.81  # Acceleration due to gravity in m/s^2
  angle_radians <- angle_degrees * (pi / 180)
  return((v0^2 * sin(2 * angle_radians)) / g)
}

projectile_motion_by_v0 <- function(v0, angle_degrees = 45) {
  g = 9.81  # Acceleration due to gravity in m/s^2
  angle_radians <- angle_degrees * (pi / 180)
  return((v0 * 2 * sin(2 * angle_radians)) / g)
}

projectile_motion_by_angle <- function(angle_degrees, v0 = 3) {
  g = 9.81  # Acceleration due to gravity in m/s^2
  angle_radians <- angle_degrees * (pi / 180)  # Convert angle to radians
  return((2 * v0^2 * cos(2 * angle_radians)) / g)
}


# Forces Equation
force <- function(m, a) {
  return(m * a)  # F = ma
}

elastic_force <- function(mu_k, normal) {
  return(mu_k * normal)
}

# Friction Equations
# The angle must be in radians for these functions to work correctly.
# In R, you can convert degrees to radians using: radians <- degrees * (pi / 180)


friction_force_static <- function(mu_s, normal) {
  return(mu_s * normal)
}

friction_force_kinetic <- function(mu_k, normal) {
  return(mu_k * normal)
}

# Plot Velocity w/r/t Acceleration ----------------------------------------------------------

# Define the v and a ranges for the grid
v_range <- seq(-2, 2, by=0.2)
a_range <- seq(-2, 2, by=0.2)

# Create a data frame of grid points
grid_data <- expand.grid(v = v_range, a = a_range)

# Calculate the slope (dv/da) at each point
grid_data <- grid_data %>%
  mutate(da = 0.1, dv = 0.1 * a)

# Create the Vector field plot
ggplot(grid_data, aes(x = a, y = v)) +
  geom_segment(aes(xend = a + da, yend = v + dv), 
               arrow = arrow(length = unit(0.2, "cm"))) +
  theme_minimal() +
  labs(title = "Vector Field for v' = a", x = "a", y = "v")

#
position <- function(x0, v0, t, a) {
  return(x0 + v0 * t + 0.5 * a * t^2)
}


# Position w/r/t Time -------------------------------------------------------


# Load the required libraries
library(ggplot2)
library(dplyr)

create_vector_field_plot <- function(x_range, y_range, input_function) {
  
  # Create a data frame of grid points
  grid_data <- expand.grid(x = x_range, y = y_range)
  
  # Calculate the slope (dv/da) at each point using the input function
  grid_data <- grid_data %>%
    mutate(dx = 0.1, dy = 0.1 * input_function(x))
  
  # Create the Vector field plot
  plot <- ggplot(grid_data, aes(x = x, y = y)) +
    geom_segment(aes(xend = x + dx, yend = y + dy), 
                 arrow = arrow(length = unit(0.2, "cm"))) +
    theme_minimal() +
    labs(title = "Vector Field")
  
  # Return or print the plot
  return(plot)
}

# Example usage:
# Suppose the input function is simply the identity function of 'a'.
# Then you would call the function like this:
# create_vector_field_plot(function(a) a)

create_vector_field_plot(seq(-2, 2, by=0.2), 
                         seq(-2, 2, by=0.2), 
                         velocity_by_time) + 
  labs(x = "Acceleration (m/(s^2))", y = "Velocity (m/s)")

create_vector_field_plot(seq(0, 5, by=0.2), 
                         seq(-2, 2, by=0.2),
                         position_by_acceleration) + 
  labs(x = "Time (t)", y = "Position (m)")

create_vector_field_plot(seq(0, 10, by=0.2), 
                         seq(-2, 2, by=0.2),
                         projectile_motion_by_v0) + 
  labs(x = "Initial Velocity (m/s)", y = "Range (m)")

# more of a slope field
create_vector_field_plot(seq(-360,360, by=20), 
                         seq(-2, 2, by=.2),
                         projectile_motion_by_angle) + 
  labs(x = "Projectile Angle (Degrees)", y = "Range (m)")

# Define the system of differential equations
projectile_equations <- function(t, state, params) {
  v0 <- params$v0
  theta <- params$theta
  g <- 9.81
  
  # State variables
  x <- state[1]  # Horizontal position
  y <- state[2]  # Vertical position
  
  # Derivatives
  dx_dt <- v0 * cos(theta)
  dy_dt <- v0 * sin(theta) - g * t
  
  return(list(c(dx_dt, dy_dt)))
}

# Parameters
params <- list(v0 = 30, theta = pi/4)  # Example: 30 m/s at 45 degrees

# Initial conditions: starting at the origin
initial_state <- c(x = 0, y = 0)

# Time points
time_points <- seq(0, 5, by = 0.1)  # Example: from 0 to 5 seconds

# Solve the differential equations
library(deSolve)
solution <- ode(y = initial_state, times = time_points, func = projectile_equations, parms = params)

# The solution contains the x and y positions at each time point

# Make sure you have the ggplot2 package installed and loaded
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Convert the solution to a data frame
solution_df <- as.data.frame(solution)
colnames(solution_df) <- c("time", "x", "y")

# Plotting the trajectory
ggplot(solution_df, aes(x = x, y = y)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Projectile Motion Trajectory",
       x = "Horizontal Distance (m)",
       y = "Vertical Distance (m)",
       caption = "Initial velocity: 30 m/s, Launch angle: 45 degrees") +
  theme(panel.grid.major = element_line(color = "grey80"))

# This will generate a plot of the projectile motion trajectory.

library(viridis)

ma <- tibble(m = seq(-100, 100, 5), g = m) %>% 
  expand.grid() %>% 
  mutate(f = force(m, g))

ggplot(ma, aes(x = m, y = g, fill = f, z = f)) +
  geom_tile() + 
  geom_contour() +
  scale_fill_viridis() +
  labs(title = "Force Plot",
       x = "Mass (m)",
       y = "Gravity (g)",
       fill = "Force (f)") +
  theme_minimal() +
  theme(legend.position = "right")


