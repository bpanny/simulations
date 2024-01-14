# Load required packages
library(deSolve)
library(ggplot2)

# Define the differential equation
# x'' + (g/L) * sin(x) = 0
# We'll break it into a system of first order ODEs:
# Let x1 = x and x2 = x'
# Then x1' = x2 and x2' = -(g/L) * sin(x1)
library(deSolve)
library(ggplot2)

# Define the differential equation for small angles (linear approximation)
ode_system <- function(t, state, parameters) {
  g <- parameters["g"]
  L <- parameters["L"]
  
  x1 <- state["x1"] # x
  x2 <- state["x2"] # x'
  
  list(c(x2, -g/L * sin(x1))) # x1' = x2, x2' = -g/L * x1
}

# Parameters and initial conditions
params <- c(g = 9.81, L = 1) # example values for g and L

# Convert angle from degrees to radians
initial_angle <- 85 * (pi / 180) # 10 degrees in radians

init_state <- c(x1 = initial_angle, x2 = 0) # initial displacement in radians

# Time points to solve over
times <- seq(0, 10, by = 0.1)

# Solve the ODE
solution <- ode(y = init_state, times = times, func = ode_system, parms = params)

# Convert to data frame for plotting
solution_df <- as.data.frame(solution)
names(solution_df) <- c("time", "x", "x_dot")

# Plotting with ggplot2
ggplot(data = solution_df, aes(x = time, y = x)) +
  geom_line() +
  labs(x = "Time", y = "Position x(t)", title = "Pendulum Position over Time")

ggplot(data = solution_df, aes(x = x, y = x_dot)) +
  geom_path() +
  labs(x = "Angular displacement x(t)", y = "Angular Velocity x_dot(t)", title = "Pendulum Position and Velocity")

