library(deSolve)

# Parameters
m <- 1.0   # mass
b <- 0.1   # damping coefficient
k <- 1.0   # spring constant

# Differential equation
oscillator <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # The system of equations
    dxdt <- v
    dvdt <- -(b/m) * v - (k/m) * x
    list(c(dxdt, dvdt))
  })
}

# Initial state
state <- c(x = 1, v = 0)  # Initial displacement and velocity

# Time sequence
times <- seq(0, 100, by = .01)

# Solve the differential equation
parameters <- c(b = b, k = k, m = m)
out <- ode(y = state, times = times, func = oscillator, parms = parameters)

# Plot
plot(out, xlab = "Time", ylab = c("Displacement", 'Velocity'), main = "Damped Harmonic Oscillator")

# Create a data frame for ggplot
data <- data.frame(Time = out[,'time'], Displacement = out[, "x"], Velocity = out[, "v"])

# Create the plot using ggplot2 for both displacement and velocity
data %>% 
  pivot_longer(cols = c('Displacement', 'Velocity'),
               names_to = 'Variable',
               values_to = 'Value') %>% 
  ggplot(aes(x = Time, y = Value, color = Variable)) +
  geom_line() +
  labs(x = "Time", y = "Value", title = "Damped Harmonic Oscillator") +
  theme_minimal()

data %>% 
  ggplot(aes(x = Displacement, y = Velocity)) +
  geom_path() +
  labs(x = "Displacement", y = "Velocity", title = "Damped Harmonic Oscillator") +
  theme_minimal()
