# Load necessary libraries
library(deSolve)
library(ggplot2)
library(reshape2)

# Define the SEIR model
seir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I + theta1 * E1 + theta4 * I
    dE1 <- beta * S * I - sigma * E1 * I - theta1 * E1 + theta2 * E2
    dE2 <- sigma * E1 * I - gamma * E2 * I - theta2 * E2 + theta3 * E3
    dE3 <- gamma * E2 * I - lambda * E3 * I - eta * E3 - theta3 * E3
    dI <- lambda * E3 * I - theta4 * I
    dNS <- eta * E3
    return(list(c(dS, dE1, dE2, dE3, dI, dNS)))
  })
}

par(las=F)

# Initial state and parameters
initial_state <- c(S = 999, E1 = 0, E2 = 0, E3 = 0, I = 1, dNS = 0)
parameters <- c(beta = 0.01, sigma = 0.01, gamma = 0.01, 
                lambda = 0.01, theta4 = 0.041798, eta = 0, theta1 = 0,
                theta2 = 0, theta3 = 0)
times <- seq(0, 6000, by = 1)

# Solve the model
output <- ode(y = initial_state, times = times, func = seir_model, parms = parameters)

# Convert output to a data frame
output_df <- as.data.frame(output)

# Reshape for ggplot2
output_melted <- melt(output_df, id.vars = "time")

# Plot using ggplot2
ggplot(output_melted, aes(x = time, y = value, color = variable)) +
  geom_line() +
  labs(x = "Time", y = "Number of Individuals", color = "Compartment") +
  ggtitle("SEIR Model") +
  theme_minimal()
