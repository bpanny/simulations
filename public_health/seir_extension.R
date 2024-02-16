# Load necessary libraries
library(deSolve)
library(ggplot2)
library(reshape2)

# Define the SEIR model
seir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I
    dE1 <- beta * S * I - sigma * E1
    dE2 <- sigma * E1 - gamma * E2
    dI <- gamma * E2 - lambda * I
    dR <- lambda * I
    return(list(c(dS, dE1, dE2, dI, dR)))
  })
}

par(las=F)

# Initial state and parameters
initial_state <- c(S = 999, E1 = 1, E2 = 0, I = 0, R = 0)
parameters <- c(beta = 0.01, sigma = 0.1, gamma = 0.01, lambda = 0.1)
times <- seq(0, 200, by = 1)

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
