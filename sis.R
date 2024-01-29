# Define the SIS model
sis_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Rate of moving from S to I due to contact with infected individuals
    seasonal_beta = beta * (1 + amplitude * sin(2 * pi * time / period))
    
    dS <- -seasonal_beta * S * I + gamma * I
    # Rate of moving from I back to S after recovery
    dI <- seasonal_beta * S * I - gamma * I
    
    return(list(c(dS, dI)))
  })
}

# sliderInput("amplitude", "Amplitude of Seasonal Variation", min = 0, max = 1, value = 0.1),
# sliderInput("period", "Period of Seasonal Variation (days)", min = 1, max = 365, value = 365)

# Example initial state and parameters for the SIS model
initial_state <- c(S = 999, I = 1)
parameters <- c(beta = 0.0002, gamma = 0.1, amplitude = 0.1, period = 100)  # Adjust beta and gamma accordingly
times <- seq(0, 500, by = 1)

# Solve the model
output <- ode(y = initial_state, times = times, func = sis_model, parms = parameters)

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
