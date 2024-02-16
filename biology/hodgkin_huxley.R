# Hodgkin-Huxley Model: Understanding dV, dn, dm, dh

# dV - Change in Membrane Potential
# Represents: Rate of change of the neuron's membrane potential over time.
# Influences: Ionic currents (Na+, K+, leakage).
# Mechanism: Na+ influx causes depolarization, then K+ efflux causes repolarization.
# Role in Action Potential: Rapid Na+ channel opening and subsequent K+ channel opening.

# dn - Change in Potassium Channel Activation Variable
# Represents: Rate of change of n, governing potassium channels opening.
# Alpha/Beta Dynamics: Controlled by alpha_n (opening rate) and beta_n (closing rate).
# Action Potential: Slow increase during depolarization, helps in repolarization.

# dm - Change in Sodium Channel Activation Variable
# Represents: Rate of change of m, related to sodium channel activation.
# Alpha/Beta Dynamics: Governed by alpha_m (opening rate) and beta_m (closing rate).
# Action Potential: Rapid increase during initial depolarization phase.

# dh - Change in Sodium Channel Inactivation Variable
# Represents: Rate of change of h, linked to sodium channel inactivation.
# Alpha/Beta Dynamics: Dictated by alpha_h (inactivation rate) and beta_h (de-inactivation rate).
# Action Potential: Decreases during depolarization, preventing further Na+ influx.

# Summary:
# Interplay: V affects n, m, h, which in turn influence the ionic currents and V.
# Action Potential Phases: Rapid V rise due to m, decrease due to n and h.
# Return to Resting State: Variables return to baseline, readying neuron for next action potential.


# Load necessary library
library(deSolve)

# v is membrane potential
# n is probabiility of potassium channel opening


# Hodgkin-Huxley Model Parameters
g_Na = 120   # Sodium (Na) maximum conductances, in mS/cm^2
g_K = 36     # Postassium (K) maximum conductances, in mS/cm^2
g_L = 0.3    # Leak maximum conductances, in mS/cm^2
E_Na = 50    # Sodium (Na) Nernst reversal potentials, in mV
E_K = -77    # Postassium (K) Nernst reversal potentials, in mV
E_L = -54.4  # Leak Nernst reversal potentials, in mV
Cm = 1       # Membrane capacitance, in uF/cm^2

# Define the Hodgkin-Huxley model
hodgkin_huxley_model <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Calculate channel gating variables (n = )
    alpha_n = 0.01 * (V + 55) / (1 - exp(-(V + 55) / 10))
    beta_n = 0.125 * exp(-(V + 65) / 80)
    alpha_m = 0.1 * (V + 40) / (1 - exp(-(V + 40) / 10))
    beta_m = 4 * exp(-(V + 65) / 18)
    alpha_h = 0.07 * exp(-(V + 65) / 20)
    beta_h = 1 / (1 + exp(-(V + 35) / 10))
    
    # Calculate currents
    I_Na = g_Na * m^3 * h * (V - E_Na)
    I_K = g_K * n^4 * (V - E_K)
    I_L = g_L * (V - E_L)
    
    # External current
    I_ext = ifelse(t > 5 & t < 30, 10, 0)
    
    # Differential equations
    dV = (I_ext - I_Na - I_K - I_L) / Cm
    dn = alpha_n * (1 - n) - beta_n * n
    dm = alpha_m * (1 - m) - beta_m * m
    dh = alpha_h * (1 - h) - beta_h * h
    
    return(list(c(dV, dn, dm, dh)))
  })
}

# Initial conditions
state <- c(V = -65, n = 0.3176769, m = 0.05293204, h = 0.5961207)

# Time sequence
times <- seq(0, 50, by = 0.01)

# Model parameters (not changing over time)
parameters <- c(g_Na = g_Na, g_K = g_K, g_L = g_L, E_Na = E_Na, E_K = E_K, E_L = E_L, Cm = Cm)

# Run the simulation
out <- ode(y = state, times = times, func = hodgkin_huxley_model, parms = parameters)

# Convert output to a data frame for ggplot
results_df <- as.data.frame(out)
names(results_df) <- c("Time", "V", "n", "m", "h")

# Load ggplot2 library
library(ggplot2)

# Create the plot
ggplot(data = results_df, aes(x = Time, y = V)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(title = "Hodgkin-Huxley Model Simulation",
       x = "Time (ms)",
       y = "Membrane Potential (mV)")
