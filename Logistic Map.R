

# Logistic Map ------------------------------------------------------------

fx <- function(r, x){r*x*(1-x)}

r <- 2.5
init <- .5
tbl <- c(init)

for (i in 2:1000){
  prev <- tbl[i-1]; 
  tbl[i] <- fx(r,prev)
}

plot(seq(1,length(tbl)), tbl)
lines(seq(1,length(tbl)),tbl)


# Logistic Bifurcation Diagram -----------------------------------------------------

fx <- function(r, x) {
  r * x * (1 - x)
}

# Range of r values
r_values <- seq(2.5, 4, by = 0.01)

# Initialize variables to store the results
r_plot <- c()
x_plot <- c()

# Iterate over each value of r
for (r in r_values) {
  # Initial value of x
  x <- 0.5
  
  # Iterate the logistic map to reach steady state
  for (i in 1:1000) {
    x <- fx(r, x)
  }
  
  # Collect additional data points after reaching steady state
  for (i in 1:100) {
    x <- fx(r, x)
    r_plot <- c(r_plot, r)
    x_plot <- c(x_plot, x)
  }
}

# Plot the bifurcation diagram
plot(r_plot, x_plot, pch = ".", xlab = "r", ylab = "x", main = "Logistic Map Bifurcation Diagram")

