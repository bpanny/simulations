library(shiny)
library(deSolve)
library(reshape2)

# Define the SEIR model
seir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I + theta1 * E1
    dE1 <- beta * S * I - beta2 * E1 * I - theta1 * E1 + theta2 * E2
    dE2 <- beta2 * E1 * I - lambda * E2 - theta2 * E2 - eta * E2
    dI <- lambda * E2
    dNS <- eta * E2
    return(list(c(dS, dE1, dE2, dI, dNS)))
  })
}

# ui <- fluidPage(
#   titlePanel("SEIR Model Simulator"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("S", "Initial Susceptible (S)", min = 0, max = 10000, value = 999, step = 1),
#       sliderInput("I", "Initial Infected (I)", min = 0, max = 10000, value = 1, step = 1),
#       sliderInput("beta", "Beta 1 (Contact Rate)", min = 0, max = 1, value = 0.01, step = .001),
#       sliderInput("beta2", "Beta 2 (Contact Rate)", min = 0, max = 1, value = 0.01, step = .001),
#       sliderInput("lambda", "Lambda (Choose Active)", min = 0, max = 1, value = 0.01, step = .001),
#       sliderInput("eta", "Eta (Choose Inactive)", min = 0, max = 1, value = 0.005, step = .001),
#       sliderInput("theta1", "Theta 1 (E1 Revert to S)", min = 0, max = 1, value = 0.9, step = .001),
#       sliderInput("theta2", "Theta 2 (E2 Revert to S)", min = 0, max = 1, value = 0.8, step = .001),
#     ),
#     
#     mainPanel(
#       plotOutput("seirPlot")
#     )
#   )
# )


server <- function(input, output) {
  output$seirPlot <- renderPlot({
    initial_state <- c(S = input$S, E1 = 0, E2 = 0, I = input$I, dNS = 0)
    parameters <- c(beta = input$beta, beta2 = input$beta2, gamma = input$gamma, 
                    lambda = input$lambda, eta = input$eta, theta1 = input$theta1,
                    theta2 = input$theta2)
    times <- seq(0, 700, by = 1)
    
    output <- ode(y = initial_state, times = times, func = seir_model, parms = parameters)
    output_df <- as.data.frame(output)
    output_melted <- melt(output_df, id.vars = "time")
    
    ggplot(output_melted, aes(x = time, y = value, color = variable)) +
      geom_line() +
      labs(x = "Time", y = "Number of Individuals", color = "Compartment") +
      ggtitle("SEIR Model") +
      theme_minimal()
  })
}

# shinyApp(ui = ui, server = server)