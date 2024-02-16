library(shiny)

# Define the SEIR model
seir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I + theta1 * E1
    dE1 <- beta * S * I - beta2 * E1 * I - theta1 * E1 + theta2 * E2
    dE2 <- beta2 * E1 * I - beta3 * E2 * I - theta2 * E2 + theta3 * E3
    dE3 <- beta3 * E2 * I - lambda * E3 * I - eta * E3 - theta3 * E3
    dI <- lambda * E3 * I
    dNS <- eta * E3
    return(list(c(dS, dE1, dE2, dE3, dI, dNS)))
  })
}

# ui <- fluidPage(
#   titlePanel("SEIR Model Simulator"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("S", "Initial Susceptible (S)", min = 0, max = 10000, value = 999),
#       helpText("Number of individuals not yet infected with the disease"),
#       
#       sliderInput("I", "Initial Infected (I)", min = 0, max = 10000, value = 1),
#       helpText("Number of individuals who are infected with the disease"),
#       
#       sliderInput("beta", "Beta (Transmission rate: S to I)", min = 0, max = 1, value = 0.001, step = 0.00001),
#       helpText("Rate at which an infected individual transmits the disease to a susceptible individual"),
#       
#       sliderInput("beta2", "beta2 (Progression rate: E1 to E2)", min = 0, max = 1, value = 0.001, step = 0.00001),
#       helpText("Rate at which individuals move from the first exposed stage (E1) to the second exposed stage (E2)"),
#       
#       sliderInput("beta3", "beta3 (Progression rate: E2 to E3)", min = 0, max = 1, value = 0.001, step = 0.00001),
#       helpText("Rate at which individuals move from the second exposed stage (E2) to the third exposed stage (E3)"),
#       
#       sliderInput("lambda", "Lambda (Infection rate: E3 to I)", min = 0, max = 1, value = 0.001, step = 0.00001),
#       helpText("Rate at which individuals in the third exposed stage (E3) become infectious"),
#       
#       sliderInput("eta", "Eta (Sexually inactive rate after E3)", min = 0, max = 1, value = 0.005, step = 0.00001),
#       helpText("Sexually inactive rate for individuals in the third exposed stage (E3)"),
#       
#       sliderInput("theta1", "Theta1 (Regression rate: E1 to S)", min = 0, max = 1, value = 0.9),
#       helpText("Rate at which individuals move from the first exposed stage (E1) back to the susceptible stage (S)"),
#       
#       sliderInput("theta2", "Theta2 (Regression rate: E2 to E1)", min = 0, max = 1, value = 0.8),
#       helpText("Rate at which individuals move from the second exposed stage (E2) back to the first exposed stage (E1)"),
#       
#       sliderInput("theta3", "Theta3 (Regression rate: E3 to E2)", min = 0, max = 1, value = 0.001, step = 0.00001),
#       helpText("Rate at which individuals move from the third exposed stage (E3) back to the second exposed stage (E2)"),
#       
#       sliderInput("maxTime", "Maximum Time", min = 1, max = 1000, value = 700),
#       helpText("Maximum time for the simulation")
#     ),
#     
#     mainPanel(
#       plotOutput("seirPlot")
#     )
#   )
# )

ui <- fluidPage(
  titlePanel("SEIR Model Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      div(style = "display: flex; flex-wrap: wrap;",  # Two-column layout
          # Column 1
          div(style = "flex: 50%;",
              sliderInput("S", "Initial Susceptible (S)", min = 0, max = 10000, value = 999),
              helpText("Number of individuals not yet infected with the disease"),
              
              sliderInput("I", "Initial Infected (I)", min = 0, max = 10000, value = 1),
              helpText("Number of individuals who are infected with the disease"),
              
              sliderInput("beta", "Beta (Transmission rate: S to I)", min = 0, max = 1, value = 0.001, step = 0.00001),
              helpText("Rate at which an infected individual transmits the disease to a susceptible individual"),
              
              sliderInput("beta2", "beta2 (Progression rate: E1 to E2)", min = 0, max = 1, value = 0.001, step = 0.00001),
              helpText("Rate at which individuals move from the first exposed stage (E1) to the second exposed stage (E2)"),
              
              sliderInput("theta2", "Theta2 (Regression rate: E2 to E1)", min = 0, max = 1, value = 0.008, step = 0.00001),
              helpText("Rate at which individuals move from the second exposed stage (E2) back to the first exposed stage (E1)"),
              
          ),
          # Column 2
          div(style = "flex: 50%;",
              sliderInput("beta3", "beta3 (Progression rate: E2 to E3)", min = 0, max = 1, value = 0.001, step = 0.00001),
              helpText("Rate at which individuals move from the second exposed stage (E2) to the third exposed stage (E3)"),
              
              sliderInput("lambda", "Lambda (Infection rate: E3 to I)", min = 0, max = 1, value = 0.001, step = 0.00001),
              helpText("Rate at which individuals in the third exposed stage (E3) become infectious"),
              
              sliderInput("eta", "Eta (Mortality rate: E3)", min = 0, max = 1, value = 0.005, step = 0.00001),
              helpText("Mortality rate for individuals in the third exposed stage (E3)"),
              
              sliderInput("theta1", "Theta1 (Regression rate: E1 to S)", min = 0, max = 1, value = 0.009, step = 0.00001),
              helpText("Rate at which individuals move from the first exposed stage (E1) back to the susceptible stage (S)"),
              sliderInput("theta3", "Theta3 (Regression rate: E3 to E2)", min = 0, max = 1, value = 0.001, step = 0.00001),
              helpText("Rate at which individuals move from the third exposed stage (E3) back to the second exposed stage (E2)"),
              
          )
      ),
      sliderInput("maxTime", "Maximum Time", min = 1, max = 10000, value = 700),
      helpText("Maximum time for the simulation")
    ),
    mainPanel(
      plotOutput("seirPlot", height = "600px")
    )
  )
)



server <- function(input, output) {
  output$seirPlot <- renderPlot({
    initial_state <- c(S = input$S, E1 = 0, E2 = 0, E3 = 0, I = input$I, dNS = 0)
    parameters <- c(beta = input$beta, beta2 = input$beta2, beta3 = input$beta3, 
                    lambda = input$lambda, eta = input$eta, theta1 = input$theta1,
                    theta2 = input$theta2, theta3 = input$theta3)
    times <- seq(0, input$maxTime, by = 1)
    
    output <- ode(y = initial_state, times = times, func = seir_model, parms = parameters)
    output_df <- as.data.frame(output)
    output_melted <- melt(output_df, id.vars = "time")
    
    ggplot(output_melted, aes(x = time, y = value, color = variable)) +
      geom_line() +
      labs(x = "Time", y = "Number of Individuals", color = "Compartment") +
      ggtitle("SEIR Model") +
      theme_minimal() +
      theme(
        legend.text = element_text(size = 12),  # Increase legend font size
        axis.text = element_text(size = 12),    # Increase axis text size
        axis.title = element_text(size = 14)    # Increase axis title size
      )
  })
}

shinyApp(ui = ui, server = server)