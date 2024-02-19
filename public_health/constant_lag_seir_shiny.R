library(shiny)
library(deSolve)
library(ggplot2)
library(tidyverse)

# Define your SEIR model functions here

# Shiny UI
ui <- fluidPage(
  titlePanel("SEIR Model Simulator"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("days", "Simulation Days", min = 1, max = 1000, value = 200),
      sliderInput("w", "Lag Days (w)", min = 0, max = 100, value = 10),
      sliderInput("S0", "Initial Susceptible (S0)", min = 0, max = 10000, value = 999),
      sliderInput("E0", "Initial Exposed (E0)", min = 0, max = 1000, value = 0),
      sliderInput("I0", "Initial Infected (I0)", min = 0, max = 1000, value = 1),
      sliderInput("R0", "Initial Recovered (R0)", min = 0, max = 1000, value = 0),
      sliderInput("beta", "Transmission Rate (beta)", min = 0, max = 1, value = 0.3, step = 0.01),
      sliderInput("sigma", "Incubation Rate (sigma)", min = 0, max = 1, value = 1/5, step = 0.01),
      sliderInput("gamma", "Recovery Rate (gamma)", min = 0, max = 1, value = 1/10, step = 0.01)
    ),
    mainPanel(
      plotOutput("seirPlot")
    )
  )
)

# Server logic
server <- function(input, output) {
  # Reactive expression to compute model results
  modelResults <- reactive({
    results <- seir_model_lag_corrected(input$S0, input$E0, input$I0, input$R0, input$beta, input$sigma, input$gamma, input$w, input$days)
    results
  })
  
  # Automatically update the plot when inputs change
  output$seirPlot <- renderPlot({
    results <- modelResults() # Fetch reactive model results
    
    results_long <- results %>%
      pivot_longer(cols = c(S, E, I, R), names_to = "Compartment", values_to = "Count") %>%
      mutate(Compartment = factor(Compartment, levels = c("R", "I", "E", "S")))
    
    ggplot(results_long, aes(x = day, y = Count, color = Compartment)) +
      geom_line() +
      scale_color_manual(values = c("S" = "blue", "E" = "orange", "I" = "red", "R" = "green")) +
      labs(x = "Days", y = "Population", title = "SEIR Model with Lag") +
      theme_minimal() +
      theme(legend.title = element_blank()) +
      guides(color = guide_legend(reverse = TRUE))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
