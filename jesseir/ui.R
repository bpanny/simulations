library(shiny)

ui <- fluidPage(
  titlePanel("SEIR Model Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("S", "Initial Susceptible (S)", min = 0, max = 10000, value = 999, step = 1),
      sliderInput("I", "Initial Infected (I)", min = 0, max = 10000, value = 1, step = 1),
      sliderInput("beta", "Beta 1 (Contact Rate)", min = 0, max = 1, value = 0.01, step = .001),
      sliderInput("beta2", "Beta 2 (Contact Rate)", min = 0, max = 1, value = 0.01, step = .001),
      sliderInput("lambda", "Lambda (Choose Active)", min = 0, max = 1, value = 0.01, step = .001),
      sliderInput("eta", "Eta (Choose Inactive)", min = 0, max = 1, value = 0.005, step = .001),
      sliderInput("theta1", "Theta 1 (E1 Revert to S)", min = 0, max = 1, value = 0.9, step = .001),
      sliderInput("theta2", "Theta 2 (E2 Revert to S)", min = 0, max = 1, value = 0.8, step = .001),
    ),
    
    mainPanel(
      plotOutput("seirPlot")
    )
  )
)