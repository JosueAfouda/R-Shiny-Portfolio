# Code pour la fonction withSpinner()


library(shiny)
library(shinycssloaders)

ui <- fluidPage(
  withSpinner(plotOutput("plot"))
)

server <- function(input, output) {
  output$plot <- renderPlot({
    
    plot(cars)
  })
}

shinyApp(ui, server)