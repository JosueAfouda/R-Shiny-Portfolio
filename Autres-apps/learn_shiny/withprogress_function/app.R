# Code pour la fonction withProgress()


library(shiny)

ui <- fluidPage(
  plotOutput("plot")
)

server <- function(input, output) {
  output$plot <- renderPlot({
    
    #########Fonction withProgress()########################################
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                 })
    ########################################################################
    
    plot(cars)
  })
}

shinyApp(ui, server)

