# Josue AFOUDA

library(shiny)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("All ggplot colors"),

    # Sidebar with a select input for color's name
    sidebarLayout(
        sidebarPanel(
            selectInput("color_name",
                        "Name of color:", choices = colors())
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot", height = 700)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
        # draw the histogram with the specified color
        p <- ggplot(iris, aes(x = Sepal.Length)) + geom_histogram(fill = input$color_name)
        ggplotly(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
