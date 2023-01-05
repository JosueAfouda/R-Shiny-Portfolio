#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          # p() est pour ajouter un paragraphe
          p("For more information about ", strong("Shiny"), " look at the ", 
            a(href = "http://shiny.rstudio.com/articles/", 
              "documentation.")),
          
          # hr() pour ajouter une ligne horizontale
          hr(),
           plotOutput("distPlot"),
          p("If you wish to write some code you may like to use the pre()
            function like this:"), 
          
          # La fonction pre() permet d'écrire du code
            pre('sliderInput("year", "Year", min = 1893, max = 2005, 
                   value = c(1945, 2005), sep = "")')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
