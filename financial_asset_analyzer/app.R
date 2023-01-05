library(shiny)
library(shinythemes)
library(DT)
library(quantmod)
library(ggplot2)
library(plotly)


colors <- c("Close" = "blue", "Short MA" = "red", "Long MA" = "green")

# Interface Utilisateur
ui <- fluidPage(
  
    theme = shinytheme("flatly"),

    # Application title
    titlePanel("Analyse d'actifs en Bourse"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            
            selectInput('stock', 'Choisis un actif', 
                        choices = c('GOOG', 'AAPL', 'META', 'AMZN', 'TSLA')),
            
            dateRangeInput('period', 'Date range:', 
                           '2015-01-01', '2020-12-31'),
            
            sliderInput('short', 'Short Moving Average', 
                        min = 0, max = 100, value = 10, step = 1),
            
            sliderInput('long', 'Long Moving Average', 
                        min = 0, max = 200, value = 50, step = 1)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("lineplot", 1000, 700)
        )
    )
)

# Serveur
server <- function(input, output) {
  
  df <- reactive({
    
    #stocks <- new.env()
    data <- getSymbols(
      #env = stocks,
      input$stock, 
      auto.assign = FALSE,
      from = as.Date(input$period[1]), 
      to = as.Date(input$period[2])
    )
    Cl(data)
    #do.call(merge, lapply(stocks, Cl))
  })
  
  output$lineplot <- renderPlotly({
    
    p <- ggplot(df(), aes(x = index(df()))) + 
      geom_line(aes(y = df(), color = "Close")) +
      geom_line(aes(y = SMA(df(), n = input$short), color = "Short MA")) +
      geom_line(aes(y = SMA(df(), n = input$long), color = "Long MA")) +
      labs(x = "Date", y = "Price", color = "Legend", title = input$stock) +
      scale_color_manual(values = colors) +
      theme_bw()
    
    ggplotly(p)
    
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
