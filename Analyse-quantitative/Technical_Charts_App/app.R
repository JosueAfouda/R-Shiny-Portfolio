# Importation des librairies
library(tidyverse)
library(stringr)
library(shiny)
library(shinythemes)
library(DT)
library(quantmod)
library(plotly)
library(readr)

# Importation des dataframes des noms des actions
dax_ticker_name <- read_csv("dax_tickers_names.csv")
nikkei_ticker_name <- read_csv("nikkei_tickers_names.csv")
sp500_ticker_name <- read_csv("sp500_tickers_names.csv")
cac40_ticker_name <- read_csv("cac40_tickers_names.csv")
ftse100_ticker_name <- read_csv("ftse100_tickers_names.csv")


colors <- c("Close" = "blue", "Short MA" = "red", "Long MA" = "green")


# Interface Utilisateur
ui <- fluidPage(
    
    # Titre de l'application
    h1("Technical Stock Charts Web App", style = "font-family: 'Jura'; 
     color: red; font-size: 80px;"),
    h4(tags$b("R Shiny web application for Quantitative Analysis of several financial markets (United States of America, France, Germany, England and Japan).")),
    h4(tags$b("This app may not display charts for some stocks due to their data unavailability on Yahoo Finance.")),
    
    # Thème
    theme = shinytheme('sandstone'),
    
    h2(tags$a("Author : Josué AFOUDA", 
              href = 'https://www.linkedin.com/in/josu%C3%A9-afouda/')),
    
    h3(tags$a("Learn R Shiny",href='https://youtube.com/playlist?list=PLmJWMf9F8euStJ32KHOThQzJ7hub-JXA0')),
    
    br(),
    
    fluidRow(
        column(12, 
               wellPanel(radioButtons('indice', 'Choose a stock market index',
                                      choices = c('France (CAC 40)', 
                                                  'USA (S&P 500)',
                                                  'Germany (DAX)',
                                                  'England (FTSE 100)', 
                                                  'Japan (Nikkei 225)'),
                                      inline = T
               ))  
        )
    ),
    
    fluidRow(
        column(12, wellPanel(uiOutput('MarketControl')))
    ),
    
    fluidRow(
        column(3, 
               wellPanel(
                   
                   dateRangeInput('period', 'Date range:', 
                                  '2015-01-01', '2021-08-01'),
                   
                   radioButtons('parameter', 'Price or Return ?', 
                                choices = c('Price', 'Return')),
                   
                   
                   sliderInput('short', 'Short Moving Average', 
                               min = 0, max = 100, value = 10, step = 1),
                   
                   sliderInput('long', 'Long Moving Average', 
                               min = 0, max = 200, value = 50, step = 1),
                   
                   checkboxInput('ShowData', 'Show the data', FALSE)
                   
               )
        ),
        #column(9, plotlyOutput("boxplot")),
        column(4, plotlyOutput("hist", height = 440)),
        column(4, offset = 1, plotlyOutput("boxplot", height = 440))
    ),
    
    fluidRow(
        column(12, wellPanel(plotlyOutput("lineplot")))
    ),
    
    fluidRow(
        column(12, wellPanel(
            conditionalPanel(
                condition = "input.ShowData",
                numericInput("Nrows", "Number of lines to show", value = 10)
            ),
            DTOutput('StockData')
        ))
    ),
    
    
    fluidRow(
        column(3, 
               wellPanel(
                   
                   numericInput('yearsubset', 
                                'Specify a particular year of the serie to view (You must choose a year in the data period)', 
                                value = 2017)
               )
        ),
        column(9, plotOutput('Chart', height = 700))  
    )
    
)

# Serveur
server <- function(input, output) {
    
    # Variable réactive qui indique les différents marchés boursiers
    market <- reactive({
        if (input$indice == 'France (CAC 40)') {
            cac40_ticker_name$NameOfStock
        } else if (input$indice == 'England (FTSE 100)') {
            ftse100_ticker_name$NameOfStock
        } else if (input$indice == 'Japan (Nikkei 225)') {
            nikkei_ticker_name$NameOfStock
        } else if (input$indice == 'Germany (DAX)') {
            dax_ticker_name$NameOfStock
        } else {
            sp500_ticker_name$NameOfStock
        }
    })
    
    output$MarketControl <- renderUI({
        selectInput('stock', 'Choose an Asset', 
                    choices = market())
    })
    
    
    # Importation des données
    df <- reactive({
        
        stock_data <- getSymbols(
            str_split(input$stock, pattern = ",")[[1]][2], 
            auto.assign = F,
            from = as.Date(input$period[1]), 
            to = as.Date(input$period[2])
            )
        
        names(stock_data) <- c("Open", "High", "Low", 
                               "Close", "Volume", "Adjusted")
        
        stock_data
        
    })
    
    
    # Calcul des rendements
    returns <- reactive({
        dailyReturn(df()$Adjusted)
    })
    
    
    # Evolution des Prix/Rendements
    output$lineplot <- renderPlotly({
        
        if (input$parameter == 'Price') {
            p <- ggplot(df(), aes(x = index(df()))) + 
                geom_line(aes(y = Adjusted, color = "Close")) +
                geom_line(aes(y = SMA(Adjusted, n = input$short), color = "Short MA")) +
                geom_line(aes(y = SMA(Adjusted, n = input$long), color = "Long MA")) +
                labs(x = "Date", y = "Adjusted Closed Price", color = "Legend", 
                     title = paste("Compagny Name:", 
                                   str_split(input$stock, pattern = ",")[[1]][1], 
                                   " SYMBOL:", str_split(input$stock, pattern = ",")[[1]][2])
                ) +
                scale_color_manual(values = colors) +
                theme_bw()
            
            ggplotly(p)
            
        } else {
            r <- ggplot(returns(), aes(x = index(df()))) + 
                geom_line(aes(y = daily.returns)) +
                labs(x = "Date", y = "Daily Returns", 
                     title = paste("Compagny Name:", 
                                   str_split(input$stock, pattern = ",")[[1]][1], 
                                   " SYMBOL:", str_split(input$stock, pattern = ",")[[1]][2])
                ) +
                theme_bw() +
                theme(legend.position = "none") 
            
            ggplotly(r)
        }
        
    })
    
    
    # Boîte à moustache
    output$boxplot <- renderPlotly({
        if (input$parameter == 'Price') {
            box1 <- ggplot(df()) +
                geom_boxplot(aes(y = Adjusted, fill = "red")) +
                labs(y = "Adjusted Closed Price",
                     title = paste("Compagny Name:", 
                                   str_split(input$stock, pattern = ",")[[1]][1], 
                                   " SYMBOL:", str_split(input$stock, pattern = ",")[[1]][2]),
                ) +
                theme_minimal() +
                theme(legend.position = "none")
            ggplotly(box1)
            
        } else {
            box2 <- ggplot(returns()) +
                geom_boxplot(aes(y = daily.returns, fill = "red")) +
                labs(y = "Daily Returns",
                     title = paste("Compagny Name:", 
                                   str_split(input$stock, pattern = ",")[[1]][1], 
                                   " SYMBOL:", str_split(input$stock, pattern = ",")[[1]][2]),
                ) +
                theme_minimal() +
                theme(legend.position = "none")
            ggplotly(box2)
        }
    })
    
    
    # Histogramme
    output$hist <- renderPlotly({
        if (input$parameter == 'Price') {
            histo <- ggplot(df()) +
                geom_histogram(aes(x = Adjusted, fill = "red")) +
                labs(x = "Adjusted Closed Price",
                     title = paste("Compagny Name:", 
                                   str_split(input$stock, pattern = ",")[[1]][1], 
                                   " SYMBOL:", str_split(input$stock, pattern = ",")[[1]][2]),
                ) +
                theme_minimal() +
                theme(legend.position = "none")
            ggplotly(histo)
            
        } else {
            histo2 <- ggplot(returns()) +
                geom_histogram(aes(x = daily.returns, fill = "red")) +
                labs(x = "Daily Returns",
                     title = paste("Compagny Name:", 
                                   str_split(input$stock, pattern = ",")[[1]][1], 
                                   " SYMBOL:", str_split(input$stock, pattern = ",")[[1]][2]),
                ) +
                theme_minimal() +
                theme(legend.position = "none")
            ggplotly(histo2)
        }
    })
    
    
    # Tableau des données à montrer 
    data_to_show <- reactive({
        if (input$parameter == 'Price') {
            df()
        } else {
            returns()
        }
    })
    
    
    output$StockData <- renderDT({
        if (input$ShowData) {
            datatable(
                as.data.frame(data_to_show()),
                extensions = 'Buttons', 
                options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    pageLength = input$Nrows
                ),
                style = "bootstrap", rownames = T
            )
        } else {
            NULL
        }
    })
    
    
    # chartSeries
    output$Chart <- renderPlot({
        chartSeries(
            df(),
            subset = as.character(input$yearsubset),
            TA = c(addBBands(), addRSI(), addEMA(), addMACD()),
            theme = chartTheme("black"),
            name = paste("Compagny Name:", 
                         str_split(input$stock, pattern = ",")[[1]][1], 
                         " SYMBOL:", str_split(input$stock, pattern = ",")[[1]][2])
        )
    })
    
    
    
}

# Exécution de l'application 
shinyApp(ui = ui, server = server)
