library(shiny)
library(DT)
library(plotly)

#importation de la base
df<- read.csv("hmeq.csv",sep=";")
dfclean <- df[complete.cases(df), ]
dfclean$BAD <- as.factor(dfclean$BAD)
dfclean$REASON <- as.factor(dfclean$REASON)
dfclean$JOB <- as.factor(dfclean$JOB)

vars_num <- names(dfclean[ , unlist(lapply(dfclean, is.numeric))])


# Define UI for application that draws a histogram
ui <- fluidPage(


    # Application title
    titlePanel("Analyse exploratoire des données du risque de credit"),
    
    h2("Author : Josue AFOUDA"),
    
    tags$a("Follow me on Linkedin",href='https://www.linkedin.com/in/josu%C3%A9-afouda/'),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("hist_id", "choisis une variable numérique", choices = vars_num)
        
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(
             tabPanel("DATA", DTOutput("table")),
             tabPanel("STATISTIQUE", verbatimTextOutput('stat')),
             tabPanel('VISUALISATION', 
                      tabsetPanel(
                        tabPanel("HISTOGRAMME", 
                                 sliderInput('bin', "Choisis le nombre de bins:", 
                                             min = 1, max = 100, value = 50),
                                 plotOutput("hist"), 
                                 plotlyOutput("hist_bad")),
                        tabPanel("BOXPLOT",plotlyOutput("box")),
                        tabPanel("BARPLOT", 
                                 selectInput('var_cat', 
                                             'Choisis une variable catégorielle',
                                             choices = c('BAD', "REASON", "JOB")),
                                 plotlyOutput('bar'), 
                                 plotlyOutput('bar_bad'))
                        )
                      )
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$table <- renderDT({
      dfclean
    })
    
    output$stat<- renderPrint({
      summary(dfclean)
    })
    
    output$hist<-renderPlot({
      hist(dfclean[ ,input$hist_id], 
           breaks = input$bin,
           main=paste("histogramme de ", input$hist_id),
           xlab = input$hist_id)
    })
    
    output$hist_bad <- renderPlotly({
      h <- ggplot(dfclean, aes(dfclean[ ,input$hist_id])) + 
        geom_histogram(fill = "red", bins = input$bin) + 
        facet_wrap(~BAD, scales = "free", ncol = 2) +
        labs(x = input$hist_id)
      ggplotly(h)
    })
    
    output$box<- renderPlotly({
      b <- ggplot(dfclean, aes(x = BAD, y= dfclean[ ,input$hist_id], fill = BAD)) + 
        geom_boxplot() +
        labs(y = input$hist_id)
      ggplotly(b)
    })

    output$bar <- renderPlotly({
      bar_plot <- ggplot(dfclean, aes(dfclean[ ,input$var_cat])) + 
        geom_bar() +
        labs(x = input$var_cat)
      ggplotly(bar_plot)
    })
    
    output$bar_bad <- renderPlotly({
      bar2_plot <- ggplot(dfclean, aes(dfclean[ ,input$var_cat], fill = BAD)) + 
        geom_bar() +
        labs(x = input$var_cat)
      
      ggplotly(bar2_plot)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
