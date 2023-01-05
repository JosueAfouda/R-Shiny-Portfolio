# CUSTOM HTML LINKS IN SHINY


library(shiny)
library(tidyverse)
library(gapminder)
library(shinythemes)


# Define UI for application that draws a histogram
ui <- fluidPage(

  themeSelector(),

  tags$head(HTML("<link href='http://fonts.googleapis.com/css?family=Jura' 
 rel='stylesheet' type='text/css'>")),
  
  # Mettre le titre en vert
  h2("Custom HTML", style = "font-family: 'Jura'; 
 color: green; font-size: 64px;"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("country", "Country", 
                   c("Afghanistan", "Bahrain", "Cambodia"))
    ),
    
    mainPanel( 
      h3("Time series"),
      HTML("<p><em>Life expectancy</em> over time</p>"),
      plotOutput("plotDisplay"),
      tags$a("Go to my Website",href='https://afouda-datascience.com/')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$plotDisplay <- renderPlot({
    
    gapminder %>%
      filter(country == input$country) %>%
      ggplot(aes(x = year, y = lifeExp)) +
      geom_line()
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
