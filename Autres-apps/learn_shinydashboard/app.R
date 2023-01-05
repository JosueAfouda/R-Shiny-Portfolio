#packages
library(shiny)
library(shinydashboard)
library(DT)

# Début du programme

header <- dashboardHeader(
    dropdownMenu(
        type = 'messages',
        messageItem(
            from = 'Josue Afouda',
            message = 'Check out J.A DATATECH CONSULTING',
            href = 'https://afouda-datascience.com/'
        ),
        messageItem(
            from = 'Josue Afouda',
            message = 'Follow me on Linkedin',
            href = 'https://www.linkedin.com/in/josueafouda'
        ),
        messageItem(
            from = 'Josue Afouda',
            message = 'Subscribe to my YouTube channel',
            href = 'https://www.youtube.com/channel/UCpd56FfjlkKbkHlbgY6XE3w'
        )
    )
)

sidebar <- dashboardSidebar(
    # Add a slider
    sliderInput(
        inputId = "height",
        label = "Height",
        min = 66,
        max = 264,
        value = 264),
    
    selectInput(inputId = "name", 
                label = "Name",
                choices = iris$Species
    ),
    
    sidebarMenu(
        menuItem(
           text = 'Dashboard',
           tabName = 'dashboard'
        ),
        menuItem(
            text = 'Inputs',
            tabName = 'inputs'
        )
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = 'dashboard'),
        tabItem(tabName = 'inputs')
    ),
    
    textOutput('name'),
    
    DTOutput('table')
)

# Create the UI using the header, sidebar, and body
ui <- dashboardPage(
    skin = "purple",
    header = header,
    sidebar = sidebar,
    body = body
)

server <- function(input, output, session) {
    output$name <- renderText({
        input$name
    })
    
    reactive_starwars_data <- reactiveFileReader(
        intervalMillis = 1000,
        session = session,
        filePath = "http://s3.amazonaws.com/assets.datacamp.com/production/course_6225/datasets/starwars.csv",
        readFunc = function(filePath) { 
            read.csv(url(filePath))
        }
    )
    
    output$table <- renderDT({
        reactive_starwars_data()
    })
    
}

shinyApp(ui, server)