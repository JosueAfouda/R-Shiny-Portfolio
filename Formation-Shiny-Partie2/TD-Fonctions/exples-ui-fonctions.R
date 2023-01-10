library(shiny)

# Au niveau de l'ui, on a copié/coller 4 fois sliderInput()
ui <- fluidRow(
  sliderInput("one", "one", min = 0, max = 1, value = 0.5, step = 0.1),
  sliderInput("two", "two", min = 0, max = 1, value = 0.5, step = 0.1),
  sliderInput("three", "three", min = 0, max = 1, value = 0.5, step = 0.1),
  sliderInput("four", "four", min = 0, max = 1, value = 0.5, step = 0.1)
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)


# Dans le code ci-dessus, on reconnait facilement ce qui est répété plusieurs fois
# On peut donc créer une fonction pour avoir un code UI plus simple

sliderInput01 <- function(id) {
  sliderInput(id, label = id, min = 0, max = 1, value = 0.5, step = 0.1)
}

ui <- fluidRow(
  sliderInput01("one"),
  sliderInput01("two"),
  sliderInput01("three"),
  sliderInput01("four"),
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)

# Dans l'exemple ci-dessus, ce qui est intéressant est que si nous voulons
# changer par exemple la valeur de l'argument step, nous le ferons une seule fois
# au lieu de 4 fois.


# C'est vrai, vous allez me dire qu'on a quand même répété 4 fois la fonction sliderInput01
# On peut utiliser la programmation fonctionnelle avec les fontions purrr pour rendre cela encore plus lisible

library(purrr)
vars <- c("one", "two", "three", "four")
sliders <- map(vars, sliderInput01)

ui <- fluidRow(
  sliders
)
server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)

##################################################### Exemple 2 ##########################################################################
ui <- fluidRow(
  sliderInput("slider", "Slide me", 0, 100, 1),
  textOutput("num")
)

server <- function(input, output) {
  output$num <- renderText({
    input$slider
  })
}

shinyApp(ui = ui, server = server)

# Application avec fonction

slider <- function() {
  sliderInput("slider", "Slide Me", 0, 100, 1)
}

ui <- fluidRow(
  slider(),
  textOutput("num")
)

server <- function(input, output) {
  output$num <- renderText({input$slider})
}

shinyApp(ui = ui, server = server)


# Imaginons qu'on veuille faire 2 slides et deux outputs.
# On pourrait écrire :

ui <- fluidRow(
  slider(),
  textOutput("num1"),
  slider(),
  textOutput("num2")
)

server <- function(input, output) {
  output$num1 <- renderText({input$slider})
  output$num1 <- renderText({input$slider})
}

shinyApp(ui = ui, server = server)

# Avec le code ci-dessus, on n'a pas le fonctionnement espéré
# Voici ce qu'on peut écrire :

slider <- function(id) {
  sliderInput(id, "Slide Me", 0, 100, 1)
}

ui <- fluidRow(
  slider("slider1"),
  textOutput("num1"),
  slider("slider2"),
  textOutput("num2")
)

server <- function(input, output) {
  output$num1 <- renderText({input$slider1})
  output$num2 <- renderText({input$slider2})
}

shinyApp(ui = ui, server = server)

######################################################### Exemple 3 ###########################################################################





































