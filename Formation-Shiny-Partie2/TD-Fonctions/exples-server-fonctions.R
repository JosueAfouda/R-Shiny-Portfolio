# Téléchargement de données

## req(input$file) : pour être sûr que l'exécution du code attendra que le premier fichier soit uploadé par l'utilisateur

## argument accept : permet de limiter les extensions possibles de fichiers (exemple : accept = ".csv" pour charger uniquement les fichiers csv)

## Mais l'argument accept n'est qu'une suggestion au navigateur et n'est pas toujours appliqué, il est donc recommandé de le valider 

## (voir Validation des inputs de l'utilisateur dans Partie 1 de la formation)

## Pour connaitre l'extension d'un fichier dans R, le moyen facile est le code : tools::file_ext()

## Exemple : tools::file_ext("D:/customer_call_transcriptions.csv") retourne "csv" comme résultat

library(shiny)

ui <- fluidPage(
  fileInput("file", NULL, accept = c(".csv", ".tsv")),
  numericInput("n", "Rows", value = 5, min = 1, step = 1),
  tableOutput("head")
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    
    switch(
      ext,
      csv = vroom::vroom(input$file$datapath, delim = ","), # la fonction vroom permet de lire un fichier et retourne un tibble
      tsv = vroom::vroom(input$file$datapath, delim = "\t"),
      validate("Fichier invalide. SVP, sélectionner un fichier .csv ou .tsv")
    )
  })
  
  # Affichage des n premières lignes de data
  output$head <- renderTable({
    head(data(), input$n)
  })
  
}

shinyApp(ui = ui, server = server)

# Nous allons réécrire l'application ci-dessus en définissant une fonction qui génère un composant du serveur

load_file <- function(nom, chemin) {
  extensions <- tools::file_ext(nom)
  switch(
    extensions,
    csv = vroom::vroom(chemin, delim = ","),
    tsv = vroom::vroom(chemin, delim = "\t"),
    validate("Fichier invalide. SVP, sélectionner un fichier .csv ou .tsv")
  )
}

ui <- fluidPage(
  fileInput("file", NULL, accept = c(".csv", ".tsv")),
  numericInput("n", "Rows", value = 5, min = 1, step = 1),
  tableOutput("head")
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    load_file(nom = input$file$name, chemin = input$file$datapath)
  })
  
  output$head <- renderTable({
    head(data(), input$n)
  })
  
}

shinyApp(ui = ui, server = server)

# Ce qu'on vient de faire a plusieurs avantages :

## Puisque la fonction load_file() est totalement indépendante, on peut la placer en dehors du code de l'app (dans un autre fichier .R par exemple)
    # ce qui rend le code server plus simple et plus lisible

## La manière dont on a écrit cette fonction load_file() fait qu'on peut passer en argument des valeurs non réactives comme des valeurs réactives.

  # L'appel de cette fonction transforme le résultat en objet réactif si et seulement si c'est nécessaire c-a-d si les valeurs des arguments
  # de la fonction sont réactives.
  # En général, il est préférable de garder les parties réactives et non réactives de votre application aussi séparées que possible.

## Un autre avantage est que nous pouvons ainsi facilement réaliser nos tests sur l'application. Par exemple, on peut jouer facilement avec la 
  # fonction load_file() au niveau de notre console et voir comment l'application se comporte.

# N.B : Le fait d'écrire des fonctions indépendantes (que vous pouvez placer en dehors du code de votre app) comme dans l'exemple ci-dessus est génial.
# Dans certains cas, il faudra nécessairement que votre fonction soit à l'intérieur du code server pour que l'app fonctionne.
# Dans ces cas, au moins vous réduisez la duplication de code même si ce faisant cela ne rendra pas vos tests plus faciles à réaliser.


# Conclusion du TD sur les fonctions :

  ## L'écriture de fonctions a dans tous les cas des avantages. 
  # Les fonctions que nous avons écrit au cours de ce TD permettent de générer seulement des composants de l'UI ou des composants du server. 
  # Pas les 2 à la fois
  
# Dans le prochain TD, nous verrons les modules Shiny qui permettent de coordonner code UI et code server en un seul object.




