# Version du 27/03/2021 à 13h36

# Importation des packages nécessaires

library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(DT)
library(factoextra)
library(Rtsne)

# S'assurer de la reproductibilité des résultats de l'algorithme t-SNE qui est un processus aléatoire

seed = 111

set.seed(seed)

# Fonction pour normaliser les donnees entre 0 et 1

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Interface Utilisateur (frontend)
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  h1("R SHINY APP FOR K-MEANS CLUSTERING AND DIMENSIONALITY REDUCTION"),
  
  h2("Author : Josue AFOUDA"),
  
  tags$a("Follow me on Linkedin",href='https://www.linkedin.com/in/josu%C3%A9-afouda/'),
  
  sidebarLayout(
    sidebarPanel(
      textInput('file', "Entrez l'url du fichier CSV de vos donnees :",
                value = 'https://raw.githubusercontent.com/JosueAfouda/Mes-Projets-R-Shiny/main/iris_num.csv'),
      
      
      checkboxInput("header", "Header", TRUE),
      
      helpText("Decochez ce bouton si votre fichier n'a pas de noms de colonnes"),
      
      selectInput('scaled_met', 'Normalisation ou Standardisation des donnees :',
                  c('None',
                    'Normalisation',
                    'Standardisation')),
      
      selectInput('dim_reduction', 'Dimensionnality Reduction:', 
                  c('PCA',
                    'T-SNE')),
      
      conditionalPanel(
        condition = "input.dim_reduction == 'T-SNE'",
        checkboxInput("pca_tsne", "T-SNE with PCA or not:", FALSE),
        numericInput('n_iter', 'Number of iterations:', 1000, min = 1000, step = 500)
      ),
      
      helpText("Le nombre de clusters est un entier naturel strictement positif"),
      
      numericInput('n_clusters', 
                   'Selectionnez le nombre de clusters :', 
                   value = 3, min = 1),
      
      helpText("Choisissez les 2 variables a representer dans un nuage de points"),
      
      numericInput('varx', 'Numero de la variable en Axe X :', 
                   value = 1, min = 1),
      
      numericInput('vary', 'Numero de la variable en Axe Y :', 
                   value = 2, min = 1),
      
      helpText("Cliquer sur 'Submit' après chaque changement de paramètre(s)"),
      
      actionButton("go", "Submit")
      
      
      
    ),
    
    # Tableaux et Graphiques dans le panneau principal
    mainPanel(
      tabsetPanel(
        tabPanel('Cleaned Data', DTOutput('dataframe'), 
                 downloadButton('download_clean_data', label = 'Telechargez les donnees nettoyees :')),
        
        tabPanel('Norm/Stand Data', DTOutput('scaled'), 
                 downloadButton('download_stand_data', label = 'Telechargez les donnees standardisees ou nourmalisees :')),
        
        tabPanel('Dim. Reduction', 
                 conditionalPanel(condition = "input.dim_reduction == 'PCA'",
                                  plotOutput("pca_screeplot"), 
                                  plotOutput("pca_biplot"),
                                  numericInput('first_cp', 'How many first components do you want to choose ?', 2, min = 1, step = 1),
                                  DTOutput('table_pca'),
                                  downloadButton('save_pca_data', label = 'Save PCA Data :'),
                                  ),
                 
                 conditionalPanel(condition = "input.dim_reduction == 'T-SNE'",
                                  plotOutput("tsne_costs_plot"),
                                  DTOutput("tsne_data"),
                                  downloadButton('save_tsne_data', label = 'Save TSNE Data :'),
                                
                                  plotOutput('tsne_biplot'))
                 
        ), 
        
        tabPanel('Elbow Method', plotlyOutput('elbow', height = "550px")),
        
        tabPanel('Clusters Visualization', plotOutput("scatter_plot", 850, 600)),
        
        tabPanel('Average Silhouette', plotlyOutput('silhouette', height = "550px")),
        
      )
      
    )
  )
)

# Serveur (backend)
server <- function(input, output) {
  
  df <- reactive({
    df_clean <- read.csv(input$file, header = input$header)
    df_clean <- df_clean[, !sapply(df_clean, is.character)]
    df_clean <- df_clean[complete.cases(df_clean), ]
    df_clean
  })
  # Essai
  output$dataframe <- renderDT({
    input$go
    isolate({
      #df()
      datatable(df(), rownames = FALSE, 
                extensions = 'Buttons', options = list(
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                ),
                filter = 'top', style = 'bootstrap')
    })
  })
  
  output$download_clean_data <- downloadHandler(
    filename <- function() {
      paste("clean_data_", Sys.Date(), ".csv", sep=",")
    },
    content <- function(file) {
      write.csv(df(), file)
    }
  )
  
  data <- reactive({
    
    if (input$scaled_met == 'Normalisation') {
      normalize(df())
    } else if (input$scaled_met == 'Standardisation') {
      as.data.frame(scale(df()))
    } else if (input$scaled_met == 'None') {
      df()
    }
    
  })
  
  # PCA Model
  
  model_pca <- reactive({
    prcomp(data(), scale = FALSE, center = FALSE)
  })
  
  # PCA Scree Plot
  
  output$pca_screeplot <- renderPlot({
    input$go
    isolate({
      fviz_screeplot(model_pca(), ncp = ncol(data()))
    })
  })
  
  # PCA Biplot
  
  output$pca_biplot <- renderPlot({
    input$go
    isolate({
      plot(model_pca()$x[, 1:2])
    })
  })
  
  # PCA Data
  
  output$table_pca <- renderDT({
    input$go
    isolate({
      #as.data.frame(model_pca()$x[, 1:input$first_cp])
      datatable(as.data.frame(model_pca()$x[, 1:input$first_cp]), 
                rownames = FALSE, 
                extensions = 'Buttons', options = list(
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                ),
                filter = 'top', style = 'bootstrap')
    })
  })
  
  # Save PCA Data
  
  output$save_pca_data <- downloadHandler(
    filename <- function() {
      paste("pca_data_", Sys.Date(), ".csv", sep=",")
    },
    content <- function(file) {
      write.csv(as.data.frame(model_pca()$x[, 1:input$first_cp]), file)
    }
  )
  
  # TSNE model
  model_tsne <- reactive({
    Rtsne(data(), pca = input$pca_tsne, dims = 2, 
          check_duplicates = FALSE, max_iter = input$n_iter)
  })
  
  # Costs Plot for TSNE
  
  output$tsne_costs_plot <- renderPlot({
    input$go
    isolate({
      plot(model_tsne()$costs, type = 'l', ylab = 'Costs')
    })
  })
  
  # TSNE 2 dim data
  output$tsne_data <- renderDT({
    input$go
    isolate({
      ####
      datatable(data.frame(tsne_x = model_tsne()$Y[, 1], 
                           tsne_y =model_tsne()$Y[, 2]), 
                rownames = FALSE, 
                extensions = 'Buttons', options = list(
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                ),
                filter = 'top', style = 'bootstrap')
      ####
      
    })
  })
  
  # TSNE scatter plot
  output$tsne_biplot <- renderPlot({
    input$go
    isolate({
      plot(data.frame(tsne_x = model_tsne()$Y[, 1], 
                      tsne_y =model_tsne()$Y[, 2]))
    })
  })
  
  # Save TSNE Data
  
  output$save_tsne_data <- downloadHandler(
    filename <- function() {
      paste("tsne_data_", Sys.Date(), ".csv", sep=",")
    },
    content <- function(file) {
      write.csv(data.frame(tsne_x = model_tsne()$Y[, 1], 
                           tsne_y =model_tsne()$Y[, 2]), file)
    }
  )
  
  
  output$scaled <- renderDT({
    
    input$go
    isolate({
      #####
      datatable(data(), 
                rownames = FALSE, 
                extensions = 'Buttons', options = list(
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                ),
                filter = 'top', style = 'bootstrap')
      #####
      
    })
    
  })
  
  output$download_stand_data <- downloadHandler(
    filename <- function() {
      paste("scaled_or_norm_data_", Sys.Date(), ".csv", sep=",")
    },
    content <- function(file) {
      write.csv(data(), file)
    }
  )
  
  output$elbow <- renderPlotly({
    input$go
    isolate({
      fviz_nbclust(data(), FUNcluster = kmeans, method = "wss")
    })
  })
  
  output$silhouette <- renderPlotly({
    input$go
    isolate({
      fviz_nbclust(data(), FUNcluster = kmeans, method = "silhouette")
    })
  })
  
  # K-Means Model
  
  model <- reactive({
    kmeans(data(), centers = input$n_clusters, nstart = 20)
  })
  
  data_selected <- reactive({
    cbind(data()[input$varx], data()[input$vary])
  })
  
  output$scatter_plot <- renderPlot({
    
    input$go
    isolate({
      plot(data_selected(), col = as.factor(model()$cluster), pch = 20, cex = 3)
    })
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)