library(shiny)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dendextend)
library(scales)
library(DT)

dendrogram_color_scheme <- c(head(brewer.pal(8, 'Set1'), 2), brewer.pal(8, "Dark2"))

# Import Data
market <- read.csv("data/global_market_data.csv", check.names = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  fluidRow(
    class="vertical-align",
    column(3, 
           p(strong("Hello"), ", this application allows you to specify different clusters of customers", 
             "based on age and income to determine more targeted marketing segments. Simply, ", 
             "move the slider at right for more or less clusters and chose a clustering method."), 
           p("After choosing a cluster scheme", 
             "you can filter and download customer data based on those clusters to run a campaign.")
    ),
    # allow the user to pick a total number of
    # clusters to create
    column(3,
           sliderInput("cluster_count",
                       label = "How Many Clusters?",
                       min = 2, max = 10, 
                       value = 6, step = 1)
    ),
    # allow the user to pick a clustering method
    # default to K-means
    column(6,
           radioButtons("cluster_method",
                        label = "Clustering Method?",
                        choices = c("K-means", "Hierarchical"), 
                        selected = "K-means", 
                        inline = FALSE)
    )
  ),
  
  fluidRow(
    column(7, 
           # display the ggplot of clusters
           plotOutput("cluster_viz", height = "500px")
    ),
    column(5, 
           h3("Cluster Summary Table"),
           # display the table of cluster summaries of age and income
           DT::dataTableOutput("campaign_summary_table", width = "100%")
    )
  ),
  
  hr(),
  h3("Potential Campaign Members"),
  fluidRow(
    column(4,
           p("The data in the table below shows all potential customers, their cluster ", 
             "membership, and their relative percentile of age and income in that cluster. ", 
             "Use the filter boxes at the top of the table to select even more granular ", 
             "sets of customers.")
    ),
    column(4,
           p("For example, if you select the 75th percentile and above ", 
             "for 'Income %Tile within Cluster', then you will be selecting only the top 25% ", 
             "of incomes from each cluster, which might be a nicely stratified sample of customers ", 
             "with disposable income relative to their peer group.")
    ),
    column(4,
           # add a button to download the data in the table
           tags$div(class = "itemright",
                    downloadButton("downloadDataFromTable",
                                   "Download Table Data"))
    )
  ),
  
  fluidRow(
    column(12, 
           # display the table of potential campaign members
           DT::dataTableOutput("campaign_table", width = "100%")
    )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  clustered_dataset <- reactive({
    
    # append the cluster_id to a result dataframe
    # remember to leave the market data.frame alone
    # since it is shared between other Shiny user sessions
    result_dat <- market
    
    # rebuild the model with the user specified cluster count
    # because this code is inside a "reactive" function
    # it will always re-execute whenever the user
    # changes input$cluster_count or input$cluster_method
    if (input$cluster_method == "K-means") {
      kmeans_model <- kmeans(x = market[, c("age_scale", "inc_scale")], 
                             centers = input$cluster_count)
      result_dat$cluster_id <- as.factor(kmeans_model$cluster)
    } else {
      hierarchical_model <- hclust(dist(market[, c("age_scale", "inc_scale")]),
                                   method = "ward.D2")
      result_dat$cluster_id <- as.factor(cutree(hierarchical_model, 
                                                input$cluster_count))
    }
    
    return(result_dat)
  })
  
  # create the cluster summary dataset
  cluster_summary_dataset <- reactive({
    # this will always re-evaluate whenver 
    # clustered_dataset changes. This is
    # how reactive changes "bubble" through
    # the application
    summary <- clustered_dataset() %>% 
      group_by(cluster_id) %>% 
      summarise(min_age = min(age), median_age = median(age),
                max_age = max(age), median_inc = median(income), 
                min_inc = min(income), max_inc = max(income)) %>%
      arrange(median_inc) %>% 
      select("Median Age" = median_age, "Median Income" = median_inc, 
             "Cluster Id" = cluster_id,
             "Min. Age" = min_age, "Max. Age" = max_age,
             "Min. Income" = min_inc, "Max. Income" = max_inc
      )
    return(summary)
  })
  
  # create all aspects of the table of data
  # it is recommended to use DT:: notation
  # so shiny is not confused with shiny::renderDataTable
  output$campaign_summary_table <- DT::renderDataTable({
    
    # create a datatable with a specific
    # set of configuration options
    d <- DT::datatable(cluster_summary_dataset(), 
                       options = list(
                         deferRender = FALSE,
                         # center all of the columns using the dt-center class
                         # defined in our app-styling.css file
                         columnDefs = list(list(className = "dt-center",
                                                targets = "_all")),
                         autoWidth = FALSE,
                         lengthChange = FALSE,
                         searching = FALSE,
                         paginate = FALSE,
                         info = FALSE,
                         ordering = FALSE
                       ),
                       filter = "none",
                       selection = "none",
                       class = "cell-border stripe", 
                       rownames = FALSE,
                       escape = FALSE)
    
    # add formats to the columns for ease of interpretation
    d <- d %>% 
      formatRound("Median Age", digits = 0) %>%
      formatCurrency(c("Median Income", "Min. Income", "Max. Income")) %>%
      formatStyle("Median Income", fontWeight = "bold",
                  background = styleColorBar(data = c(0, 125000),
                                             color = "#d5fdd5"),
                  backgroundSize = "100% 70%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
    # return the datatable object to be rendered
    return(d)
  })
  
  # create a visual representation of the clusters
  output$cluster_viz <- renderPlot({
    
    # calculate cluster centers so we can 
    # show a label at each center to denote
    # each cluster
    centers <- clustered_dataset() %>% 
      group_by(cluster_id) %>% 
      summarize(median_age = median(age), 
                median_income = median(income)) %>%
      ungroup() %>%
      mutate(cluster_id = as.numeric(as.character(cluster_id)), 
             label = paste("Cluster", cluster_id, "\nMedian Age:", 
                           median_age, "\nMedian Income:", 
                           dollar(round(median_income, 2)))) %>% 
      arrange(cluster_id) %>% 
      as.data.frame
    
    # calculate a total count of clusters to 
    # display in the plot title
    cluster_count <- nrow(centers)
    
    # the plotting method will change
    # based upon the clustering method
    if (input$cluster_method == "Hierarchical") {
      
      # recompute the clusters
      # based on the user-specified
      # cluster count
      cent <- NULL
      for (k in 1:cluster_count) {
        cent <- rbind(cent, colMeans(clustered_dataset()[clustered_dataset()$cluster_id == k,
                                                         c("age_scale",
                                                           "inc_scale"),
                                                         drop = FALSE]))
      }
      cut_tree <- hclust(dist(cent) ^ 2, method = "cen", 
                         members = table(clustered_dataset()$cluster_id))
      
      # convert the cut tree to a dendrogram to 
      # prepare it for plotting and add
      # more informative labels
      dend <- cut_tree %>% as.dendrogram
      labels(dend) <- centers$label
      
      # using the ggdendro package
      # add styling to the dendrogram
      dend <- dend %>%
        set("branches_k_color", 
            k = cluster_count, 
            dendrogram_color_scheme[1:cluster_count] ) %>% 
        set("branches_lwd", 1.2) %>%
        set("branches_lty", 1) %>%
        set("labels_colors", 
            dendrogram_color_scheme[1:cluster_count]) %>% 
        set("nodes_pch", 18)
      
      # convert the dendrogram to a ggplot
      # friendly version so we can style and add
      # labels via ggplot2 methods
      ggdend <- as.ggdend(dend)
      
      # push the color schemes over to
      # the text labels so they match up 
      # with the dendrogram segments
      centers <- inner_join(centers, 
                            ggdend$segments %>% 
                              filter(!duplicated(xend), !is.na(col)) %>% 
                              select(xend,col), by = c("cluster_id" = "xend"))
      
      # create the plot of the dendrogram
      p <- ggplot(ggdend, labels = F) +
        # add the labels at each leaf
        geom_text(data = centers, 
                  aes(label = label, x = as.numeric(cluster_id), y = -.1, 
                      size = 4, lineheight = .8, color = col), hjust = 0) +
        # flip the axes and scale to fit the text
        coord_flip() +  
        scale_y_reverse(limits = c(max(ggdend$segments$yend) + .15,
                                   min(ggdend$segments$yend) - 1.5)) + 
        scale_x_reverse(limits = c(max(ggdend$segments$xend) + .5,
                                   min(ggdend$segments$yend) + .5)) + 
        # add titling and other theme elements to make 
        # more consistent with the K-means created plot
        ggtitle(paste(cluster_count, "- Cluster Model")) +
        theme_bw() +
        theme(line = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.title.x = element_blank(),
              axis.title.y = element_blank(), 
              axis.ticks = element_blank(),
              plot.title = element_text(face = "bold", size = 20,
                                        margin = margin(0, 0, 10, 0)))
      
    } else {
      
      # create the cluster plot to help
      # the user visualize their choice
      # for the total number of clusters
      p <- ggplot(clustered_dataset(), aes(x = age, y = income,
                                           color = cluster_id)) + 
        geom_point() +
        geom_text(data = centers, aes(x = median_age,
                                      y = median_income,
                                      label = as.character(cluster_id)),
                  color = "black",
                  size = 16) +
        scale_colour_discrete(guide = FALSE) +
        scale_y_continuous(labels = dollar,
                           breaks = pretty_breaks(n = 10)) + 
        scale_x_continuous(breaks = pretty_breaks(n = 10)) + 
        xlab("Age") + 
        ylab("Income") +
        ggtitle(paste(cluster_count, "- Cluster Model")) + 
        theme_bw() + 
        theme(plot.title = element_text(face = "bold", size = 20,
                                        margin = margin(0, 0, 10, 0)), 
              axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
              axis.title.y = element_text(margin = margin(0, 10, 0, 0)))
    }
    
    # after creating the plot, print it so
    # that it is made visible to the UI
    print(p)
  })
  
  # create a dataset to display in a table 
  # and download from
  table_data <- reactive({
    table_data <- clustered_dataset() %>% 
      mutate(income_ptile_overall = round(rank(income)/length(income), 4)) %>%
      group_by(cluster_id) %>% 
      mutate(income_ptile = round(rank(income)/length(income), 4), 
             income = round(income,2)) %>%
      ungroup() %>%
      arrange(-income_ptile) %>%
      select(`First Name`, `Last Name`,
             `Email`, "Age" = age, "Income" = income,
             "Income %Tile within Cluster" = income_ptile,
             "Income %Tile Overall" = income_ptile_overall,
             "Cluster Id" = cluster_id)
    
    return(table_data)
  })
  
  # create the table using a variet of options
  # to configure the look and feel
  output$campaign_table <- DT::renderDataTable({
    
    d <- DT::datatable(table_data(), options = list(
      columnDefs = list(list(width = "150px", targets = 0),
                        list(width = "50px", targets = c(3, 7)),
                        list(width = "110px", targets = c(4, 5, 6)),
                        list(className = "dt-center", targets = 3:7)),
      deferRender = TRUE, autoWidth = FALSE, lengthChange = FALSE,
      searching = TRUE, paginate = TRUE, pageLength = 25, info = TRUE,
      ordering = TRUE,
      # sort by cluster id initially
      order = list(list(7, "asc"))
    ),
    filter = "top",
    selection = "none",
    class = "cell-border stripe",
    rownames = FALSE,
    escape = FALSE
    )
    
    d <- d %>% formatCurrency("Income", digits = 0) %>%
      formatPercentage(c("Income %Tile within Cluster", 
                         "Income %Tile Overall"), digits = 1)
    
    return(d)
  })
  
  # provide a server-side method for handling user
  # downloads from the table of data
  output$downloadDataFromTable <- downloadHandler(
    filename = function() {
      paste0("CAMPAIGN_DATA_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      write.csv(table_data()[input$campaign_table_rows_all, ], file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
