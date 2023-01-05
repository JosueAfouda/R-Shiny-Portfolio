# Application pas encore finie

library(shiny)
library(DT)
#library(plotly)
#library(tidyverse)
library(pROC)
library(ROSE)
library(ggplot2)
library(ggthemes)
theme_set(theme_minimal())
library(ggplotify)
#library(grid)

seed <- 131

set.seed(seed)

#################################################################################

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

#######################################################################################

# Importation des données

url = 'https://github.com/JosueAfouda/Credit-Risk-Modeling/raw/master/data_credit.txt'

df <- read.csv(url)

df$loan_status <- as.factor(df$loan_status)

df_clean <- df

index_outlier_income <- which(df_clean$person_income < quantile(df_clean$person_income, 0.25) - 1.5 * IQR(df_clean$person_income) | df_clean$person_income > quantile(df_clean$person_income, 0.75) + 1.5 * IQR(df_clean$person_income))

# Suppression des valeurs aberantes au niveau de la variable 'person_income'

df_clean <- df_clean[-index_outlier_income, ]

# Suppression des valeurs aberrantes au niveau de la variabe 'person_age'

df_clean <- subset(df_clean, df_clean$person_age < 100)

index_NA_person_emp_length <- which(is.na(df_clean$person_emp_length))

df_clean$person_emp_length[index_NA_person_emp_length] <- median(df_clean$person_emp_length, na.rm=TRUE)

# Variable loan_int_rate

index_NA_rate <- which(is.na(df_clean$loan_int_rate))

df_clean$loan_int_rate[index_NA_rate] <- median(df_clean$loan_int_rate, na.rm=TRUE)

df_clean2 <- df_clean

normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}

# Normalisation des données 
#réaprtir les valeurs entre 0 et 1 tout en gardant les distributions originales

for (col in colnames(df_clean2)) {
    if ((class(df_clean2[, col]) != 'factor') & (class(df_clean2[, col]) != 'character')) {
        df_clean2[, col] <- normalize(df_clean2[, col])
    }
}

# Standardisation

df_clean3 <- df_clean

for (col in colnames(df_clean3)) {
    if ((class(df_clean3[, col]) != 'factor') & (class(df_clean3[, col]) != 'character')) {
        df_clean3[, col] <- scale(df_clean3[, col])
    }
}


# User Interface (frontend)
ui <- fluidPage(

    # Application title
    titlePanel("Credit Scoring with Logistic Regression"),

    # Sidebar with a slider input for number of bins 
    tabsetPanel(
        tabPanel('DATA', DTOutput('table')),
        
        tabPanel('EDA', 
                 tabsetPanel(tabPanel('Statistics', verbatimTextOutput("stats")), 
                             tabPanel('Summary', verbatimTextOutput("summary")),
                             tabPanel('DataViz',
                                      plotOutput('univariate', height = 750))
                             )),
        
        tabPanel('Data Preparation',
                 tabsetPanel(tabPanel('Stand/Norm',
                                      selectInput('stand_norm', 
                                                  'Standardization or Normalization:', 
                                                  choices = c('Standardization', 'Normalization')),
                                      DTOutput('table_scaled')),
                             
                             tabPanel('Stats on Scaled Data', verbatimTextOutput("scaled_stats")),
                             
                             tabPanel('Train set',
                                      sliderInput('train_pct', 'Train Percentage:', 0.8, min = 0.05, max = 1.0),
                                      DTOutput('train')),
                             
                             tabPanel('Test set', DTOutput('test')),
                             
                             tabPanel('Splits Target', plotOutput("train_stats"), 
                                      plotOutput("test_stats")),
                             
                             tabPanel('Resampling of Train set', 
                                      selectInput('resampling', 
                                                  'Select a Resampling Method:',
                                                  c("Random Over Sampling (ROS)",
                                                    "Random Under Sampling (RUS)",
                                                    "Combining of ROS and RUS",
                                                    "None")),
                                      DTOutput('train_balanced')),
                             
                             tabPanel('Balanced classes', plotOutput(('balanced')))))
    )
)

# Server (backend)
server <- function(input, output) {

    output$table <- renderDT({
        df
    })
    
    output$stats <- renderPrint({
        summary(df)
    })
    
    output$summary <- renderPrint({
        str(df)
    })
    
    output$univariate <- renderPlot({
        
        #par(mfrow = c(4, 4))  # 4 rows and 4 columns
        
        #a <- as.ggplot(~plot(df$loan_status, main = "Loan Status"))
        
        b <- ggplot(df, aes(x = "", y=loan_percent_income, fill = loan_status)) + 
                geom_boxplot() +
                labs(x = "", y = "loan_percent_income")
        
        
        c <- ggplot(df, aes(x = "", y=loan_amnt, fill = loan_status)) +
                geom_boxplot() +
                labs(x = "", y = "loan_amnt")
        
        #d <- as.ggplot(~hist(df['loan_amnt'], main = "Montant du crédit"))
        
        e <- ggplot(subset(df, df$person_income < 100000), aes(x = "", y=person_income, fill = loan_status)) +
                geom_boxplot() +
                labs(x = "", y = "person_income")
        
        f <- ggplot(df, aes(x = "", y=loan_int_rate, fill = loan_status)) +
                geom_boxplot() +
                labs(x = "", y = "loan_int_rate")
        
        g <- ggplot(df, aes(x=person_home_ownership, fill = person_home_ownership)) +
                geom_bar() +
                theme_bw()
        
        h <- ggplot(df, aes(x=loan_intent, fill = loan_intent)) + 
                geom_bar() +
                theme_bw()
        
        multiplot(b, c, e, f, g, h, cols = 3)
    })
    
    df_prepared <- reactive({
        if (input$stand_norm == 'Standardization') {
            df_clean3
        } else if (input$stand_norm == 'Normalization') {
            df_clean2
        }
    })
    
    output$table_scaled <- renderDT({
        df_prepared()
    })
    
    output$scaled_stats <- renderPrint({
        summary(df_prepared())
    })
    
    index_train <- reactive({
        sample(1:nrow(df_prepared()), input$train_pct * nrow(df_prepared()))
    })
    
    train_set <- reactive({
        df_prepared()[index_train(), ]
    })
    
    test_set <- reactive({
        df_prepared()[-index_train(), ]
    })
    
    output$train <- renderDT({
        train_set()
    })
    
    output$test <- renderDT({
        test_set()
    })
    
    output$train_stats <- renderPlot({
        plot(train_set()['loan_status'], main = "Loan Status on Train set")
    })
    
    output$test_stats <- renderPlot({
        plot(test_set()['loan_status'], main = "Loan Status on Test set")
    })
    
    
    balanced_df <- reactive({
        if (input$resampling == 'Random Over Sampling (ROS)') {
            over <- ovun.sample(formula = loan_status ~ ., 
                                data = train_set(), 
                                method = 'over', 
                                seed = seed)
            over$data
        } else if (input$resampling == 'Random Under Sampling (ROS)') {
            under <- ovun.sample(formula = loan_status ~ ., 
                                 data = train_set(), 
                                 method = 'under', 
                                 seed = seed)
            under$data
        } else if (input$resampling == 'Combining of ROS and RUS') {
            both <- ovun.sample(formula = loan_status ~ ., 
                                data = train_set(), 
                                method = 'both', 
                                seed = seed)
            both$data
        } else if (input$resampling == 'None') {
            train_set()
        }
    })
    
    
    output$train_balanced <- renderDT({
        balanced_df()
    })
    
    output$balanced <- renderPlot({
        plot(balanced_df()['loan_status'], main = 'Balanced Classes')
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
