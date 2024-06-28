library(tidyverse)
library(readxl)
library(psych)

library(plotly)
library(lubridate)
library(DT)
library(ggplot2)
library(graphics)
library(base)

library(reshape2)
library(mirt)
library(lattice)
library(scales)

library(ggridges)
library(ggdist)
library(patchwork)
library(ggstatsplot)
library(cluster)
library(factoextra)
library(stats)
library(hms)
library(caret)
library(ggfortify)
library(gridExtra)
library(GGally)
library(parallelPlot)
library(seriation)
library(ineq)
library(ggthemes)
library(hrbrthemes)

library(rlang)

# Load preprocessed RDS files
# Task 3:
StudentLMKA_data <- read_rds("StudentLMKA_data.rds")
StudentLM_data <- read_rds("StudentLM_data.rds")

# Task 4:

#Shiny app starts here
library(shiny)
library(shinydashboard)
library(shinythemes)

ui <- dashboardPage(skin = 'green',
                    dashboardHeader(title = 'A Statistical Study of Singapore Vehicle Carbon Emissions', titleWidth = 800),
                    
                    dashboardSidebar(
                      sidebarMenu(menuItem('HomePage', tabName = 'HomePage'),
                                  menuItem('Task 1: Knowledge Mastery & Weak links', tabName = 'Task1'),
                                  menuItem('Task 2: Learners Profile', tabName = 'Task2'),
                                  menuItem('Task 3: Learning Mode & Knowledge Acquisition', tabName = 'Task3'),
                                  menuItem('Task 4: Question Difficulty & Learners Knowledge Level',tabName = 'Task4'),
                                  menuItem('Task 5: Recommendations', tabName = 'Task5')
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        
                        tabItem(tabName = 'HomePage',
                                fluidPage( theme = shinytheme("simplex"), 
                                         
                                           mainPanel(h3("G6 Project Title",align ='center'),
                                                     br(),
                                                     strong('Introduction'),
                                                     p('para 1'),
                                                     p('para 2'),
                                                     br(),
                                                     
                                                     strong('Objective'),
                                                     p('The objective of the study are as follows :'),
                                                     p("1) Objective 1"),
                                                     p("2) Objective 2")
                                           ) #main panel
                                ) #fluidpage
                        ), #tabitem-intro
                        
                        ###end of introduction
                        
                        
                        
                        tabItem(tabName = 'Task1',
                                fluidPage(
                                  titlePanel("Knowledge Mastery & Weak links"),

                                  mainPanel(h3("Title",align ='center'),
                                            br(),
                                            strong('Section header'),
                                            p('para 1'),
                                            p('para 2'),
                                            br(),
                                            
                                            strong('Section Header'),
                                            p('para 1'),
                                            p("1) point 1"),
                                            p("2) point 2")
                                  ) #main panel
                                  
                                )),
                        
                        tabItem(tabName = 'Task2',
                                fluidPage(
                                  titlePanel("Learners Profile"),
                                  
                                  mainPanel(h3("Title",align ='center'),
                                            br(),
                                            strong('Section header'),
                                            p('para 1'),
                                            p('para 2'),
                                            br(),
                                            
                                            strong('Section Header'),
                                            p('para 1'),
                                            p("1) point 1"),
                                            p("2) point 2")
                                  ) #main panel
                                  
                                  )),
                        
                        
                        
                        tabItem(tabName = 'Task3',
                                fluidPage(
                                  titlePanel("Learning Mode & Knowledge Acquisition"),
                                  tabsetPanel(
                                    tabPanel("Clustering Analysis for Learning Modes",
                                             
                                             #box(
                                               #radioButtons('xcol',
                                                            #label = tags$strong('Analyse By:'),
                                                            #choices = c('Vehicle Category' = 'category',
                                                                        #'Fuel Type' = 'Combined.Type'
                                                            #),
                                                            #inline = TRUE)
                                             #),

                                             #box(
                                               #selectInput("category",tags$strong("Choose an category:"), choices = c("All Category",unique(NumofVehbytype6$category)),selected = "Cars"),
                                               #selectInput("type", tags$strong("Choose a Fuel type:"), choices = c("All Fuel Type",unique(NumofVehbytype6$type)),selected = "Petrol")
                                               
                                             #),
                                             
                                             box(
                                               title = "Silhouette Analysis for Number of K-means Clusters",
                                               plotOutput("plot1", height = "400px")
                                             ),
                                             
                                             box(
                                               title = "Parallel Coordinate plot",
                                               plotOutput("plot2", height = "400px")
                                             )
                                             

                                    ),
 
                                    tabPanel("Knowledge Acquisition Distribution Across Both Clusters", 
                                             
                                             #box(
                                             #radioButtons('xcol',
                                             #label = tags$strong('Analyse By:'),
                                             #choices = c('Vehicle Category' = 'category',
                                             #'Fuel Type' = 'Combined.Type'
                                             #),
                                             #inline = TRUE)
                                             #),
                                             
                                             box(
                                             #selectInput("type", tags$strong("Choose a Fuel type:"), choices = c("All Fuel Type",unique(NumofVehbytype6$type)),selected = "Petrol"),
                                             selectInput("KAindicator1",tags$strong("Choose an indicator:"), choices = c("No. of questions answered fully or partially correct", "Overall sum of highest submission scores per question", "Overall sum of question mastery points"), selected = "Overall sum of question mastery points")
                                             ),
                                             
                                             box(
                                               title = "Ridgeline Plot of Distribution of Both Clusters",
                                               plotOutput("plot3", height = "400px")
                                             ),
                                             
                                             box(
                                               title = "Ridgeline Plot of Distribution by Knowledge Areas for Both Clusters",
                                               plotOutput("plot4", height = "400px")
                                             ),
                                             
                                    ),
                                                                       
                                    tabPanel("2-Sample Mean Statistical Test For Both Clusters",
                                             
                                             #box(
                                             #radioButtons('xcol',
                                             #label = tags$strong('Analyse By:'),
                                             #choices = c('Vehicle Category' = 'category',
                                             #'Fuel Type' = 'Combined.Type'
                                             #),
                                             #inline = TRUE)
                                             #),
                                             
                                             box(
                                               #selectInput("type", tags$strong("Choose a Fuel type:"), choices = c("All Fuel Type",unique(NumofVehbytype6$type)),selected = "Petrol"),
                                               selectInput("KAindicator2",tags$strong("Choose an indicator:"), choices = c("Percent of submissions absolutely correct", "Overall sum of highest submission scores per question", "Overall sum of question mastery points"), selected = "Overall sum of question mastery points")
                                               ),

                                             box(
                                               title = "2-Sample Difference in Mean Statistical Test for Both Clusters",
                                               plotOutput("plot5", height = "400px")
                                             ),
                                             
                                             #box(
                                               #title = "Silhouette Analysis for Number of K-means Clusters",
                                               #plotOutput("plot1", height = "400px")
                                             #),
                                             
                                             #box(
                                               #title = "Parallel Coordinate plot",
                                               #plotOutput("plot2", height = "400px")
                                             #)
                                             
                                    ),
                                    
                                    tabPanel("Multi-linear Regression (Alternative from Clustering)",
                                             
                                             #box(
                                             #radioButtons('xcol',
                                             #label = tags$strong('Analyse By:'),
                                             #choices = c('Vehicle Category' = 'category',
                                             #'Fuel Type' = 'Combined.Type'
                                             #),
                                             #inline = TRUE)
                                             #),
                                             
                                             box(
                                               #selectInput("type", tags$strong("Choose a Fuel type:"), choices = c("All Fuel Type",unique(NumofVehbytype6$type)),selected = "Petrol"),
                                               selectInput("KAindicator3",tags$strong("Choose an indicator:"), choices = c("Overall sum of highest submission scores per question", "Overall sum of question mastery points"), selected = "Overall sum of question mastery points")
                                               ),
                                             
                                             box(
                                               title = "Multi-linear Regression of learning mode features (Overall)",
                                               plotOutput("plot6", height = "400px")
                                             ),
                                             
                                             box(
                                               title = "Multi-linear Regression of learning mode features (By Knowledge Areas)",
                                               plotOutput("plot7", height = "400px")
                                             )
                                             
                                             
                                    ),
                                    
                                  )
                                )
                                ),
                        
                        tabItem(tabName = 'Task4',
                                fluidPage(
                                  titlePanel("Question Difficulty & Learners Knowledge Level"),
                                  
                                  mainPanel(h3("Title",align ='center'),
                                            br(),
                                            strong('Section header'),
                                            p('para 1'),
                                            p('para 2'),
                                            br(),
                                            
                                            strong('Section Header'),
                                            p('para 1'),
                                            p("1) point 1"),
                                            p("2) point 2")
                                  ) #main panel
                                  
                                )),
                        
                        tabItem(tabName = 'Task5',
                                fluidPage(
                                  titlePanel("Recommendations"),
                                  
                                  mainPanel(h3("Title",align ='center'),
                                            br(),
                                            strong('Section header'),
                                            p('para 1'),
                                            p('para 2'),
                                            br(),
                                            
                                            strong('Section Header'),
                                            p('para 1'),
                                            p("1) point 1"),
                                            p("2) point 2")
                                  ) #main panel
                                  
                                ))
                        )
                        )
                        )

server <- function(input, output, session) { 

  output$plot1 <- renderPlot({
    
    # Filter Non-clustering features
    clustering_data <- StudentLM_data %>%
      select(-student_ID)
    
    # Function to compute silhouette widths
    silhouette_analysis <- function(data, max_clusters) {
      avg_sil_widths <- numeric(max_clusters)
      
      for (k in 2:max_clusters) {
        # Perform k-means clustering
        kmeans_result <- kmeans(data, centers = k, nstart = 25)
        
        # Compute silhouette widths
        sil <- silhouette(kmeans_result$cluster, dist(data))
        
        # Calculate average silhouette width
        avg_sil_widths[k] <- mean(sil[, 3])
      }
      
      return(avg_sil_widths)
    }
    
    # Determine the maximum number of clusters to test
    max_clusters <- 12
    
    # Perform silhouette analysis
    avg_sil_widths <- silhouette_analysis(clustering_data, max_clusters)
    
    # Plot the average silhouette widths
    plot(1:max_clusters, avg_sil_widths, type = "b", pch = 19, frame = FALSE,
         xlab = "Number of clusters", ylab = "Average silhouette width",
         main = "Silhouette Analysis for Determining Optimal Number of Clusters")
    
    # Highlight the optimal number of clusters
    optimal_clusters <- which.max(avg_sil_widths)
    points(optimal_clusters, avg_sil_widths[optimal_clusters], col = "red", pch = 19)
    
  })  

  output$plot2 <- renderPlot({
    
    # Filter Non-clustering features
    clustering_data <- StudentLM_data %>%
      select(-student_ID)
    
    # Standardize the data
    clustering_data_scaled <- scale(clustering_data)
    
    # Perform k-means clustering
    set.seed(123)  # For reproducibility
    kmeans_result <- kmeans(clustering_data_scaled, centers = 2, nstart = 25)
    
    # Add the cluster assignments to the original data
    StudentLM_data$cluster <- kmeans_result$cluster
    
    # Ensure cluster in factor format
    StudentLM_data_factor <- StudentLM_data
    StudentLM_data_factor$cluster <- as.character(StudentLM_data_factor$cluster)
    
    # Plot facet Parallel coordinates plot
    ggparcoord(data = StudentLM_data_factor,
               columns = c(2:13), 
               groupColumn = 14,
               scale = "uniminmax",
               alphaLines = 0.2,
               boxplot = TRUE, 
               title = "Parallel Coordinates Split Plot of Students' learning modes")+
      facet_wrap(~ cluster)+
      theme(
        plot.title = element_text(size = 10),
        axis.text.x = element_text(angle = 30, hjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)
      )
    
  })
      
  output$plot3 <- renderPlot({

    StudentLMKA_data$cluster <- as.factor(StudentLMKA_data$cluster)
    
    # Define the dynamic variable based on the input selection
    KAindicator01 <- NULL
    if (input$KAindicator1 == "No. of questions answered fully or partially correct") {
      KAindicator01 <- "`No. of questions answered fully or partially correct`"
    } else if (input$KAindicator1 == "Overall sum of highest submission scores per question") {
      KAindicator01 <- "`Sum of overall highest submission scores`"
    } else if (input$KAindicator1 == "Overall sum of question mastery points") {
      KAindicator01 <- "`Sum of points Overall`"
    }
    
    # Generate a ridgeline chart using ggplot
    ggplot(StudentLMKA_data, 
           aes_string(x = KAindicator01, 
                      y = "cluster",
                      fill = "factor(stat(quantile))"
           )) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE, 
        quantiles = 4,
        quantile_lines = TRUE) +
      scale_fill_viridis_d(name = "Quartiles") +
      theme_ridges() 
  })
  
  output$plot4 <- renderPlot({
    
    StudentLMKA_data$cluster <- as.factor(StudentLMKA_data$cluster)
    
    # Define the dynamic variable based on the input selection
    #KAindicator01 <- NULL
    #if (input$KAindicator1 == "No. of questions answered fully or partially correct") {
    #  KAindicator01 <- "`Sum of overall highest submission scores for`"
    #} else if (input$KAindicator1 == "Overall sum of highest submission scores per question") {
    #  KAindicator01 <- "`Sum of overall highest submission scores for`"
    #} else if (input$KAindicator1 == "Overall sum of question mastery points") {
    #  KAindicator01 <- "`Sum of points for`"
    #}
    
    # Generate a Combined Ridgeline chart using ggplot
    a <- 
    ggplot(StudentLMKA_data, 
           aes(x = `Sum of points for b3C9s knowledge`,
               y = cluster,
               fill = factor(stat(quantile))
           )) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE, 
        quantiles = 4,
        quantile_lines = TRUE) +
      scale_fill_viridis_d(name = "Quartiles") +
      theme_ridges()
    
    b <- 
    ggplot(StudentLMKA_data, 
           aes(x = `Sum of points for g7R2j knowledge`,
               y = cluster,
               fill = factor(stat(quantile))
           )) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE, 
        quantiles = 4,
        quantile_lines = TRUE) +
      scale_fill_viridis_d(name = "Quartiles") +
      theme_ridges()
    c <- 
    ggplot(StudentLMKA_data, 
           aes(x = `Sum of points for m3D1v knowledge`, 
               y = cluster,
               fill = factor(stat(quantile))
           )) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE, 
        quantiles = 4,
        quantile_lines = TRUE) +
      scale_fill_viridis_d(name = "Quartiles") +
      theme_ridges()
    d <- 
    ggplot(StudentLMKA_data, 
           aes(x = `Sum of points for r8S3g knowledge`, 
               y = cluster,
               fill = factor(stat(quantile))
           )) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE, 
        quantiles = 4,
        quantile_lines = TRUE) +
      scale_fill_viridis_d(name = "Quartiles") +
      theme_ridges()
    e <- 
    ggplot(StudentLMKA_data, 
           aes(x = `Sum of points for t5V9e knowledge`, 
               y = cluster,
               fill = factor(stat(quantile))
           )) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE, 
        quantiles = 4,
        quantile_lines = TRUE) +
      scale_fill_viridis_d(name = "Quartiles") +
      theme_ridges()
    f <- 
    ggplot(StudentLMKA_data, 
           aes(x = `Sum of points for y9W5d knowledge`, 
               y = cluster,
               fill = factor(stat(quantile))
           )) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE, 
        quantiles = 4,
        quantile_lines = TRUE) +
      scale_fill_viridis_d(name = "Quartiles") +
      theme_ridges()
    g <- 
    ggplot(StudentLMKA_data, 
           aes(x = `Sum of points for k4W1c knowledge`, 
               y = cluster,
               fill = factor(stat(quantile))
           )) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE, 
        quantiles = 4,
        quantile_lines = TRUE) +
      scale_fill_viridis_d(name = "Quartiles") +
      theme_ridges()
    h <- 
    ggplot(StudentLMKA_data, 
           aes(x = `Sum of points for s8Y2f knowledge`, 
               y = cluster,
               fill = factor(stat(quantile))
           )) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE, 
        quantiles = 4,
        quantile_lines = TRUE) +
      scale_fill_viridis_d(name = "Quartiles") +
      theme_ridges()
    
    (a + b) / (c + d) / (e + f) / (g + h)
  })

  output$plot5 <- renderPlot({
    
    # Define the dynamic variable based on the input selection
    KAindicator02 <- NULL
    if (input$KAindicator2 == "Percent of submissions absolutely correct") {
      KAindicator02 <- "Percent of submissions absolutely correct"
    } else if (input$KAindicator2 == "Overall sum of highest submission scores per question") {
      KAindicator02 <- "Sum of overall highest submission scores"
    } else if (input$KAindicator2 == "Overall sum of question mastery points") {
      KAindicator02 <- "Sum of points Overall"
    }
    
    ggbetweenstats(
      data = StudentLMKA_data,
      x = "cluster", 
      y = !!sym(KAindicator02),  # Use !!sym() to evaluate the variable name correctly
      type = "np",
      messages = FALSE
    )
  })
  
  output$plot6 <- renderPlot({
    
  # Define the dynamic variable based on the input selection
  KAindicator03 <- NULL
  if (input$KAindicator3 == "Overall sum of highest submission scores per question") {
    KAindicator03 <- "Sum of overall highest submission scores"
  } else {
    KAindicator03 <- "Sum of points Overall"
  }
  
  # Fit the multi-linear regression model
  model <- lm(get(KAindicator03) ~  
                 `Percent of submissions on weekdays`+
                 `Percent of submissions during working hrs`+
                 `Total no. of different qns_attempted`+
                 `Gini Index for qns in submission`+
                 `Mean selected question scores`+
                 `Mean submission memory size by qns`+
                 `Mean timeconsume by qns`+
                 `Total no. of submissions`+
                 `Mean no. of different answering methods per qns`+
                 `Gini index for answering methods used per qns`+
                 `Total memory size of submissions`+
                 `Total timeconsume of submissions`, data = StudentLMKA_data)
  
  # Plot coefficients
  ggcoefstats(model, 
              output = "plot") +
    theme(
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)
    )
    
    
  })
  
  output$plot7 <- renderPlot({
    
    model3 <- lm(`Sum of points for b3C9s knowledge` ~  
                   `Percent of submissions on weekdays`+
                   `Percent of submissions during working hrs`+
                   `Total no. of different qns_attempted`+
                   `Gini Index for qns in submission`+
                   `Mean selected question scores`+
                   `Mean submission memory size by qns`+
                   `Mean timeconsume by qns`+
                   `Total no. of submissions`+
                   `Mean no. of different answering methods per qns`+
                   `Gini index for answering methods used per qns`+
                   `Total memory size of submissions`+
                   `Total timeconsume of submissions`, data = StudentLMKA_data)
    model4 <- lm(`Sum of points for g7R2j knowledge` ~ 
                   `Percent of submissions on weekdays`+
                   `Percent of submissions during working hrs`+
                   `Total no. of different qns_attempted`+
                   `Gini Index for qns in submission`+
                   `Mean selected question scores`+
                   `Mean submission memory size by qns`+
                   `Mean timeconsume by qns`+
                   `Total no. of submissions`+
                   `Mean no. of different answering methods per qns`+
                   `Gini index for answering methods used per qns`+
                   `Total memory size of submissions`+
                   `Total timeconsume of submissions`, data = StudentLMKA_data)
    model5 <- lm(`Sum of points for k4W1c knowledge` ~  
                   `Percent of submissions on weekdays`+
                   `Percent of submissions during working hrs`+
                   `Total no. of different qns_attempted`+
                   `Gini Index for qns in submission`+
                   `Mean selected question scores`+
                   `Mean submission memory size by qns`+
                   `Mean timeconsume by qns`+
                   `Total no. of submissions`+
                   `Mean no. of different answering methods per qns`+
                   `Gini index for answering methods used per qns`+
                   `Total memory size of submissions`+
                   `Total timeconsume of submissions`, data = StudentLMKA_data)
    model6 <- lm(`Sum of points for m3D1v knowledge` ~  
                   `Percent of submissions on weekdays`+
                   `Percent of submissions during working hrs`+
                   `Total no. of different qns_attempted`+
                   `Gini Index for qns in submission`+
                   `Mean selected question scores`+
                   `Mean submission memory size by qns`+
                   `Mean timeconsume by qns`+
                   `Total no. of submissions`+
                   `Mean no. of different answering methods per qns`+
                   `Gini index for answering methods used per qns`+
                   `Total memory size of submissions`+
                   `Total timeconsume of submissions`, data = StudentLMKA_data)
    model7 <- lm(`Sum of points for r8S3g knowledge` ~ 
                   `Percent of submissions on weekdays`+
                   `Percent of submissions during working hrs`+
                   `Total no. of different qns_attempted`+
                   `Gini Index for qns in submission`+
                   `Mean selected question scores`+
                   `Mean submission memory size by qns`+
                   `Mean timeconsume by qns`+
                   `Total no. of submissions`+
                   `Mean no. of different answering methods per qns`+
                   `Gini index for answering methods used per qns`+
                   `Total memory size of submissions`+
                   `Total timeconsume of submissions`, data = StudentLMKA_data)
    model8 <- lm(`Sum of points for s8Y2f knowledge` ~  
                   `Percent of submissions on weekdays`+
                   `Percent of submissions during working hrs`+
                   `Total no. of different qns_attempted`+
                   `Gini Index for qns in submission`+
                   `Mean selected question scores`+
                   `Mean submission memory size by qns`+
                   `Mean timeconsume by qns`+
                   `Total no. of submissions`+
                   `Mean no. of different answering methods per qns`+
                   `Gini index for answering methods used per qns`+
                   `Total memory size of submissions`+
                   `Total timeconsume of submissions`, data = StudentLMKA_data)
    model9 <- lm(`Sum of points for t5V9e knowledge` ~  
                   `Percent of submissions on weekdays`+
                   `Percent of submissions during working hrs`+
                   `Total no. of different qns_attempted`+
                   `Gini Index for qns in submission`+
                   `Mean selected question scores`+
                   `Mean submission memory size by qns`+
                   `Mean timeconsume by qns`+
                   `Total no. of submissions`+
                   `Mean no. of different answering methods per qns`+
                   `Gini index for answering methods used per qns`+
                   `Total memory size of submissions`+
                   `Total timeconsume of submissions`, data = StudentLMKA_data)
    model10 <- lm(`Sum of points for y9W5d knowledge` ~ 
                    `Percent of submissions on weekdays`+
                    `Percent of submissions during working hrs`+
                    `Total no. of different qns_attempted`+
                    `Gini Index for qns in submission`+
                    `Mean selected question scores`+
                    `Mean submission memory size by qns`+
                    `Mean timeconsume by qns`+
                    `Total no. of submissions`+
                    `Mean no. of different answering methods per qns`+
                    `Gini index for answering methods used per qns`+
                    `Total memory size of submissions`+
                    `Total timeconsume of submissions`, data = StudentLMKA_data)
    
    a <- 
    ggcoefstats(model3, 
                output = "plot")
    b <- 
    ggcoefstats(model4, 
                output = "plot")
    c <- 
    ggcoefstats(model5, 
                output = "plot")
    d <- 
    ggcoefstats(model6, 
                output = "plot")
    e <- 
    ggcoefstats(model7, 
                output = "plot")
    f <- 
    ggcoefstats(model8, 
                output = "plot")
    g <- 
    ggcoefstats(model9, 
                output = "plot")
    h <- 
    ggcoefstats(model10, 
                output = "plot")
    (a + b) / (c + d) / (e + f) / (g + h) 
    
  })
  

  }

shinyApp(ui, server)
