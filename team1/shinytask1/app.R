library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(dplyr)
library(ggplot2)

# Load your data
merged_data <- readRDS("data/merged_data.RDS")
knowledge_expanded <- readRDS("data/knowledge_expanded.RDS")
mastery_scores <- readRDS("data/mastery_scores.RDS")
adjusted_scores <- readRDS("data/adjusted_scores.RDS")
knowledge_mastery <- readRDS("data/knowledge_mastery.RDS")

# Ensure 'actual_score' and 'question_score' are numeric
knowledge_expanded <- knowledge_expanded %>%
  mutate(actual_score = as.numeric(actual_score),
         question_score = as.numeric(question_score))

# Shiny app UI
ui <- dashboardPage(
  skin = 'blue',
  dashboardHeader(title = 'VISUALISING LEARNING EFFECTIVENESS FOR INSIGHTS ON NORTHCLASS INSTITUTE’S EDUCATION SYSTEM', titleWidth = 800),
  dashboardSidebar(
    sidebarMenu(
      menuItem('HomePage', tabName = 'HomePage'),
      menuItem('Task 1: Knowledge Mastery & Weak links', tabName = 'Task1'),
      menuItem('Task 2: Learners Profile', tabName = 'Task2'),
      menuItem('Task 3: Learning Mode & Knowledge Acquisition', tabName = 'Task3'),
      menuItem('Task 4: Question Difficulty & Learners Knowledge Level', tabName = 'Task4'),
      menuItem('Task 5: Recommendations', tabName = 'Task5')
    )
  ),
  dashboardBody(
    tabItems(
      # HomePage
      tabItem(tabName = 'HomePage',
              fluidPage(theme = shinytheme("simplex"),
                        mainPanel(h3("G6 Project Title", align ='center'),
                                  br(),
                                  strong('Introduction'),
                                  p('para 1'),
                                  p('para 2'),
                                  br(),
                                  strong('Objective'),
                                  p('The objective of the study are as follows :'),
                                  p("1) Objective 1"),
                                  p("2) Objective 2")
                        ) # main panel
              ) # fluid page
      ), # tab item - intro
      
      # Task 1
      tabItem(tabName = 'Task1',
              fluidPage(
                titlePanel("Task1: Knowledge Mastery and Weak Links"),
                tabsetPanel(
                  tabPanel("Weak Links by Questions",
                           selectInput(
                             'questionMetric',
                             tags$strong('Choose Metric:'),
                             choices = c(
                               'Normalized Average Highest Score' = 'NormAvgHighestScore',
                               'Non-normalized Average Highest Score' = 'NonNormAvgHighestScore',
                               'Average Methods Applied on Questions' = 'AvgMethodsApplied'
                             )
                           ),
                           box(
                             width = 12, height = 500, solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE,
                             plotlyOutput('questionPlot', height = 400)
                           )
                  ),
                  tabPanel("Weak Links by Knowledge Area",
                           selectInput(
                             'knowledgeMetric',
                             tags$strong('Choose Metric:'),
                             choices = c(
                               'Total Points on Each Knowledge Area' = 'TotalPointsKnowledge',
                               'Mastery Points for Knowledge Area' = 'MasteryPointsKnowledge',
                               'Mastery Points for Sub Knowledge Area' = 'MasteryPointsSubKnowledge'
                             )
                           ),
                           box(
                             width = 12, height = 500, solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE,
                             plotlyOutput('knowledgePlot', height = 400)
                           )
                  )
                )
              )
      ),
      # Task 2
      tabItem(tabName = 'Task2',
              fluidPage(
                titlePanel("Task2: Learners Profile"),
                # Add your UI components for Task 2 here
              )
      ),
      # Task 3
      tabItem(tabName = 'Task3',
              fluidPage(
                titlePanel("Task3: Learning Mode & Knowledge Acquisition"),
                tabsetPanel(
                  tabPanel("Clustering Analysis for Learning Modes",
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
                           selectInput(
                             "KAindicator1", tags$strong("Choose an indicator:"),
                             choices = c(
                               "No. of questions answered fully or partially correct", 
                               "Overall sum of highest submission scores per question", 
                               "Overall sum of question mastery points"
                             ), 
                             selected = "Overall sum of question mastery points"
                           ),
                           box(
                             title = "Ridgeline Plot of Distribution of Both Clusters",
                             plotOutput("plot3", height = "400px")
                           ),
                           box(
                             title = "Ridgeline Plot of Distribution by Knowledge Areas for Both Clusters",
                             plotOutput("plot4", height = "400px")
                           )
                  ),
                  tabPanel("2-Sample Mean Statistical Test For Both Clusters",
                           selectInput(
                             "KAindicator2", tags$strong("Choose an indicator:"),
                             choices = c(
                               "Percent of submissions absolutely correct", 
                               "Overall sum of highest submission scores per question", 
                               "Overall sum of question mastery points"
                             ), 
                             selected = "Overall sum of question mastery points"
                           ),
                           box(
                             title = "2-Sample Difference in Mean Statistical Test for Both Clusters",
                             plotOutput("plot5", height = "400px")
                           )
                  ),
                  tabPanel("Multi-linear Regression (Alternative from Clustering)",
                           selectInput(
                             "KAindicator3", tags$strong("Choose an indicator:"),
                             choices = c(
                               "Overall sum of highest submission scores per question", 
                               "Overall sum of question mastery points"
                             ), 
                             selected = "Overall sum of question mastery points"
                           ),
                           box(
                             title = "Multi-linear Regression of learning mode features (Overall)",
                             plotOutput("plot6", height = "400px")
                           ),
                           box(
                             title = "Multi-linear Regression of learning mode features (By Knowledge Areas)",
                             plotOutput("plot7", height = "400px")
                           )
                  )
                )
              )
      ),
      # Task 4
      tabItem(tabName = 'Task4',
              fluidPage(
                titlePanel("Task4: Question Difficulty & Learners Knowledge Level"),
                # Add your UI components for Task 4 here
              )
      ),
      # Task 5
      tabItem(tabName = 'Task5',
              fluidPage(
                titlePanel("Task5: Recommendations"),
                # Add your UI components for Task 5 here
              )
      )
    )
  )
)


# Shiny app server
server <- function(input, output) {
  # Reactive expression to dynamically choose the plot based on the selected metric for questions
  output$questionPlot <- renderPlotly({
    if (input$questionMetric == 'NormAvgHighestScore') {
      # Normalized Average Highest Score plot code
      highest_scores <- knowledge_expanded %>%
        group_by(student_ID, title_ID, knowledge) %>%
        summarise(highest_actual_score = max(actual_score, na.rm = TRUE) / question_score, .groups = 'drop')
      
      average_highest_scores <- highest_scores %>%
        group_by(title_ID, knowledge) %>%
        summarise(average_highest_score = mean(highest_actual_score, na.rm = TRUE), .groups = 'drop')
      
      average_highest_scores <- average_highest_scores %>%
        left_join(knowledge_expanded %>% select(title_ID, question_score) %>% distinct(), by = "title_ID")
      
      average_highest_scores <- average_highest_scores %>%
        mutate(knowledge = as.factor(knowledge))
      
      color_scale_limits <- range(average_highest_scores$average_highest_score, na.rm = TRUE)
      
      p_heatmap <- ggplot(average_highest_scores, aes(x = knowledge, y = title_ID, fill = average_highest_score,
                                                      text = paste("Avg Highest Score:", round(average_highest_score, 2),
                                                                   "<br>Question Score:", question_score))) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "blue", mid = "green", high = "red", midpoint = 0.9, 
                             limits = color_scale_limits, name = "Avg Highest Score") +
        labs(title = "Normalised Average Highest Actual Score per Knowledge Area per Question",
             x = "Knowledge Areas",
             y = "Question IDs",
             fill = "Avg Highest Score") +
        theme_minimal(base_size = 15) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 6),
          axis.title.x = element_text(size = 10, margin = margin(t = 15)),
          axis.title.y = element_text(size = 10, margin = margin(r = 15)),
          plot.title = element_text(size = 8, face = "bold", margin = margin(b = 15)),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8),
          legend.key.size = unit(1, "cm"),
          legend.position = "right"
        )
      
      ggplotly(p_heatmap, tooltip = "text")
    } else if (input$questionMetric == 'NonNormAvgHighestScore') {
      # Non-normalized Average Highest Score plot code
      highest_scores <- knowledge_expanded %>%
        group_by(student_ID, title_ID, knowledge) %>%
        summarise(highest_actual_score = max(actual_score, na.rm = TRUE), .groups = 'drop')
      
      average_highest_scores <- highest_scores %>%
        group_by(title_ID, knowledge) %>%
        summarise(average_highest_score = mean(highest_actual_score, na.rm = TRUE), .groups = 'drop')
      
      average_highest_scores <- average_highest_scores %>%
        left_join(knowledge_expanded %>% select(title_ID, question_score) %>% distinct(), by = "title_ID")
      
      average_highest_scores <- average_highest_scores %>%
        mutate(knowledge = as.factor(knowledge))
      
      color_scale_limits <- range(average_highest_scores$average_highest_score, na.rm = TRUE)
      
      p_heatmap <- ggplot(average_highest_scores, aes(x = knowledge, y = title_ID, fill = average_highest_score,
                                                      text = paste("Avg Highest Score:", round(average_highest_score, 2),
                                                                   "<br>Question Score:", question_score))) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "blue", mid = "green", high = "red", midpoint = 2.5, 
                             limits = color_scale_limits, name = "Avg Highest Score") +
        labs(title = "Non-normalised Average Highest Actual Score per Knowledge Area per Question",
             x = "Knowledge Areas",
             y = "Question IDs",
             fill = "Avg Highest Score") +
        theme_minimal(base_size = 15) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 6),
          axis.title.x = element_text(size = 10, margin = margin(t = 15)),
          axis.title.y = element_text(size = 10, margin = margin(r = 15)),
          plot.title = element_text(size = 8, face = "bold", margin = margin(b = 15)),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8),
          legend.key.size = unit(1, "cm"),
          legend.position = "right"
        )
      
      ggplotly(p_heatmap, tooltip = "text")
    } else if (input$questionMetric == 'AvgMethodsApplied') {
      # Average Methods Applied on Questions plot code
      mastery_scores <- mastery_scores %>%
        mutate(unique_methods = as.numeric(unique_methods))
      
      method_counts <- mastery_scores %>%
        group_by(student_ID, title_ID, knowledge) %>%
        summarise(avg_methods = mean(unique_methods, na.rm = TRUE), .groups = 'drop')
      
      method_counts_summary <- method_counts %>%
        group_by(title_ID, knowledge) %>%
        summarise(avg_methods = mean(avg_methods, na.rm = TRUE), .groups = 'drop')
      
      method_counts_summary <- method_counts_summary %>%
        mutate(knowledge = as.factor(knowledge))
      
      p_heatmap <- ggplot(method_counts_summary, aes(x = knowledge, y = title_ID, fill = avg_methods,
                                                     text = paste("Knowledge Area:", knowledge,
                                                                  "<br>Title ID:", title_ID,
                                                                  "<br>Avg Methods:", round(avg_methods, 2)))) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "white", high = "blue", mid = "green", midpoint = mean(method_counts_summary$avg_methods, na.rm = TRUE), name = "Avg Methods") +
        labs(title = "Average Methods used per Knowledge Area per Question",
             x = "Knowledge Areas",
             y = "Question IDs",
             fill = "Avg Methods") +
        theme_minimal(base_size = 15) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 8),
          axis.title.x = element_text(size = 10, margin = margin(t = 20)),
          axis.title.y = element_text(size = 10, margin = margin(r = 20)),
          plot.title = element_text(size = 8, face = "bold", margin = margin(b = 20)),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8),
          legend.key.size = unit(1, "cm"),
          legend.position = "right"
        )
      
      ggplotly(p_heatmap, tooltip = "text")
    }
  })
  
  # Reactive expression to dynamically choose the plot based on the selected metric for knowledge areas
  output$knowledgePlot <- renderPlotly({
    if (input$knowledgeMetric == 'TotalPointsKnowledge') {
      # Total Points on Each Knowledge Area plot code
      adjusted_scores <- adjusted_scores %>%
        mutate(points = as.numeric(points))
      
      adjusted_scores <- adjusted_scores %>%
        group_by(student_ID, title_ID, knowledge) %>%
        mutate(attempts = n()) %>%
        ungroup()
      
      total_points_attempts <- adjusted_scores %>%
        group_by(student_ID, title_ID, knowledge) %>%
        summarise(total_points_sum = sum(points, na.rm = TRUE),
                  total_attempts = sum(attempts, na.rm = TRUE), .groups = 'drop')
      
      total_summary <- total_points_attempts %>%
        group_by(title_ID, knowledge) %>%
        summarise(total_points_sum = sum(total_points_sum, na.rm = TRUE),
                  total_attempts = sum(total_attempts, na.rm = TRUE), .groups = 'drop')
      
      total_summary <- total_summary %>%
        mutate(knowledge = as.factor(knowledge))
      
      color_scale_limits <- range(total_summary$total_points_sum, na.rm = TRUE)
      
      p_heatmap <- ggplot(total_summary, aes(x = knowledge, y = title_ID, fill = total_points_sum,
                                             text = paste("Total Points:", total_points_sum, "<br>Total Attempts:", total_attempts))) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "blue", mid = "green", high = "red", midpoint = mean(color_scale_limits, na.rm = TRUE), 
                             limits = color_scale_limits, name = "Total Points") +
        labs(title = "Total Points per Question per Knowledge Area",
             x = "Knowledge Areas",
             y = "Question IDs",
             fill = "Total Points") +
        theme_minimal(base_size = 15) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 6),
          axis.title.x = element_text(size = 10, margin = margin(t = 15)),
          axis.title.y = element_text(size = 10, margin = margin(r = 15)),
          plot.title = element_text(size = 8, face = "bold", margin = margin(b = 15)),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8),
          legend.key.size = unit(1, "cm"),
          legend.position = "right"
        )
      
      ggplotly(p_heatmap, tooltip = "text")
    } else if (input$knowledgeMetric == 'MasteryPointsKnowledge') {
      # Mastery Points for Knowledge Area plot code
      mean_scores <- knowledge_mastery %>%
        group_by(knowledge) %>%
        summarize(mean_total_score = mean(total_score, na.rm = TRUE)) %>%
        arrange(desc(mean_total_score))
      
      knowledge_mastery <- knowledge_mastery %>%
        mutate(knowledge = factor(knowledge, levels = mean_scores$knowledge))
      
      p <- ggplot(knowledge_mastery, aes(x = knowledge, y = total_score, text = paste("Total Score:", total_score))) +
        geom_boxplot(fill = "gray", color = "darkblue", alpha = 0.7) +
        geom_jitter(width = 0.2, alpha = 0.5, color = "skyblue") +
        theme_minimal() +
        labs(
          title = "Distribution of Total Mastery Points by Knowledge",
          x = "Knowledge",
          y = "Total Mastery Points"
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      ggplotly(p, tooltip = "text")
    } else if (input$knowledgeMetric == 'MasteryPointsSubKnowledge') {
      # Mastery Points for Sub Knowledge Area plot code
      mean_scores <- knowledge_mastery %>%
        group_by(sub_knowledge) %>%
        summarize(mean_total_score = mean(total_score, na.rm = TRUE)) %>%
        arrange(desc(mean_total_score))
      
      knowledge_mastery <- knowledge_mastery %>%
        mutate(sub_knowledge = factor(sub_knowledge, levels = mean_scores$sub_knowledge))
      
      p <- ggplot(knowledge_mastery, aes(x = sub_knowledge, y = total_score, text = paste("Total Score:", total_score))) +
        geom_boxplot(fill = "gray", color = "darkblue", alpha = 0.7) +
        geom_jitter(width = 0.2, alpha = 0.5, color = "skyblue") +
        theme_minimal() +
        labs(
          title = "Distribution of Total Mastery Points by Sub-Knowledge",
          x = "Sub-Knowledge",
          y = "Total Mastery Points"
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      ggplotly(p, tooltip = "text")
    }
  })
}

shinyApp(ui, server)
