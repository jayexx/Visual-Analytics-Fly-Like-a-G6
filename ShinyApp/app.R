library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(dplyr)
library(ggplot2)
library(ggridges)
library(scales)
library(reshape2)
library(mirt)
library(lattice)
library(hrbrthemes)
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

# Load your data
merged_data <- readRDS("merged_data1.RDS")
knowledge_expanded <- readRDS("knowledge_expanded1.RDS")
mastery_scores <- readRDS("mastery_scores1.RDS")
adjusted_scores <- readRDS("adjusted_scores1.RDS")
knowledge_mastery <- readRDS("knowledge_mastery1.RDS")

StudentLMKA_data <- readRDS("StudentLMKA_data.rds")
StudentLM_data <- readRDS("StudentLM_data.rds")

merged_data <- readRDS("merged_data.RDS")
knowledge_expanded <- readRDS("knowledge_expanded.RDS")
never_absolutely_correct <- readRDS("never_absolutely_correct.RDS")
aggregate_title_info <- readRDS("aggregate_title_info.RDS")
percentage_correct <- readRDS("percentage_correct.RDS")
overall_mastery <- readRDS ("overall_mastery.RDS")
df_IRT <- readRDS ("mirtdata.RDS")
# task 2 for yuhui
avg_sil_widths <- readRDS("avg_sil_widths.rds")
cluster_factor <- readRDS("cluster_factor.rds")
# Ensure 'actual_score' and 'question_score' are numeric
knowledge_expanded <- knowledge_expanded %>%
  mutate(actual_score = as.numeric(actual_score),
         question_score = as.numeric(question_score))

# Ensure class is a factor with ordered levels
class_levels <- paste0("Class", 1:15)
overall_mastery <- overall_mastery %>%
  mutate(class = factor(class, levels = class_levels))

# Shiny app UI

ui <- dashboardPage(
  skin = 'blue',
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      "VISUALISING LEARNING EFFECTIVENESS FOR INSIGHTS ON NORTHCLASS INSTITUTE’S EDUCATION SYSTEM"
    ),
    titleWidth = 700
  ),
  dashboardSidebar(
    width = 420,
    sidebarMenu(
      menuItem('HomePage', tabName = 'HomePage', icon = icon("home")),
      menuItem('Task 1: Knowledge Mastery & Weak Links', tabName = 'Task1', icon = icon("chart-bar")),
      menuItem('Task 2: Learners Profile', tabName = 'Task2', icon = icon("user")),
      menuItem('Task 3: Learning Mode & Knowledge Acquisition', tabName = 'Task3', icon = icon("book")),
      menuItem('Task 4: Question Difficulty & Learners Knowledge Level', tabName = 'Task4', icon = icon("question-circle")),
      menuItem('Task 5: Recommendations', tabName = 'Task5', icon = icon("lightbulb"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .logo {
          font-family: 'Arial', sans-serif;
          font-size: 24px;
          font-weight: bold;
        }
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
        }
        .main-sidebar {
          font-size: 16px;
        }
        .sidebar-menu > li > a {
          color: #4a4e4d;
        }
        .sidebar-menu > li > a:hover {
          background-color: #3c8dbc;
          color: white;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = 'HomePage',
              fluidPage(
                box(
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  title = "G6: Visualising Learning Effectiveness for Insights on Northclass Institute",
                  h3("Introduction", style = "text-align: left;"),
                  p("This project focuses on analysis of the learning and performance of students in a computer programming course at NorthClass Training Institute to provide insights and recommendations on its education system."),
                  br(),
                  h3("The Dataset", style = "text-align: left;"),
                  p("The provided materials for the challenge include 3 datasets described below, as well as a separate document providing a more detailed description of the data and variables:"),
                  tags$ul(
                    tags$li("Dataset 1: Student Information - This comprises of 5 columns and 1364 rows, providing individualized demographic variables of the learners (a.k.a students) within the scope of this project."),
                    tags$li("Dataset 2: Learning Subject Title Information - This comprises of 5 columns and 44 rows, providing variables of the questions from the programming tasks which are collated in the scope of this project."),
                    tags$li("Dataset 3: Class Submission Records - This comprises of multiple datasets, each with 10 columns and various number of rows, providing the participating learners’ answering variables to the questions collated in the scope of this project.")
                  ),
                  br(),
                  h3("Objective", style = "text-align: left;"),
                  p("The objectives of the study are as follows:"),
                  tags$ul(
                    tags$li("1) Knowledge Mastery & Weak Links: Quantitatively assess the learners' question-answering behaviors to identify weak links in their knowledge system."),
                    tags$li("2) Learner Profiling: Profile learners from various perspectives based on their personalized learning behavior patterns and characteristics."),
                    tags$li("3) Learning Modes & Knowledge Acquisition: Examine the relationship between the learners' learning modes and knowledge acquisition."),
                    tags$li("4) Question Difficulty & Learner Knowledge: Analyse the alignment of questions' difficulty level with learners' knowledge levels to identify inappropriate questions."),
                    tags$li("5) Provide recommendations for adjusting the question bank and teaching strategies based on the analysis.")
                  )
                )
              )
      ),
                        
                        # Task 1
                        tabItem(tabName = 'Task1',
                                fluidPage(
                                  titlePanel("Task1: Knowledge Mastery and Weak Links"),
                                  tabsetPanel(
                                    tabPanel("Weak Links by Questions",
                                             box(
                                               selectInput('questionMetric', tags$strong('Choose Metric:'),
                                                           choices = c('Normalized Average Highest Score' = 'NormAvgHighestScore',
                                                                       'Non-normalized Average Highest Score' = 'NonNormAvgHighestScore',
                                                                       'Average Methods Applied on Questions' = 'AvgMethodsApplied',
                                                                       'Total Points on Each Question and Knowledge Area' = 'TotalPointsKnowledge')
                                               )),
                                             box(
                                               width = 12, height = 500, solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE,
                                               plotlyOutput('questionPlot', height = 400)
                                             )
                                    ),
                                    tabPanel("Weak Links by Knowledge Area",
                                             box(
                                               selectInput('knowledgeMetric', tags$strong('Choose Metric:'),
                                                           choices = c(
                                                                       'Mastery Points for Knowledge Area' = 'MasteryPointsKnowledge',
                                                                       'Mastery Points for Sub Knowledge Area' = 'MasteryPointsSubKnowledge')
                                               )),
                                             box(
                                               width = 12, height = 500, solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE,
                                               plotlyOutput('knowledgePlot', height = 400)
                                             )
                                    )
                                  )
                                )),
                        
                        # Task 2
                        tabItem(tabName = 'Task2',
                                fluidPage(
                                  titlePanel("Task2: Learners Profile"),
                                  tabsetPanel(
                                    tabPanel("Silhouette Analysis",
                                             fluidRow(
                                               plotOutput("plot01", height = "400px")  # Silhouette Analysis plot
                                             )
                                    ),
                                    tabPanel("Parallel Coordinates Plot",
                                             fluidRow(
                                               column(2, 
                                                      sliderInput("num_clusters", "Number of Clusters:",
                                                                  min = 2, max = 10, value = 2)),
                                               column(10, 
                                                      plotlyOutput("plot02", height = "800px"))  # Parallel Coordinates Plot
                                             ),
                                             fluidRow(
                                               column(2,
                                                      checkboxGroupInput("selected_vars", "Select Variables:",
                                                                         choices = list("Title ID" = "title_ID",
                                                                                        "Knowledge" = "knowledge",
                                                                                        "Method" = "method",
                                                                                        "Age" = "age",
                                                                                        "Major" = "major"),
                                                                         selected = c("title_ID", "knowledge", "method", "age", "major"))
                                               ),
                                               column(10,
                                                      DTOutput("cluster_table", height = "400px"))  # Data Table
                                             )
                                    )
                                  )
                                )
                        )
                        
                        ,
                        
                        # Task 3
                        tabItem(tabName = 'Task3',
                                fluidPage(
                                  titlePanel("Task3: Learning Mode & Knowledge Acquisition"),
                                  tabsetPanel(
                                    tabPanel("Clustering Analysis for Learning Modes",
                                             fluidRow( 
                                               title = "Silhouette Analysis for Number of K-means Clusters",
                                               plotOutput("plot1", height = "400px")
                                             ),
                                             
                                             fluidRow(


                                               sliderInput("num_clusters1", "Number of Clusters:",
                                                           min = 2, max = 10, value = 2) ,                                            
                                             
                                               title = "Parallel Coordinate plot",
                                               plotlyOutput("plot2", height = "400px")
                                             )
                                    ),
                                    tabPanel("Knowledge Acquisition Distribution Across Both Clusters", 
                                             selectInput("KAindicator1", tags$strong("Choose an indicator:"), 
                                                         choices = c("No. of questions answered fully or partially correct", 
                                                                     "Overall sum of highest submission scores per question", 
                                                                     "Overall sum of question mastery points"), 
                                                         selected = "Overall sum of question mastery points"),
                                             
                                               box(
                                               title = "Ridgeline Plot of Distribution of Both Clusters",
                                               plotOutput("plot3", height = "400px")
                                               ),
                                             
                                             
                                             
                                               box(
                                                 title = "Ridgeline Plot of Distribution by Knowledge Areas for Both Clusters",
                                                 plotOutput("plot4", height = "1000px")
                                               ) 
                                             
                                             
                                    ),
                                    tabPanel("2-Sample Mean Statistical Test For Both Clusters",
                                             selectInput("KAindicator2", tags$strong("Choose an indicator:"), 
                                                         choices = c("Percent of submissions absolutely correct", 
                                                                     "Overall sum of highest submission scores per question", 
                                                                     "Overall sum of question mastery points"), 
                                                         selected = "Overall sum of question mastery points"),
                                             box(
                                               title = "2-Sample Difference in Mean Statistical Test for Both Clusters",
                                               plotlyOutput("plot5", height = "400px")
                                             )
                                    ),
                                    tabPanel("Multi-linear Regression (Alternative from Clustering)",
                                             selectInput("KAindicator3", tags$strong("Choose an indicator:"), 
                                                         choices = c("Overall sum of highest submission scores per question", 
                                                                     "Overall sum of question mastery points"), 
                                                         selected = "Overall sum of question mastery points"),
                                             fluidRow(
                                               column( width = 12,
                                                 
                                                 title = "Multi-linear Regression of learning mode features (Overall)",
                                                 plotlyOutput("plot6", width = NULL, height = "600px")
                                               
                                               )
                                             ),
                                             fluidRow(
                                               column( width = 12,
                                               
                                                 title = "Multi-linear Regression of learning mode features (By Knowledge Areas)",
                                                 plotlyOutput("plot7", width = NULL, height = "1500px")
                                               
                                               )
                                             )
                                             
                                    )
                                  )
                                )),
                        
                        # Task 4
                        tabItem(tabName = 'Task4',
                                fluidPage(
                                  titlePanel("Task4: Question Difficulty & Learners Knowledge Level"),
                                  sidebarPanel(
                                    selectInput("threshold", "Select Threshold:", 
                                                choices = c("90%", "95%"), selected = "95%"), 
                                    actionButton("filter", "Filter High Mastery Students")
                                  ),
                                  mainPanel(
                                    div(style = "overflow-y: scroll; width: 100%;",
                                        plotOutput("dumbbellPlot", height = "600px"))
                                  ),
                                  fluidRow(
                                    column(12, plotOutput("percentageWrongPlot"))
                                  ),
                                  fluidRow(
                                    column(12, plotOutput("tracePlot"))
                                  ),
                                  fluidRow(
                                    column(12, plotOutput("CCCPlot"))
                                  )
                                )),
                        
                        # Task 5
                        tabItem(tabName = 'Task5',
                                fluidPage(
                                  titlePanel("Task5: Recommendations"), 
                                  mainPanel(
                                    h3("", align = 'center'),
                                    br(),
                                    strong('Recommendations to enhance Course design & management:'),
                                    p(''),
                                    tags$ul(
                                      tags$li(
                                        "1. Targeted Remediation for Weak Knowledge Areas: Identify students with poor performance on specific knowledge areas such as r8S3g, s8Y2f. Provide focused remediation sessions with targeted exercises and practice problems to address these gaps."
                                      ),
                                      tags$li(
                                        "2. Supporting Variable Sub-Knowledge Areas: For areas like m3D1v, where specific sub-knowledge shows variability, offer detailed feedback, small group instruction, and supplemental tutoring sessions. Utilize targeted practice and peer tutoring to improve understanding."
                                      ),
                                      tags$li(
                                        "3. Addressing High Error Rate Questions: Questions such as Question_5fgqjSBwTPG7KUV3it6O and Question_YWXHr4G6Cl7bEm9iF2kQ, which consistently show higher error rates, tailor teaching methods to meet varied learning needs. Provide additional explanations, simplified examples, and hands-on activities. Break down complex topics into smaller, manageable chunks, offering support at each step and gradually reducing assistance to build confidence and competence."
                                      ),
                                      tags$li(
                                        "4. Reinforcing Foundational Concepts: Reinforce foundational concepts through interactive and hands-on learning activities. Relate these concepts to real-world examples and applications to help students understand their relevance and practical use."
                                      ),
                                      tags$li(
                                        "5. Frequent Formative Assessments: Conduct frequent formative assessments to monitor students’ progress and understanding. Use the results to adjust teaching strategies and provide timely, constructive feedback."
                                      )
                                    ),
                                    br(),
                                    strong('Recommendations to improve Question bank:'),
                                    p(''),
                                    tags$ul(
                                      tags$li(
                                        "1. Balanced Coverage of Knowledge Areas: Currently, some knowledge areas and sub-knowledge areas, such as k4W1c and s8Y2f, include only one question, while b3C9s has three questions. This limits the assessment of learners' mastery. To provide a comprehensive evaluation, ensure that the question bank includes a balanced number of questions across all knowledge and sub-knowledge areas. This will help in identifying gaps in students' understanding more accurately."
                                      ),
                                      tags$li(
                                        "2. Incorporate Various Question Types: Include diverse formats of questions, such as multiple-choice, short answer, problem-solving, and essay questions. Add more question types specifically targeting weaker sub-knowledge areas like s8Y2f_v4x8by9j and g7R2j_j1g8g3v. These questions should vary in difficulty to progressively build students' mastery."
                                      ),
                                      tags$li(
                                        "3. Sequential Difficulty and Instant Feedback: Arrange questions in a sequence of increasing difficulty. Start with basic questions to build confidence and gradually introduce more complex problems to challenge students. Design questions that provide instant feedback, offering explanations for both correct and incorrect responses. This helps students learn from their mistakes."
                                      ),
                                      tags$li(
                                        "4. Engagement through Application: Diversify the sub-knowledge areas by incorporating real-world scenarios through case studies and project-based questions. This approach will engage students, encourage them to apply their knowledge, and facilitate learning through answering questions."
                                      ),
                                      tags$li(
                                        "5. Dynamic Question Bank: Regularly update the question bank based on student feedback to ensure balanced and thorough assessments. These strategies will help identify knowledge gaps and inform more effective teaching methods, ultimately leading to improved student performance across all knowledge areas."
                                      )
                                    )
                                  )
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
                                                      text = paste("Knowledge Area:", knowledge,
                                                                   "<br>Question ID:", title_ID,
                                                                   "<br>Avg Highest Score:", round(average_highest_score, 2),
                                                                   "<br>Question Score:", question_score))) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "blue", mid = "green", high = "red", midpoint = 0.9, 
                             limits = color_scale_limits, name = "Avg Highest Score") +
        labs(title = "Normalized Average Highest Actual Score per Knowledge Area per Question",
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
                                                      text = paste("Knowledge Area:", knowledge,
                                                                   "<br>Question ID:", title_ID,
                                                                   "<br>Avg Highest Score:", round(average_highest_score, 2),
                                                                   "<br>Question Score:", question_score))) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "blue", mid = "green", high = "red", midpoint = 2.5, 
                             limits = color_scale_limits, name = "Avg Highest Score") +
        labs(title = "Non-normalized Average Highest Actual Score per Knowledge Area per Question",
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
    } else if (input$questionMetric == 'TotalPointsKnowledge') {
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
                                             text = paste("Knowledge Area:", knowledge,
                                                          "<br>Question ID:", title_ID,
                                                          "<br>Total Points:", total_points_sum,
                                                          "<br>Total Attempts:", total_attempts))) +
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
    } else if (input$knowledgeMetric == 'MaxActualScoresDistribution') {
      # Max Actual Scores Distribution per Knowledge Area plot code
      max_scores <- adjusted_scores %>%
        group_by(title_ID, student_ID, knowledge) %>%
        summarize(max_actual_score = max(actual_score, na.rm = TRUE), .groups = 'drop')
      
      p_ridge_max_scores_quantiles <- ggplot(max_scores, aes(x = max_actual_score, y = knowledge, fill = factor(stat(quantile)))) +
        stat_density_ridges(
          geom = "density_ridges_gradient",
          calc_ecdf = TRUE,
          quantiles = 4,
          quantile_lines = TRUE
        ) +
        scale_fill_viridis_d(name = "Quartiles") +
        labs(title = "Max Actual Scores Distribution per Knowledge Area", x = "Max Actual Score", y = "Knowledge Area") +
        theme_ridges() +
        theme(
          axis.text.y = element_text(size = 8),
          axis.title.x = element_text(size = 10, margin = margin(t = 10), hjust = 0.5),
          axis.title.y = element_text(size = 10, margin = margin(r = 10), hjust = 0.5),
          plot.title = element_text(size = 10, face = "bold", margin = margin(b = 20)),
          legend.position = "right",
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8)
        )
      
      ggplotly(p_ridge_max_scores_quantiles, tooltip = "text")
    }
  })
  # Add server logic for Task 2 here yuhui
    
  # Define server logic for Task2
    
    # Silhouette Analysis logic
    output$plot01 <- renderPlot({
      # Plot the average silhouette widths
      plot(1:18, avg_sil_widths, type = "b", pch = 19, frame = FALSE,
           xlab = "Number of clusters", ylab = "Average silhouette width",
           main = "Silhouette Analysis for Determining Optimal Number of Clusters")
      
      # Highlight the optimal number of clusters
      optimal_clusters <- which.max(avg_sil_widths)
      points(optimal_clusters, avg_sil_widths[optimal_clusters], col = "red", pch = 19)
    })
    
    # Parallel Coordinates Plot logic
    output$plot02 <- renderPlotly({
      
      generate_clusters <- function(data, k) {
        numeric_data <- data %>% select_if(is.numeric)
        kmeans_result <- kmeans(numeric_data, centers = k)
        data$cluster <- as.factor(kmeans_result$cluster)
        return(data)
      }

    cluster_data00 <- generate_clusters(cluster_factor, input$num_clusters)
    parallel_plot <- ggparcoord(data = cluster_data00, 
                                columns = c(2, 3, 5, 7, 9, 10, 11, 12, 13), 
                                groupColumn = 14, # cluster column index
                                scale = "uniminmax",
                                alphaLines = 0.2,
                                boxplot = TRUE, 
                                title = "Parallel Coordinates Plot of Students' learning modes") +
      theme(axis.text.x = element_text(angle = 30, size = 20))
    ggplotly(parallel_plot, tooltip = "text") %>% layout(showlegend = FALSE)
    })
    
    output$cluster_table <- renderDT({
      
      # 计算每个聚类中所选变量的最高占比和名称
      get_top_percentage <- function(df, selected_vars) {
        result <- df %>%
          select(all_of(c(selected_vars, "cluster"))) %>%
          gather(key = "variable", value = "value", -cluster) %>%
          group_by(cluster, variable, value) %>%
          summarise(count = n(), .groups = 'drop') %>%
          mutate(percentage = count / sum(count) * 100) %>%
          arrange(cluster, variable, desc(percentage)) %>%
          group_by(cluster, variable) %>%
          slice(1) %>%
          ungroup() %>%
          select(cluster, variable, value)
        return(result)
      }
      
      generate_clusters <- function(data, k) {
        numeric_data <- data %>% select_if(is.numeric)
        kmeans_result <- kmeans(numeric_data, centers = k)
        data$cluster <- as.factor(kmeans_result$cluster)
        return(data)
      }
      
      cluster_data00 <- generate_clusters(cluster_factor, input$num_clusters)
      
      req(input$selected_vars)
      result_table <- get_top_percentage(cluster_data00, input$selected_vars)
      datatable(result_table, options = list(pageLength = 10, autoWidth = TRUE))
    })

  
  
  
  
  
  
  
  # Server logic for Task 3
  output$plot1 <- renderPlot({
    clustering_data <- StudentLM_data %>%
      select(-student_ID)
    
    silhouette_analysis <- function(data, max_clusters) {
      avg_sil_widths <- numeric(max_clusters)
      
      for (k in 2:max_clusters) {
        kmeans_result <- kmeans(data, centers = k, nstart = 25)
        sil <- silhouette(kmeans_result$cluster, dist(data))
        avg_sil_widths[k] <- mean(sil[, 3])
      }
      
      return(avg_sil_widths)
    }
    
    max_clusters <- 12
    avg_sil_widths <- silhouette_analysis(clustering_data, max_clusters)
    
    plot(1:max_clusters, avg_sil_widths, type = "b", pch = 19, frame = FALSE,
         xlab = "Number of clusters", ylab = "Average silhouette width",
         main = "Silhouette Analysis for Determining Optimal Number of Clusters")
    
    optimal_clusters <- which.max(avg_sil_widths)
    points(optimal_clusters, avg_sil_widths[optimal_clusters], col = "red", pch = 19)
  })
  
  output$plot2 <- renderPlotly({

    
    clustering_data <- StudentLM_data %>%
      select(-student_ID)
    
    clustering_data_scaled <- scale(clustering_data)
    set.seed(123)
    kmeans_result <- kmeans(clustering_data_scaled, centers = input$num_clusters1, nstart = 25)
    
    StudentLM_data$cluster <- kmeans_result$cluster
    StudentLM_data_factor <- StudentLM_data
    StudentLM_data_factor$cluster <- as.character(StudentLM_data_factor$cluster)
    
    parallel_plot1 <- ggparcoord(data = StudentLM_data_factor,
               columns = c(2:13), 
               groupColumn = 14,
               scale = "uniminmax",
               alphaLines = 0.2,
               boxplot = TRUE, 
               title = "Parallel Coordinates Split Plot of Students' learning modes") +
      facet_wrap(~ cluster) +
      theme(
        plot.title = element_text(size = 10),
        axis.text.x = element_text(angle = 30, hjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)
      )
    ggplotly(parallel_plot1, tooltip = "text")
  })
  
  output$plot3 <- renderPlot({
    StudentLMKA_data$cluster <- as.factor(StudentLMKA_data$cluster)
    
    KAindicator01 <- NULL
    if (input$KAindicator1 == "No. of questions answered fully or partially correct") {
      KAindicator01 <- "`No. of questions answered fully or partially correct`"
    } else if (input$KAindicator1 == "Overall sum of highest submission scores per question") {
      KAindicator01 <- "`Sum of overall highest submission scores`"
    } else if (input$KAindicator1 == "Overall sum of question mastery points") {
      KAindicator01 <- "`Sum of points Overall`"
    }
    
    ggplot(StudentLMKA_data, 
           aes_string(x = KAindicator01, 
                      y = "cluster",
                      fill = "factor(stat(quantile))")) +
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
    
    a <- ggplot(StudentLMKA_data, 
                aes(x = `Sum of points for b3C9s knowledge`, y = cluster, fill = factor(stat(quantile)))) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE, 
        quantiles = 4,
        quantile_lines = TRUE) +
      scale_fill_viridis_d(name = "Quartiles") +
      theme_ridges()
    
    b <- ggplot(StudentLMKA_data, 
                aes(x = `Sum of points for g7R2j knowledge`, y = cluster, fill = factor(stat(quantile)))) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE, 
        quantiles = 4,
        quantile_lines = TRUE) +
      scale_fill_viridis_d(name = "Quartiles") +
      theme_ridges()
    
    c <- ggplot(StudentLMKA_data, 
                aes(x = `Sum of points for m3D1v knowledge`, y = cluster, fill = factor(stat(quantile)))) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE, 
        quantiles = 4,
        quantile_lines = TRUE) +
      scale_fill_viridis_d(name = "Quartiles") +
      theme_ridges()
    
    d <- ggplot(StudentLMKA_data, 
                aes(x = `Sum of points for r8S3g knowledge`, y = cluster, fill = factor(stat(quantile)))) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE, 
        quantiles = 4,
        quantile_lines = TRUE) +
      scale_fill_viridis_d(name = "Quartiles") +
      theme_ridges()
    
    e <- ggplot(StudentLMKA_data, 
                aes(x = `Sum of points for t5V9e knowledge`, y = cluster, fill = factor(stat(quantile)))) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE, 
        quantiles = 4,
        quantile_lines = TRUE) +
      scale_fill_viridis_d(name = "Quartiles") +
      theme_ridges()
    
    f <- ggplot(StudentLMKA_data, 
                aes(x = `Sum of points for y9W5d knowledge`, y = cluster, fill = factor(stat(quantile)))) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE, 
        quantiles = 4,
        quantile_lines = TRUE) +
      scale_fill_viridis_d(name = "Quartiles") +
      theme_ridges()
    
    g <- ggplot(StudentLMKA_data, 
                aes(x = `Sum of points for k4W1c knowledge`, y = cluster, fill = factor(stat(quantile)))) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE, 
        quantiles = 4,
        quantile_lines = TRUE) +
      scale_fill_viridis_d(name = "Quartiles") +
      theme_ridges()
    
    h <- ggplot(StudentLMKA_data, 
                aes(x = `Sum of points for s8Y2f knowledge`, y = cluster, fill = factor(stat(quantile)))) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE, 
        quantiles = 4,
        quantile_lines = TRUE) +
      scale_fill_viridis_d(name = "Quartiles") +
      theme_ridges()
    
    (a + b) / (c + d) / (e + f) / (g + h)
  })
  
  output$plot5 <- renderPlotly({
    KAindicator02 <- NULL
    if (input$KAindicator2 == "Percent of submissions absolutely correct") {
      KAindicator02 <- "Percent of submissions absolutely correct"
    } else if (input$KAindicator2 == "Overall sum of highest submission scores per question") {
      KAindicator02 <- "Sum of overall highest submission scores"
    } else if (input$KAindicator2 == "Overall sum of question mastery points") {
      KAindicator02 <- "Sum of points Overall"
    }
    
    plot <- ggbetweenstats(
      data = StudentLMKA_data,
      x = "cluster", 
      y = !!sym(KAindicator02),  
      type = "np",
      messages = FALSE
    )
    ggplotly(plot)
  })
  
  output$plot6 <- renderPlotly({
    KAindicator03 <- NULL
    if (input$KAindicator3 == "Overall sum of highest submission scores per question") {
      KAindicator03 <- "Sum of overall highest submission scores"
    } else {
      KAindicator03 <- "Sum of points Overall"
    }
    
    model <- lm(get(KAindicator03) ~  
                  `Percent of submissions on weekdays` +
                  `Percent of submissions during working hrs` +
                  `Total no. of different qns_attempted` +
                  `Gini Index for qns in submission` +
                  `Mean selected question scores` +
                  `Mean submission memory size by qns` +
                  `Mean timeconsume by qns` +
                  `Total no. of submissions` +
                  `Mean no. of different answering methods per qns` +
                  `Gini index for answering methods used per qns` +
                  `Total memory size of submissions` +
                  `Total timeconsume of submissions`, data = StudentLMKA_data)
    
    plot61 <- ggcoefstats(model, 
                output = "plot") +
                theme(
                      plot.title = element_text(size = 10),
                      axis.title = element_text(size = 8),
                      axis.text = element_text(size = 8),
                      legend.title = element_text(size = 10),
                      legend.text = element_text(size = 8)
                    )
    ggplotly(plot61)
  })
  
  output$plot7 <- renderPlotly({
    model3 <- lm(`Sum of points for b3C9s knowledge` ~  
                   `Percent of submissions on weekdays` +
                   `Percent of submissions during working hrs` +
                   `Total no. of different qns_attempted` +
                   `Gini Index for qns in submission` +
                   `Mean selected question scores` +
                   `Mean submission memory size by qns` +
                   `Mean timeconsume by qns` +
                   `Total no. of submissions` +
                   `Mean no. of different answering methods per qns` +
                   `Gini index for answering methods used per qns` +
                   `Total memory size of submissions` +
                   `Total timeconsume of submissions`, data = StudentLMKA_data)
    
    model4 <- lm(`Sum of points for g7R2j knowledge` ~ 
                   `Percent of submissions on weekdays` +
                   `Percent of submissions during working hrs` +
                   `Total no. of different qns_attempted` +
                   `Gini Index for qns in submission` +
                   `Mean selected question scores` +
                   `Mean submission memory size by qns` +
                   `Mean timeconsume by qns` +
                   `Total no. of submissions` +
                   `Mean no. of different answering methods per qns` +
                   `Gini index for answering methods used per qns` +
                   `Total memory size of submissions` +
                   `Total timeconsume of submissions`, data = StudentLMKA_data)
    
    model5 <- lm(`Sum of points for k4W1c knowledge` ~  
                   `Percent of submissions on weekdays` +
                   `Percent of submissions during working hrs` +
                   `Total no. of different qns_attempted` +
                   `Gini Index for qns in submission` +
                   `Mean selected question scores` +
                   `Mean submission memory size by qns` +
                   `Mean timeconsume by qns` +
                   `Total no. of submissions` +
                   `Mean no. of different answering methods per qns` +
                   `Gini index for answering methods used per qns` +
                   `Total memory size of submissions` +
                   `Total timeconsume of submissions`, data = StudentLMKA_data)
    
    model6 <- lm(`Sum of points for m3D1v knowledge` ~  
                   `Percent of submissions on weekdays` +
                   `Percent of submissions during working hrs` +
                   `Total no. of different qns_attempted` +
                   `Gini Index for qns in submission` +
                   `Mean selected question scores` +
                   `Mean submission memory size by qns` +
                   `Mean timeconsume by qns` +
                   `Total no. of submissions` +
                   `Mean no. of different answering methods per qns` +
                   `Gini index for answering methods used per qns` +
                   `Total memory size of submissions` +
                   `Total timeconsume of submissions`, data = StudentLMKA_data)
    
    model7 <- lm(`Sum of points for r8S3g knowledge` ~ 
                   `Percent of submissions on weekdays` +
                   `Percent of submissions during working hrs` +
                   `Total no. of different qns_attempted` +
                   `Gini Index for qns in submission` +
                   `Mean selected question scores` +
                   `Mean submission memory size by qns` +
                   `Mean timeconsume by qns` +
                   `Total no. of submissions` +
                   `Mean no. of different answering methods per qns` +
                   `Gini index for answering methods used per qns` +
                   `Total memory size of submissions` +
                   `Total timeconsume of submissions`, data = StudentLMKA_data)
    
    model8 <- lm(`Sum of points for s8Y2f knowledge` ~  
                   `Percent of submissions on weekdays` +
                   `Percent of submissions during working hrs` +
                   `Total no. of different qns_attempted` +
                   `Gini Index for qns in submission` +
                   `Mean selected question scores` +
                   `Mean submission memory size by qns` +
                   `Mean timeconsume by qns` +
                   `Total no. of submissions` +
                   `Mean no. of different answering methods per qns` +
                   `Gini index for answering methods used per qns` +
                   `Total memory size of submissions` +
                   `Total timeconsume of submissions`, data = StudentLMKA_data)
    
    model9 <- lm(`Sum of points for t5V9e knowledge` ~  
                   `Percent of submissions on weekdays` +
                   `Percent of submissions during working hrs` +
                   `Total no. of different qns_attempted` +
                   `Gini Index for qns in submission` +
                   `Mean selected question scores` +
                   `Mean submission memory size by qns` +
                   `Mean timeconsume by qns` +
                   `Total no. of submissions` +
                   `Mean no. of different answering methods per qns` +
                   `Gini index for answering methods used per qns` +
                   `Total memory size of submissions` +
                   `Total timeconsume of submissions`, data = StudentLMKA_data)
    
    model10 <- lm(`Sum of points for y9W5d knowledge` ~ 
                    `Percent of submissions on weekdays` +
                    `Percent of submissions during working hrs` +
                    `Total no. of different qns_attempted` +
                    `Gini Index for qns in submission` +
                    `Mean selected question scores` +
                    `Mean submission memory size by qns` +
                    `Mean timeconsume by qns` +
                    `Total no. of submissions` +
                    `Mean no. of different answering methods per qns` +
                    `Gini index for answering methods used per qns` +
                    `Total memory size of submissions` +
                    `Total timeconsume of submissions`, data = StudentLMKA_data)
    
    a <- ggcoefstats(model3, 
                     output = "plot")
    b <- ggcoefstats(model4, 
                     output = "plot")
    c <- ggcoefstats(model5, 
                     output = "plot")
    d <- ggcoefstats(model6, 
                     output = "plot")
    e <- ggcoefstats(model7, 
                     output = "plot")
    f <- ggcoefstats(model8, 
                     output = "plot")
    g <- ggcoefstats(model9, 
                     output = "plot")
    h <- ggcoefstats(model10, 
                     output = "plot")
    
    a1 <- ggplotly(a)
    b1 <- ggplotly(b)
    c1 <- ggplotly(c)
    d1 <- ggplotly(d)
    e1 <- ggplotly(e)
    f1 <- ggplotly(f)
    g1 <- ggplotly(g)
    h1 <- ggplotly(h)
    
    # 使用 subplot 函数组合 plotly 对象
    combined_plot <- subplot(
      list(a1, b1, c1, d1, e1, f1, g1, h1),
      nrows = 4, shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE
    )
    
    # 显示组合的 plotly 图表
    combined_plot
})
  
  # Add server logic for Task 4 here
  # Task 4: Reactive expression to dynamically choose the plot based on the selected metric for questions
  observeEvent(input$filter, {
    threshold <- input$threshold
    attribute <- input$attribute
    selected_items <- input$itemSelect
    
    
    # Calculate threshold scores for 99% and 1%
    threshold_99 <- calculate_threshold_scores(overall_mastery, as.numeric(sub("%", "", threshold)))
    threshold_1 <- calculate_threshold_scores(overall_mastery, 100 - as.numeric(sub("%", "", threshold)))
    
    # Filter high and low mastery students based on the threshold scores
    high_mastery_students <- overall_mastery %>%
      filter(overall_mastery > threshold_99)
    
    low_mastery_students <- overall_mastery %>%
      filter(overall_mastery < threshold_1)
    
    
    # Filter never absolutely correct questions based on high mastery students
    filtered_never_correct <- never_absolutely_correct %>%
      filter(student_ID %in% high_mastery_students$student_ID) %>%
      distinct(student_ID, title_ID) %>%
      group_by(title_ID) %>%
      summarise(number_of_students = n(), .groups = 'drop') %>%
      left_join(aggregate_title_info, by = "title_ID") %>%
      mutate(percentage_of_student_wrong = (number_of_students/length(high_mastery_students$student_ID)*100)) 
    
    
    # Check if bottom percentage students got the questions that were never correct by high mastery students correct
    bottom_percentage_never_correct <- knowledge_expanded %>%
      filter(student_ID %in% low_mastery_students$student_ID) %>%
      filter(title_ID %in% filtered_never_correct$title_ID) %>%
      filter(state == "Absolutely_Correct") %>%
      distinct(student_ID, title_ID) %>%
      group_by(title_ID) %>%
      summarise(no_of_students = n(), .groups = 'drop') %>%
      mutate(percentage_of_student_right = (no_of_students/length(low_mastery_students$student_ID)*100)) 
    
    
    output$bottomPercentageTable <- renderDataTable({
      datatable(bottom_percentage_never_correct)
    })
    
    # Calculate percentages for high mastery and low mastery students
    high_mastery_percent <- filtered_never_correct %>%
      mutate(high_mastery = number_of_students / nrow(high_mastery_students) * 100) %>%
      select (title_ID, high_mastery)
    
    low_mastery_percent <- bottom_percentage_never_correct %>%
      mutate(low_mastery = no_of_students / nrow(low_mastery_students) * 100) %>%
      select (title_ID, low_mastery)
    
    comparison_data <- high_mastery_percent %>%
      left_join(low_mastery_percent, by = "title_ID") %>%
      mutate(diff = round(low_mastery - high_mastery),digits = 2) %>%
      pivot_longer (cols = c(low_mastery,high_mastery)) %>%
      rename (type_of_student = name,
              percentage = value)
    
    low_mastery <-  comparison_data %>%
      filter(type_of_student == "low_mastery")
    
    high_mastery <-  comparison_data %>%
      filter(type_of_student == "high_mastery")
    
    stats <- comparison_data %>%
      group_by(type_of_student) %>%
      summarise(mean = mean(percentage),
                SE = sd(percentage)) %>%
      mutate(meanpos = mean + 1 *SE,
             meanneg = mean - 1 *SE)
    
    stats_low_mastery <- stats %>%
      filter(type_of_student == "low_mastery")
    stats_high_mastery <- stats %>%
      filter(type_of_student == "high_mastery")
    
    diff <- comparison_data %>% 
      filter(type_of_student == "low_mastery") %>%
      mutate(x_pos = percentage + (-diff/2))
    
    comparison_spread <- comparison_data %>%
      spread(type_of_student, percentage)
    
    # Join the stats to get the mean values
    comparison_spread <- comparison_spread %>%
      left_join(stats %>% filter(type_of_student == "high_mastery") %>% select(-type_of_student), by = character()) %>%
      rename(high_mastery_mean = mean, high_mastery_SE = SE, high_mastery_meanpos = meanpos, high_mastery_meanneg = meanneg) %>%
      left_join(stats %>% filter(type_of_student == "low_mastery") %>% select(-type_of_student), by = character()) %>%
      rename(low_mastery_mean = mean, low_mastery_SE = SE, low_mastery_meanpos = meanpos, low_mastery_meanneg = meanneg)
    
    # Create the color condition
    comparison_spread <- comparison_spread %>%
      mutate(color_condition = case_when(
        high_mastery > high_mastery_meanpos & low_mastery > low_mastery_meanpos ~ "red",
        TRUE ~ "default"
      ))
    
    # Gather the data back to long format
    comparison_long <- comparison_spread %>%
      gather(type_of_student, percentage, high_mastery, low_mastery) %>%
      mutate(type_of_student = factor(type_of_student, levels = c("high_mastery", "low_mastery"))) %>%
      mutate (color_condition = ifelse(color_condition == "default", type_of_student, color_condition))
    
    comparison_long$color_condition <- as.factor(comparison_long$color_condition)
    
    
    output$dumbbellPlot <- renderPlot({
      ggplot(comparison_long) +
        geom_rect(xmin = stats_low_mastery$meanneg, xmax = stats_low_mastery$meanpos,
                  ymin = 0, ymax = 38, fill = "#762a83", alpha = .05) +
        geom_vline(xintercept = stats_low_mastery$mean, linetype = "solid", size = .5, alpha = .8, color = "#762a83")+
        
        geom_rect(xmin = stats_high_mastery$meanneg, xmax = stats_high_mastery$meanpos,
                  ymin = 0, ymax = 38, fill = "#009688", alpha = .05)+  
        geom_vline(xintercept = stats_high_mastery$mean, color = "#009688", linetype = "solid",  size = .5, alpha = .8) +
        
        
        geom_segment(data = low_mastery,
                     aes(x = percentage, y = title_ID,
                         yend = high_mastery$title_ID, xend = high_mastery$percentage),
                     color = "#aeb6bf",
                     size = 4.5,
                     alpha = 0.5) +
        geom_point(aes(x = percentage, y = title_ID, color = color_condition), size = 4, show.legend = TRUE) +
        #color points
        scale_color_manual(values = c("1" = "#009688", "2" = "#762a83", "red" = "red"),
                           labels = c("1" = "High Mastery Students", "2" = "Low Mastery Students"))+
        #add annotations for mean and standard deviations
        geom_text(x = stats_low_mastery$mean + 5, y = 38, label = "MEAN", angle = 90, size = 2.5, color = "#009688")+
        geom_text(x = stats_low_mastery$meanpos + 5, y = 38, label = "STDEV", angle = 90, size = 2.5, color = "#009688")+
        ggtitle("Comparison of High Mastery Student Never Correct Percentage vs Low Mastery Student Correct Percentage") +
        geom_text (data = diff,
                   aes(label = paste("D:", diff, "%"), x = x_pos, y = title_ID),
                   color = "#4a4e4d",
                   size = 2.5) +
        facet_grid(title_ID ~ ., scales = "free", switch = "y") +
        theme_minimal()+
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.ticks.x = element_line(color = "#4a4e4d"),
              text = element_text(family = "Segoe UI Semibold", color = "#4a4e4d"),
              strip.text.y.left  = element_text(angle = 0),
              panel.background = element_rect(fill = "white", color = "white"),
              strip.background = element_rect(fill = "white", color = "white"),
              strip.text = element_text(color = "#4a4e4d", family = "Segoe UI"),
              plot.background = element_rect(fill = "white", color = "white"),
              panel.spacing = unit(0, "lines"),
              plot.margin = margin(1,1,.5,1, "cm"))
      
    })
    
    
    
    df_IRT <- df_IRT %>%
      filter(student_ID %in% high_mastery_students$student_ID | student_ID %in% low_mastery_students$student_ID)
    
    fit3PL <- mirt(data = df_IRT[2:39], 
                   model = 1,  # alternatively, we could also just specify model = 1 in this case
                   itemtype = "2PL",
                   SE = TRUE,
                   verbose = FALSE)
    
    output$tracePlot <- renderPlot({
      plot(fit3PL, type = 'infotrace', which.item = c(1:38), facet_items = TRUE, 
           as.table = TRUE, auto.key = list(points = FALSE, lines = TRUE, columns = 1, space = 'right', cex = .8), 
           theta_lim = c(-3, 3), 
           main = "Item Information Curves Plot of 2PL Model",
           layout = c(10, ceiling(38 / 10)))
    })
    
    output$CCCPlot <- renderPlot({
      plot(fit3PL, type = 'trace', which.item = c(1:38), facet_items = TRUE, 
           as.table = TRUE, auto.key = list(points = FALSE, lines = TRUE, columns = 1, space = 'right', cex = .8), 
           theta_lim = c(-3, 3), 
           main = "Category Characteristic Curves Plot of 2PL Model",
           layout = c(10, ceiling(38 / 10)))
    })
    
    percentage_correct <- percentage_correct %>%
      filter (student_ID %in% high_mastery_students$student_ID) %>%
      group_by(title_ID) %>%
      summarise (avg_percentage_wrong = mean(percentage_wrong))
    
    percentile_75 <- quantile(percentage_correct$avg_percentage_wrong, 0.75)
    
    percentage_correct <- percentage_correct %>%
      mutate(highlight = ifelse(avg_percentage_wrong > percentile_75, "Above 75 Percentile", "Below 75 Percentile"))
    
    output$percentageWrongPlot <- renderPlot({
      ggplot(percentage_correct, aes(x = reorder(title_ID,avg_percentage_wrong), y = avg_percentage_wrong, color = highlight)) +
        geom_segment(aes(x = title_ID, xend = title_ID, y = 0, yend = avg_percentage_wrong), color = "grey") +
        geom_point(size = 4, alpha = 0.8) +
        scale_color_manual(values = c("Above 75 Percentile" = "red", "Below 75 Percentile" = "blue"), name = "Error Quantile") +
        labs(
          title = "Average Error Rate per Question for High Mastery Students",
          x = "Title ID",
          y = "Percentage of Error Rate"
        ) +
        theme_light() +
        coord_flip() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),    
          axis.title.y = element_text(size = 14, color = "black"),
          axis.title.x = element_text(size = 14, color = "black"),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold", color = "black"),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.position = "bottom",
          panel.spacing.x = unit(2, "cm")
        )
    })
  })
}

calculate_threshold_scores <- function(overall_mastery_df, threshold) {
  percentile <- as.numeric(sub("%", "", threshold)) / 100
  threshold_score <- quantile(overall_mastery_df$overall_mastery, percentile, na.rm = TRUE)
  
  return(threshold_score)

  
  # Add server logic for Task 5 here

}


shinyApp(ui, server)
