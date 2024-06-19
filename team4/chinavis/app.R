library(shiny)
library(dplyr)
library(igraph)
library(DT)
library(scales)
library(ggstatsplot)
library(ggplot2)
library(ggalluvial)
library(ggalt)
library(tidyverse)
library(reshape2)



# Load preprocessed RDS files
merged_data <- readRDS("merged_data.RDS")
knowledge_expanded <- readRDS("knowledge_expanded.RDS")
never_absolutely_correct <- readRDS("never_absolutely_correct.RDS")
knowledge_mastery <- readRDS("knowledge_mastery.RDS")
aggregate_title_info <- readRDS("aggregate_title_info.RDS")
percentage_correct <- readRDS("percentage_correct.RDS")


# Ensure class is a factor with ordered levels
class_levels <- paste0("Class", 1:15)
knowledge_mastery <- knowledge_mastery %>%
  mutate(class = factor(class, levels = class_levels))

# Define UI
ui <- fluidPage(
  titlePanel("Student Mastery Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("threshold", "Select Threshold:", 
                  choices = c("90%", "95%", "99%"), selected = "99%"),
      selectInput("attribute", "Select Attribute:", 
                  choices = c("class", "sex", "age", "major"), selected = "class"),
      actionButton("filter", "Filter High Mastery Students")
    ),
    mainPanel(
      div(style = "overflow-y: scroll; width: 100%;",
          plotOutput("dumbbellPlot", height = "600px")),
      plotOutput("heatmapPlot"),
      plotOutput("associationPlot"),
      dataTableOutput("questionsTable"),
      dataTableOutput("bottomPercentageTable")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  observeEvent(input$filter, {
    threshold <- input$threshold
    attribute <- input$attribute
    
    # Calculate threshold scores for each knowledge group
    threshold_scores <- calculate_threshold_scores(knowledge_mastery, threshold)
    
    high_mastery_students <- get_high_mastery_students(knowledge_mastery, threshold_scores)
    
    # Summarize student_ID based on the selected attribute and show the connection of Attribute & Knowledge
    attribute_knowledge_edges <- high_mastery_students %>%
      select(!!sym(attribute), knowledge) %>%
      distinct() %>%
      group_by(!!sym(attribute), knowledge) %>%
      summarise(student_count = n(), .groups = 'drop')
    
    # Generate the ggstatsplot for the association between the selected attribute and knowledge
    association_plot <- ggstatsplot::ggbarstats(
      data = high_mastery_students,
      x = knowledge,
      y = !!sym(attribute),
      title = paste("Association between", attribute, "and Knowledge")
    )
    
    output$associationPlot <- renderPlot({
      association_plot
    })
    
    
    # Filter never absolutely correct questions based on high mastery students
    filtered_never_correct <- never_absolutely_correct %>%
      filter(student_ID %in% high_mastery_students$student_ID) %>%
      distinct(student_ID, title_ID) %>%
      group_by(title_ID) %>%
      summarise(number_of_students = n(), .groups = 'drop') %>%
      left_join(aggregate_title_info, by = "title_ID") %>%
      mutate(percentage_of_student_wrong = (number_of_students/length(high_mastery_students$student_ID)*100)) 
    
    # Identify bottom percentage students based on dynamic inputs
    bottom_threshold <- 100 - as.numeric(sub("%", "", threshold))
    threshold_scores <- calculate_threshold_scores(knowledge_mastery, bottom_threshold)
    
    low_mastery_students <- get_low_mastery_students(knowledge_mastery, threshold_scores)
    
    
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
    
    
    output$dumbbellPlot <- renderPlot({
      ggplot(comparison_data) +
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
        geom_point(aes(x = percentage, y = title_ID, color = type_of_student), size = 4, show.legend = TRUE) +
        #color points
        scale_color_manual(values = c("#009688","#762a83"))+
        #add annotations for mean and standard deviations
        geom_text(x = stats_low_mastery$mean + 5, y = 38, label = "MEAN", angle = 90, size = 2.5, color = "#009688")+
        geom_text(x = stats_low_mastery$meanpos + 5, y = 38, label = "STDEV", angle = 90, size = 2.5, color = "#009688")+
        ggtitle("Comparison of High Mastery Student Incorrectness vs Low Mastery Student Correctness") +
        geom_text (data = diff,
                   aes(label = paste("D:", diff, "%"), x = x_pos, y = title_ID),
                   fill = "white",
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
    
    # Calculate average wrong percentage for each title ID and knowledge group
    avg_wrong_percentage <- percentage_correct %>%
      inner_join(high_mastery_students %>% select(student_ID, knowledge), by = "student_ID") %>%
      group_by(title_ID, knowledge) %>%
      summarise(avg_wrong_percentage = mean(percentage_wrong), .groups = 'drop')
    
    # Reshape the data for heatmap
    heatmap_data <- dcast(avg_wrong_percentage, title_ID ~ knowledge, value.var = "avg_wrong_percentage")
    
    output$heatmapPlot <- renderPlot({
      ggplot(melt(heatmap_data, id.vars = "title_ID"), aes(x = variable, y = title_ID, fill = value)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "red") +
        labs(title = "Average Wrong Percentage for Each Title ID by Knowledge Group",
             x = "Knowledge Group",
             y = "Title ID") +
        theme_minimal()
    })
    
    
  })
}

calculate_threshold_scores <- function(knowledge_mastery, threshold) {
  percentile <- as.numeric(sub("%", "", threshold)) / 100
  threshold_scores <- knowledge_mastery %>%
    group_by(knowledge) %>%
    summarise(threshold_score = quantile(average_score, percentile, na.rm = TRUE))
  
  return(threshold_scores)
}

get_high_mastery_students <- function(knowledge_mastery, threshold_scores) {
  high_mastery_students <- knowledge_mastery %>%
    inner_join(threshold_scores, by = "knowledge") %>%
    filter(average_score > threshold_score) %>%
    ungroup()
  return(high_mastery_students)
}

get_low_mastery_students <- function(knowledge_mastery, threshold_scores) {
  low_mastery_students <- knowledge_mastery %>%
    inner_join(threshold_scores, by = "knowledge") %>%
    filter(average_score < threshold_score) %>%
    ungroup()
  return(low_mastery_students)
}

# Run the application
shinyApp(ui, server)