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
library (mirt)



# Load preprocessed RDS files
merged_data <- readRDS("merged_data.RDS")
knowledge_expanded <- readRDS("knowledge_expanded.RDS")
never_absolutely_correct <- readRDS("never_absolutely_correct.RDS")
aggregate_title_info <- readRDS("aggregate_title_info.RDS")
percentage_correct <- readRDS("percentage_correct.RDS")
overall_mastery <- readRDS ("overall_mastery.RDS")


# Ensure class is a factor with ordered levels
class_levels <- paste0("Class", 1:15)
overall_mastery <- overall_mastery %>%
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
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  observeEvent(input$filter, {
    threshold <- input$threshold
    attribute <- input$attribute
    
    
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
        scale_color_manual(values = c("1" = "#009688", "2" = "#762a83", "red" = "red"))+
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
    
    
    
  })
}

calculate_threshold_scores <- function(overall_mastery_df, threshold) {
  percentile <- as.numeric(sub("%", "", threshold)) / 100
  threshold_score <- quantile(overall_mastery_df$overall_mastery, percentile, na.rm = TRUE)
  
  return(threshold_score)
}


# Run the application
shinyApp(ui, server)