library(shiny)
library(dplyr)
library(igraph)
library(DT)
library(scales)
library(ggstatsplot)
library(ggplot2)
library(ggalluvial)

# Load preprocessed RDS files
merged_data <- readRDS("merged_data.RDS")
knowledge_expanded <- readRDS("knowledge_expanded.RDS")
never_absolutely_correct <- readRDS("never_absolutely_correct.RDS")
knowledge_mastery <- readRDS("knowledge_mastery.RDS")
aggregate_title_info <- readRDS("aggregate_title_info.RDS")


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
      plotOutput("networkPlot"),
      plotOutput("associationPlot"),
      plotOutput("alluvialPlot"),
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
    
    # Calculate the node sizes based on the attribute count
    node_sizes <- high_mastery_students %>%
      group_by(!!sym(attribute)) %>%
      summarise(count = n()) %>%
      rename(node = !!sym(attribute)) %>%
      mutate(size = scales::rescale(count, to = c(8, 20)))
    
    g <- graph_from_data_frame(attribute_knowledge_edges, directed = FALSE)
    
    V(g)$shape <- ifelse(V(g)$name %in% node_sizes$node, "circle", "square")
    V(g)$size <- ifelse(V(g)$name %in% node_sizes$node,
                        node_sizes$size[match(V(g)$name, node_sizes$node)],
                        8)
    V(g)$color <- ifelse(V(g)$name %in% node_sizes$node, 
                         hue_pal()(length(unique(node_sizes$node))), "red")
    
    output$networkPlot <- renderPlot({
      plot(g, vertex.label.cex = 0.8, vertex.size = V(g)$size, vertex.shape = V(g)$shape,
           vertex.color = V(g)$color)
      
      legend("topright", legend = c("Knowledge", "Attribute"), 
             pch = c(15, 16), col = c("red", "blue"), 
             pt.cex = 2, title = "Node Type")
    })
    
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
    
    # Generate the alluvial plot for the character attributes
    alluvial_data <- high_mastery_students %>%
      select(sex, age, major, class) %>%
      group_by(sex, age, major, class) %>%
      summarise(Freq = n(), .groups = 'drop')
    
    output$alluvialPlot <- renderPlot({
      ggplot(alluvial_data,
             aes(axis1 = sex, axis2 = age, axis3 = major, axis4 = class, y = Freq)) +
        geom_alluvium(aes(fill = class)) +
        geom_stratum() +
        geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
        scale_x_discrete(limits = c("Sex", "Age", "Major", "Class"), expand = c(.1, .05)) +
        scale_fill_manual(values = scales::hue_pal()(15)) +
        theme_minimal() +
        ggtitle("Distribution of Character Attributes for High Mastery Students")
    })
    
    # Filter never absolutely correct questions based on high mastery students
    filtered_never_correct <- never_absolutely_correct %>%
      filter(student_ID %in% high_mastery_students$student_ID) %>%
      distinct(student_ID, title_ID) %>%
      group_by(title_ID) %>%
      summarise(number_of_students = n(), .groups = 'drop') %>%
      left_join(aggregate_title_info, by = "title_ID") %>%
      mutate(percentage_of_student_wrong = (number_of_students/length(high_mastery_students$student_ID)*100)) 
    
    output$questionsTable <- renderDataTable({
      datatable(filtered_never_correct)
    })
    
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