library(shiny)
library(dplyr)
library(igraph)
library(DT)
library(scales)
library(ggstatsplot)

# Load saved RDS files
attempt_scores <- readRDS("attempt_scores.RDS")
domain_mastery <- readRDS("domain_mastery.RDS")
title_info <- readRDS("title_info.RDS")

# Ensure `domain` is character type
domain_mastery <- domain_mastery %>%
  mutate(domain = as.character(domain))

merged_data <- readRDS("merged_data.RDS") %>%
  mutate(domain = as.character(domain))

# Define UI
ui <- fluidPage(
  titlePanel("Student Mastery Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("threshold", "Select Threshold:", 
                  choices = c("25%", "50%", "75%", "> 0"), selected = "75%"),
      actionButton("filter", "Filter High Mastery Students")
    ),
    mainPanel(
      plotOutput("networkPlot"),
      plotOutput("associationPlot"),
      dataTableOutput("questionsTable")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  observeEvent(input$filter, {
    threshold <- input$threshold
    high_mastery_students <- get_high_mastery_students(domain_mastery, threshold)
    
    # Summarize student_ID based on their class and show the connection of Class & Domain
    class_domain_edges <- high_mastery_students %>%
      select(class, domain) %>%
      distinct() %>%
      group_by(class, domain) %>%
      summarise(student_count = n(), .groups = 'drop')
    
    g <- graph_from_data_frame(class_domain_edges, directed = FALSE)
    
    V(g)$shape <- ifelse(V(g)$name %in% unique(class_domain_edges$class), "circle", "square")
    V(g)$size <- ifelse(V(g)$name %in% unique(class_domain_edges$class),
                        10 + 2 * class_domain_edges %>%
                          group_by(class) %>%
                          summarise(count = sum(student_count)) %>%
                          pull(count), 
                        10)
    V(g)$color <- ifelse(V(g)$name %in% unique(class_domain_edges$class), 
                         hue_pal()(length(unique(class_domain_edges$class))), "red")
    
    output$networkPlot <- renderPlot({
      plot(g, vertex.label.cex = 0.8, vertex.size = V(g)$size, vertex.shape = V(g)$shape,
           vertex.color = V(g)$color)
    })
    
    # Generate the ggstatsplot for the association between class and domain
    association_plot <- ggstatsplot::ggbarstats(
      data = high_mastery_students,
      x = domain,
      y = class,
      paired = TRUE,
      label = "both",
      title = "Association between Class and Domain"
    )
    
    output$associationPlot <- renderPlot({
      association_plot
    })
    
    
    # Determine Never_correct for questions within mastered domains
    never_correct_questions <- merged_data %>%
      inner_join(high_mastery_students, by = c("student_ID", "domain", "class"), relationship = "many-to-many") %>%
      group_by(student_ID, title_ID, domain) %>%
      summarise(total_points = sum(points), attempts = n(), .groups = 'drop') %>%
      mutate(Never_correct = if_else(total_points == -attempts, "yes", "no")) %>%
      filter(Never_correct == "yes")%>%
      ungroup() %>%
      group_by(title_ID) %>%
      summarise (count = n())
    
    # Filter questions never correct by high mastery students within their mastered domains
    filtered_questions <- never_correct_questions %>%
      left_join(title_info, by = c("title_ID" = "title_ID"))
    
    output$questionsTable <- renderDataTable({
      datatable(filtered_questions %>% select(title_ID, domain, score, knowledge))
    })
  })
}

get_high_mastery_students <- function(domain_mastery, threshold) {
  if (threshold == "> 0") {
    high_mastery_students <- domain_mastery %>%
      filter(average_score > 0)
  } else {
    percentile <- as.numeric(sub("%", "", threshold)) / 100
    threshold_score <- quantile(domain_mastery$average_score, percentile)
    high_mastery_students <- domain_mastery %>%
      filter(average_score > threshold_score)
  }
  return(high_mastery_students)
}

# Run the application
shinyApp(ui, server)
