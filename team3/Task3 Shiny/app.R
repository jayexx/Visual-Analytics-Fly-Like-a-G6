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
library(colorspace)
library(ggrepel)
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
library(dendextend)
library(corrplot)
library(ggalluvial)
library(entropy)
library(ineq)
library(ggthemes)
library(hrbrthemes)

# Load preprocessed RDS files
# Task 3:

# Task 4:
merged_data <- readRDS("merged_data.RDS")
knowledge_expanded <- readRDS("knowledge_expanded.RDS")
never_absolutely_correct <- readRDS("never_absolutely_correct.RDS")
aggregate_title_info <- readRDS("aggregate_title_info.RDS")
percentage_correct <- readRDS("percentage_correct.RDS")
overall_mastery <- readRDS ("overall_mastery.RDS")
df_IRT <- readRDS ("mirtdata.RDS")




# Ensure class is a factor with ordered levels
class_levels <- paste0("Class", 1:15)
overall_mastery <- overall_mastery %>%
  mutate(class = factor(class, levels = class_levels))

#Shiny app starts here
library(shiny)
library(shinydashboard)
library(shinythemes)

ui <- dashboardPage(skin = 'green',
                    dashboardHeader(title = 'A Statistical Study of Singapore Vehicle Carbon Emissions', titleWidth = 800),
                    
                    dashboardSidebar(
                      sidebarMenu(menuItem('HomePage', tabName = 'HomePage'),
                                  menuItem('Task 1: Knowledge Mastery & Weak links', tabName = 'Task 1: Knowledge Mastery & Weak links'),
                                  menuItem('Task 2: Learners Profile', tabName = 'Task 2: Learners Profile'),
                                  menuItem('Task 3: Learning Mode & Knowledge Acquisition', tabName = 'Task 3: Learning Mode & Knowledge Acquisition'),
                                  menuItem('Task 4: Question Difficulty & Learners Knowledge Level',tabName = 'Task 4: Question Difficulty & Learners Knowledge Level'),
                                  menuItem('Task 5: Recommendations', tabName = 'Task 5: Recommendations')
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
                        
                        
                        
                        tabItem(tabName = 'Task 1: Knowledge Mastery & Weak links',
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
                        
                        tabItem(tabName = 'Task 2: Learners Profile',
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
                        
                        
                        
                        tabItem(tabName = 'Task 3: Learning Mode & Knowledge Acquisition',
                                fluidPage(
                                  titlePanel("Learning Mode & Knowledge Acquisition"),
                                  tabsetPanel(
                                    tabPanel("Vehicle Population",
                                             box(
                                               radioButtons('xcol',
                                                            label = tags$strong('Analyse By:'),
                                                            choices = c('Vehicle Category' = 'category',
                                                                        'Fuel Type' = 'Combined.Type'
                                                            ),
                                                            inline = TRUE)
                                             ),
                                             box(
                                               width = 12,
                                               height = 600,
                                               solidHeader = TRUE,
                                               collapsible = FALSE,
                                               collapsed = FALSE,
                                               plotlyOutput('BarGraph', height = 500)
                                             )
                                    ),
                                    tabPanel("Mileage travelled", 
                                             box(
                                               width = 12,
                                               height = 500,
                                               solidHeader = TRUE,
                                               collapsible = FALSE,
                                               collapsed = FALSE,
                                               plotlyOutput('chart_mileage', height = 300)
                                             )
                                             
                                    ),
                                    
                                    
                                    tabPanel("Vehicle Fuel Economy Distribution",
                                             box(
                                               radioButtons('ycol',
                                                            label = tags$strong('Analyse By:'),
                                                            choices = c('Emissions (g/km)' = 'Combined..CO2...g.km.',
                                                                        'Fuel Economy (l/100km)' = 'Combined..Fuel.Economy...L.100.km.'
                                                            ),
                                                            inline = TRUE)
                                             ),
                                             box(
                                               width = 12,
                                               height = 500,
                                               solidHeader = TRUE,
                                               collapsible = FALSE,
                                               collapsed = FALSE,
                                               plotOutput('BoxPlot', height = 400)
                                             )
                                             
                                             
                                    ),
                                    
                                    
                                    tabPanel("Trend Analysis",
                                             
                                             box(
                                               radioButtons('TAycol',
                                                            label = tags$strong('Analyse By:'),
                                                            choices = c('Total CO2 Emissions (in tonnes)' = 'Total_CO2_Emission_Tonnes',
                                                                        'Total number of Vehicles' = 'number'
                                                                        
                                                            ),
                                                            inline = TRUE)
                                             ),
                                             
                                             box(
                                               
                                               selectInput("category",tags$strong("Choose an category:"), choices = c("All Category",unique(NumofVehbytype6$category)),selected = "Cars"),
                                               selectInput("type", tags$strong("Choose a Fuel type:"), choices = c("All Fuel Type",unique(NumofVehbytype6$type)),selected = "Petrol")
                                               
                                             ),
                                             
                                             
                                             
                                             
                                             
                                             box(
                                               width = 12,
                                               height = 500,
                                               solidHeader = TRUE,
                                               collapsible = FALSE,
                                               collapsed = FALSE,
                                               plotlyOutput('TAPlot', height = 300)
                                             )
                                             
                                             
                                             
                                             
                                    )
                                    
                                  )
                                )
                                ),
                        tabItem(tabName = 'Task 4: Question Difficulty & Learners Knowledge Level',
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
                        
                        tabItem(tabName = 'Task 5: Recommendations',
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
                        
                        )))

server <- function(input, output, session) { 
  
  output$BarGraph <- renderPlotly({
    
       # Define the dynamic variable based on the radio button selection
    if (input$xcol == 'category') {
      analysis <- NumofVehbytype6 %>%
        filter(number != 0, Year != 2023) %>%
        group_by(Year, category) %>% 
        summarise(sum_number = sum(number)/12)
    } else {
      analysis <- NumofVehbytype6 %>%
        filter(number != 0, Year != 2023) %>%
        group_by(Year, Combined.Type) %>% 
        summarise(sum_number = sum(number)/12)
    }
    
    #Generate a bar chart using ggplot
    p1 <- ggplot(analysis, aes_string(x = input$xcol,y = 'sum_number',fill = 'Year')) +
      geom_bar(stat = 'identity',position = 'dodge') + 
      labs(title = paste('Number of Vehicles by ', input$xcol), subtitle = paste('by', input$xcol),
           x = input$xcol, y = 'Number of Vehicles', fill = input$xcol) +
      scale_y_continuous(labels = scales::comma, limits = c(0, 800000)) + 
      scale_fill_manual(values = c("#BBDEFB","#90CAF9","#64B5F6","#42A5F5","#2196F3","#1E88E5","#1976D2","#1565C0" ))
    
    
    ggplotly(p1)
  
  })

  output$BoxPlot <- renderPlot({
    
    # Generate a bar chart using ggplot
    p2 <- ggplot(VehicleFEData, aes_string(x = 'Fuel.Type', y=input$ycol, fill='Fuel.Type')) +
      geom_boxplot(alpha=0.3) +
      theme(legend.position="none") +
      scale_fill_brewer(palette="BuPu") + labs("Distribution of Combined CO2g/km of Different Fuel Types")
      
    
    return(p2)
  })
  
  output$chart_mileage <- renderPlotly({
    
    chart_mileage_plot <- ggplot(df_line_chart_avg_mileage, aes(x = year, y = sum_average)) +
      geom_line(aes(color=category)) + 
      geom_point() + 
      labs(y="Average Mileage (Annual)", x= "Year") + 
      scale_y_continuous(labels = scales::comma, limits = c(1000, 100000))
      # theme_bw()
    
    return(chart_mileage_plot)
    
  })
 
  output$TAPlot <- renderPlotly({
    
    # Filter data based on user inputs
    filtered_data <- reactive({
      df <- Main  
      
      # Filter out data for year 2023
      df <- df[df$Year != 2023, ]
      
      # Filter by category if selected
      if (input$category != "All Category") {
        df <- df[df$category == input$category, ]
      }
      
      # Filter by fuel type if selected
      if (input$type != "All Fuel Type") {
        df <- df[df$type == input$type, ]
      }
      
      return(df)
    })
    
    # Create a scatter plot
    df <- filtered_data()
    
    plot <- plot_ly(data = df, x = ~month)
    
    # Get unique combinations of Category and type
    unique_combinations <- unique(paste(df$category, df$type, sep = "_"))
    
    for (comb in unique_combinations) {
      filtered_df <- df[paste(df$category, df$type, sep = "_") == comb, ]
      
      if (input$TAycol == "Total_CO2_Emission_Tonnes") {
        
        # Connect scatter points with lines for each combination
        plot <- plot %>% add_lines(data = filtered_df,
                                   y = ~Total_CO2_Emission_Tonnes, 
                                   name = comb,
                                   line = list(shape = "linear", color = comb))
        
      } else if (input$TAycol == "number") {
        
        # Connect scatter points with lines for each combination
        plot <- plot %>% add_lines(data = filtered_df,
                                   y = ~number, 
                                   name = comb,
                                   line = list(shape = "linear", color = comb))
      }
    }
    
    # Create a title based on user selections
    title_text <- paste(input$TAycol, "of", 
                        paste(input$category, collapse = ", "),
                        "and with",
                        paste(input$type, collapse = ", "))
    
    plot <- plot %>% 
      layout(title = title_text, 
             xaxis = list(title = "Year"),
             yaxis = list(title = input$TAycol))  # Update y-axis title based on input
    
    return(plot)
  })
  
  output$ScatterPlot <- renderPlot({
    
    # Filter data based on user inputs
    filtered_data1 <- reactive({
      df1 <- VehicleFEData 
      
      # Filter by Body type if selected
      if (input$Body_Type != "All Body Type") {
        df1 <- df1[df1$Body.Type == input$Body_Type, ]
      }
      
      # Filter by fuel type if selected
      if (input$Fuel_Type != "All Fuel Type") {
        df1 <- df1[df1$Fuel.Type == input$Fuel_Type, ]
      }
      
      return(df1)
    })
    
    df1 <- filtered_data1()
    
    # Create a new variable that combines Body Type and Fuel Type
    df1$Combined_Category <- paste(df1$Body.Type, df1$Fuel.Type, sep = " - ")
    
    # Calculate R-squared
    fit <- lm(Combined..Fuel.Economy...L.100.km. ~ Combined..CO2...g.km., data = df1)
    r_squared <- summary(fit)$r.squared
    
    # Create a plot
    p <- ggplot(df1, aes(x = Combined..CO2...g.km., y = Combined..Fuel.Economy...L.100.km., color = Combined_Category)) +
      geom_point() +
      scale_color_discrete(name = "Combined Category") +
      
      # Add R-squared label to the plot
      geom_text(aes(x = max(df1$Combined..CO2...g.km.) - 1, 
                    y = max(df1$Combined..Fuel.Economy...L.100.km.) - 1,
                    label = paste("R-squared =", r_squared)),
                hjust = 1, vjust = 1, size = 4, color = "black")
    
    print(p)
  })
  
  # Create the linear regression models (replace with your actual models)
  fit10 <- lm(Total_CO2_Emission_Tonnes4 ~ Mileage4, data = my_data)
  fit11 <- lm(Total_CO2_Emission_Tonnes4 ~ Population4, data = my_data)
  fit12 <- lm(Total_CO2_Emission_Tonnes4 ~ Fuel_Economy4, data = my_data)
  fit13 <- lm(Total_CO2_Emission_Tonnes4 ~ Percentage_Elec_hybrid, data = my_data)
  
  # Plot 1: Total CO2 Emissions vs Mileage
  output$plot1 <- renderPlot({
    plot(my_data$Mileage4, my_data$Total_CO2_Emission_Tonnes4, type = "p", main = "Total CO2 Emissions vs Mileage", 
         xlab = "Mileage (KM)", ylab = "Total CO2 Emissions (Tonnes)", col = 'red', cex = 1.2)
    abline(fit10, col = 'blue', lty = 'dashed')
  })
  
  # Plot 2: Total CO2 Emissions vs Fuel Economy
  output$plot2 <- renderPlot({
    plot(my_data$Fuel_Economy4, my_data$Total_CO2_Emission_Tonnes4, type = "p", main = "Total CO2 Emissions vs Fuel Economy", 
         xlab = "Fuel Economy (KM/L)", ylab = "Total CO2 Emissions (Tonnes)", col = 'red', cex = 1.2)
    abline(fit12, col = 'blue', lty = 'dashed')
  })
  
  # Plot 3: Total CO2 Emissions vs Population
  output$plot3 <- renderPlot({
    plot(my_data$Population4, my_data$Total_CO2_Emission_Tonnes4, type = "p", main = "Total CO2 Emissions vs Population", 
         xlab = "Vehicle Population", ylab = "Total CO2 Emissions (Tonnes)", col = 'red', cex = 1.2)
    abline(fit11, col = 'blue', lty = 'dashed')
  })
  
  # Plot 4: Total CO2 Emissions vs Percentage of Electric & Hybrid Vehicles
  output$plot4 <- renderPlot({
    plot(my_data$Percentage_Elec_hybrid, my_data$Total_CO2_Emission_Tonnes4, type = "p", main = "Total CO2 Emissions vs Percentage of Population Electric & Hybrid Vehicles", 
         xlab = "Percentage of Vehicle Population (Electric & Hybrid Vehicles)", ylab = "Total CO2 Emissions (Tonnes)", col = 'red', cex = 1.2)
    abline(fit13, col = 'blue', lty = 'dashed')
  })



  # Observe changes in the monthSlider input
  
  observe({
    # Convert the selected month from the slider to a POSIXct date
    selectedMonth <- as.POSIXct(input$monthSlider, origin = "1970-01-01")
    
    # Calculate the start and end date for the selected month
    start_date <- floor_date(selectedMonth, unit = "month")
    end_date <- ceiling_date(selectedMonth, unit = "month") - 1
    
    cat("Selected month:", selectedMonth, "\n")
    cat("Start date:", start_date, "\n")
    cat("End date:", end_date, "\n")
    
    # Filter data for the selected month
    filtered_data <- Main[Main$MonthYear >= start_date & Main$MonthYear <= end_date, ]
    
    # Print the number of rows in the filtered data
    cat("Number of rows in filtered data:", nrow(filtered_data), "\n")
    
    # Create a ggplot for the total CO2 emissions
    p <- ggplot(filtered_data, aes(x = Total_CO2_Emission_Tonnes, y = category, fill = type)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Total CO2 Emissions for", format(selectedMonth, format = "%b %Y")),
           x = "Category",
           y = "Total CO2 Emission (Tonnes)") +
      scale_fill_manual(values = c(
        "CNG" = "#FF5733",
        "Diesel" = "#1F77B4",
        "Diesel-Electric" = "#2CA02C",
        "Diesel-Electric (Plug-In)" = "#FFD700",
        "Electric" = "#FFA500",
        "Petrol" = "#D55E00",
        "Petrol-CNG" = "#DCB61D",
        "Petrol-Electric" = "#808080",
        "Petrol-Electric (Plug-In)" = "#AA9D7A"
      ))
    
    # Set the fixed x-axis scale (replace min_x and max_x with your desired values)
    p <- p + coord_cartesian(xlim = c(0, 350000))
          
    
    output$animatedGraph <- renderPlot({
      print(p)
    })
  })
 
  # Define a reactive expression to compute the annual CO2 emissions
  annualCO2Emissions <- reactive({
    data <- Main  # Replace 'data' with your actual data frame
    
    # Extract the year from the 'MonthYear' column
    data$Year <- format(data$MonthYear, "%Y")
    
    # Filter out data for year 2023
    data <- data[data$Year != "2023", ]
    
    # Group the data by 'Year' and calculate the total sum of CO2 emissions
    summary_data <- data %>%
      group_by(Year) %>%
      summarise(Total_CO2_Emission_Tonnes = sum(Total_CO2_Emission_Tonnes))
    
    return(summary_data)
  })
  
  # Define the output for the CO2 emissions table
  output$co2EmissionsTable <- renderDataTable({
    datatable(annualCO2Emissions(),
              width = 500,
              height = 500) %>%
      formatRound(
        columns = 2,  # Apply rounding to the second column (Total_CO2_Emission_Tonnes)
        digits = 0  # Set the number of decimal places to 0
      )
  })
  
  }

shinyApp(ui, server)
