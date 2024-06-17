pacman::p_load(shiny,shinydashboard, shinythemes,
               scatterPlotMatrix, parallelPlot,
               cluster, factoextra, tidyverse)

whitewine <- read_csv("data/wine_quality.csv") %>%
  filter(type == "white") %>%
  select(c(1:11))


# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "ShinyIDEA: Interactive Data Exploration and Analysis",
  fluide = TRUE,
  theme = shinytheme("flatly"),
  id = "navbarID",
  tabPanel("Introduction"),
  navbarMenu("Univariate"),
  navbarMenu("Bivariate"),
  navbarMenu("Multivariate")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
