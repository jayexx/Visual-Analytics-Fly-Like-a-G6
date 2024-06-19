pacman::p_load(shiny,shinydashboard, shinythemes,
               scatterPlotMatrix, parallelPlot,
               cluster, factoextra, tidyverse)

whitewine <- read_csv("data/wine_quality.csv") %>%
  filter(type == "white") %>%
  select(c(1:11))

# Define UI for application that draws a histogram
ui <- fluidPage(
)

# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
