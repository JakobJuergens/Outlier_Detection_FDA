library(shiny)
library(shinydashboard)
library(tidyverse)

header <- dashboardHeader(
  title = "Outlier Detection"
)

outliers_info <- readRDS(file = "data/outliers_info.rds")

body <- dashboardBody(
  fluidRow(
    box(title = "Information",
        h3("Flagged observations: "),
        h4(paste(outliers_info$flagged, collapse = ", ")),
        h3("Missed outliers: "),
        h4(paste(outliers_info$missed, collapse = ", ")),
        h3("False Outliers: "),
        h4(paste(outliers_info$false, collapse = ", "))),
    box(title = "Controls",
      numericInput("OBS_ID", "Which observation do you want to highlight?", value = NA))
  ),
  fluidRow(
    box(title = 'Observations',
        plotOutput("my_plot"), height = 250)
  )
)
  
ui <- dashboardPage(
    header,
    dashboardSidebar(disable = TRUE),
    body
 )

my_tibble <- readRDS(file = "data/shiny_tibble.rds")

server <- function(input, output, session) {
    
  alphas <- reactive({
    tmp <- rep(0.01, times = dim(my_tibble)[1])
    focus <- which(my_tibble$id == input$OBS_ID)
    tmp[focus] <- 1
    tmp
  })
    
  sizes <- reactive({
    tmp <- rep(0.1, times = dim(my_tibble)[1])
    focus <- which(my_tibble$id == input$OBS_ID)
    tmp[focus] <- 0.4
    tmp
  })
  
  output$my_plot <- renderPlot({
      
    a <- alphas()
    s <- sizes()
    
    ggplot(data = my_tibble) +
        geom_line(aes(x = args, y = vals, col = cert, group = id, alpha = a), size = s) +
        scale_color_gradient(low = "#0062ff", high = "#ff0000") +
        theme(text=element_text(size=16, family="Serif")) +
        guides(alpha = FALSE) 
  })
}

shinyApp(ui, server)