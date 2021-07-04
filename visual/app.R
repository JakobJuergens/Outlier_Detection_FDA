library(shiny)
library(shinydashboard)
library(tidyverse)

outliers_info <- readRDS(file = "data/outliers_info.rds")
my_tibble <- readRDS(file = "data/shiny_tibble.rds")

num_observations <- length(unique(my_tibble$id))

header <- dashboardHeader(
  title = "Outlier Detection"
)

sidebar <- dashboardSidebar(
  sliderInput("OUT_THR", "Observations over which certainty threshold shall be marked as outliers?", min = 0, max = 1, value = 0.5, step = 0.01),
  sliderInput("CERT_THR", "Show observations with certainty values over this threshold:", min = 0, max = 1, value = 0.5, step = 0.01),
  numericInput("OBS_ID", "Which observation do you want to highlight?", value = NA)
)

body <- dashboardBody(
  column(width = 12,
  fluidRow(width = 12,
           column(width = 7,
                  box(title = "Description", 
                      width = 9, h4("This dashboard visualizes the method of outlier detection used in the project. \n"))),
           column(width = 5,
                  infoBox("Observations", num_observations, icon = icon("list")),
                  infoBox("Flagged", textOutput("nflagged"), icon = icon("list")))
           )
  ),
  fluidRow(width = 12,
                  box(width = 12,
                  title = "Plotted Observations",
                  plotOutput("my_plot"))
  ),
  fluidRow(width = 12,
     column(12,
            tabBox(width = 12,
                   title = 'Information',
                   tabPanel("Flagged Observations", 
                    h3("Flagged observations: "),
                    h4(textOutput("flagged")),
                   ),
                   tabPanel("Missed Outliers", 
                            h3("Missed Outliers: "),
                            h4(textOutput("missed")),
                   ),
                   tabPanel("False Outliers:", 
                            h3("False outliers: "),
                            h4(textOutput("false")),
                   )
            )      
      )
    )
)

  
ui <- dashboardPage(
    header,
    sidebar,
    body
  )

server <- function(input, output, session) {
  
  show_which <- reactive({
    CERT_THR <- input$CERT_THR
    show_which <- which(my_tibble$cert >= CERT_THR)
    show_which
  })
  
  out_which <- reactive({
    OUT_THR <- input$OUT_THR
    out_which <- unique(my_tibble$id[which(my_tibble$cert >= OUT_THR)])
    out_which
  })
  
  alphas <- reactive({
    tmp <- rep(0.1, times = dim(my_tibble)[1])
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
  
  output$flagged <- renderText(out_which())
  output$nflagged <- renderText(length(out_which()))
  output$missed <- renderText(setdiff(outliers_info$original, out_which()))
  output$false <- renderText(setdiff(out_which(), outliers_info$original))
  
  output$my_plot <- renderPlot({
    
    plot_tibble <- my_tibble[show_which(),]
    
    a <- alphas()
    s <- sizes()
    
    a <- a[show_which()]
    s <- s[show_which()]
    
    ggplot(data = plot_tibble) +
      geom_line(aes(x = args, y = vals, col = cert, group = id, alpha = a), size = s) +
      scale_color_gradient(low = "#0062ff", high = "#ff0000") +
      theme(text=element_text(size=16, family="Serif")) +
      guides(alpha = FALSE) 
  })
}

shinyApp(ui, server)