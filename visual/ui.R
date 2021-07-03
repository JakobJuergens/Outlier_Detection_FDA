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
  )
  fluidRow(
    box(title = 'Observations',
        plotOutput("my_plot"), height = 250)
  )
)
  
dashboardPage(
    header,
    dashboardSidebar(disable = TRUE),
    body
 )