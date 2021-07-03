library(shiny)
library(shinydashboard)

header <- dashboardHeader(
	title = "Outlier Detection"
)

body <- dashboardBody(
	fluidRow(
		box(title = "Information",
			h3("Flagged observations: "),
            h4(paste(readRDS(file = "data/outliers_info.RDS")$flagged, collapse = ", ")),
			h3("Missed outliers: "),
            h4(paste(readRDS(file = "data/outliers_info.RDS")$missed, collapse = ", ")),
            h3("False Outliers: "),
            h4(paste(readRDS(file = "data/outliers_info.RDS")$false, collapse = ", ")),
            plotOutput("my_plot"))
		)
	)

	fluidRow(
		box(title = "Controls",
			numericInput("OBS_ID", "Which observation do you want to highlight?", value = NA)
		)
	)
	
	fluidRow(
		box(
			title = 'Observations',
			plotOutput("my_plot"), height = 250
		)
    )
fluidPage(

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)