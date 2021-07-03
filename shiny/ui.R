ui <- fluidPage(
  numericInput("OBS_ID", "Which observation do you want to highlight?", value = NA),
  verbatimTextOutput("Observations"),
  mainPanel(h1("Outlier Detection"),
            h3("Flagged observations: "),
            h4(paste(readRDS(file = "data/outliers_info.RDS")$flagged, collapse = ", ")),
            h3("Missed outliers: "),
            h4(paste(readRDS(file = "data/outliers_info.RDS")$missed, collapse = ", ")),
            h3("False Outliers: "),
            h4(paste(readRDS(file = "data/outliers_info.RDS")$false, collapse = ", ")),
            plotOutput("my_plot"))
)