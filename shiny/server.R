server <- function(input, output, session) {

  my_tibble <- readRDS(file = "data/shiny_tibble.rds")
    
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