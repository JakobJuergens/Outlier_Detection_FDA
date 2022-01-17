# bring into tidy format                     
tidify_1 <- function(data_set, ids){
    
    # extract arguments
    x_1 <- unlist(map(.x = data_set,
                      .f = function(fun) fun$args))
                      
    # extract values                          
    y_1 <- unlist(map(.x = data_set,
                      .f = function(fun) fun$vals))
    # calculate lengths                      
    len <- unlist(map(.x = data_set,
                      .f = function(fun) length(fun$args))) 
                      
    # make fitting repetitions of ids                      
    ids <- unlist(map(.x = 1:500,
                      .f = function(i) rep(ids[i], times = len[i])))                          
    
    # make into tibble                      
    tibbled <-  tibble(x = x_1, 
                       y = y_1,
                       ids = ids)  
                      
    return(tibbled)
}

vis_1 <- function(tidy_1){
    p <- ggplot(data = tidy_1) +
            ggtitle("Data Set 1") +
            geom_line(aes(x = x, y = y, group = ids), col = "blue", alpha = 0.1) +
            theme_light() +
            theme(plot.title = element_text(size=24),
                  axis.title.x = element_text(size=18),
                  axis.title.y = element_text(size=18))
    
    p
}
                      
# Create and save tibble for shiny app

shinyfy_1 <- function(tidy_1){
    
    shiny_tibble <- cbind(tidy_1)
}                      
