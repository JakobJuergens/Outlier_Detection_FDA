# bring into tidy format                     
tidify_3 <- function(data_set, ids){
    
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
    ids <- unlist(map(.x = 1:10000,
                      .f = function(i) rep(ids[i], times = len[i]))) 
    
    # make into tibble                      
    tibbled <-  tibble(x = x_1, 
                       y = y_1,
                       ids = ids)  
                      
    return(tibbled)
}
                      
vis_3 <- function(tidy_3){
    p <- ggplot(data = tidy_3) +
            ggtitle("Data Set 3") +
            geom_line(aes(x = x, y = y, group = ids), col = "blue", alpha = 0.1) +
            theme_light() +
            theme(plot.title = element_text(size=24),
                  axis.title.x = element_text(size=18),
                  axis.title.y = element_text(size=18))
    
    ggsave(plot = p, filename = "./material/set_3.png", width = 50, height = 15, units = "cm")
    
    p
}
