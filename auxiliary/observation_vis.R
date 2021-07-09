obs_vis <- function(){
    set.seed(12345)

    ex_tibble <- tibble(x = sort(runif(20, 2, 8)), 
                    y = 1.02*x + rnorm(20, sd = 0.2))

    ggplot(data = ex_tibble, aes(x = x, y = y)) +
        geom_point(fill = "red", col = "blue", shape = 23, size = 5) +
        geom_line(col = "blue", size = 1) +
        xlim(0, 10) + ylim(-1, 9) +
        xlab("Angle") + ylab("Torque") +
        ggtitle("One Observation") +
        geom_vline(aes(xintercept = min(x))) +
        geom_vline(aes(xintercept = max(x))) +
        geom_segment(aes(x = min(x), xend = max(x), y = -1, yend = -1), col = "green", arrow = arrow()) +
        geom_segment(aes(x = max(x), xend = min(x), y = -1, yend = -1), col = "green", arrow = arrow()) +
        annotate("text", x = 5, y = -0.5, label = "Measuring Interval", size = 8) +
        annotate("rect", xmin = 4, xmax = 6, ymin = -1, ymax = 0, fill = "green", alpha = 0.2) +
        theme_light() +
        theme(plot.title = element_text(size=24),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))
}

zeroing_vis <- function(){
    tibble_1 <- tibble(x = c(sort(runif(15, 1.5, 5)),
                             sort(runif(15, 0.5, 3)),
                             sort(runif(15, 1, 7))),
                       x_min = c(rep(x[1], times = 15),
                                 rep(x[16], times = 15),
                                 rep(x[31], times = 15)),
                       y = 1.2 * (x-x_min) + rnorm(45, sd = 0.2),
                       x_max = c(rep(x[15], times = 15),
                                 rep(x[30], times = 15),
                                 rep(x[45], times = 15)),
                       type = c(rep('A', times = 15),
                                rep('B', times = 15),
                                rep('C', times = 15)),
                       arrow_height = c(rep(0, times = 15),
                                        rep(-0.5, times = 15),
                                        rep(-1, times = 15)),
                      shift = rep(FALSE, times = 45))
    
    tibble_2 <- tibble(x = tibble_1$x - tibble_1$x_min,
                       y = tibble_1$y,
                       x_min = rep(0, times = 45),
                       x_max = tibble_1$x_max - tibble_1$x_min,
                       type = c(rep('A', times = 15),
                                rep('B', times = 15),
                                rep('C', times = 15)),
                       arrow_height = tibble_1$arrow_height,
                      shift = rep(TRUE, times = 45))
    
    my_tibble <- rbind(tibble_1, tibble_2)
    
    p <- ggplot(data = my_tibble) +
            geom_point(aes(x = x, y = y, fill = type), col = "blue", shape = 23, size = 5) +
            ggtitle("Zeroing") +
            xlab("Angle") + ylab("Torque") +
            xlim(0, 7) + ylim(-1.5, 7) +
            geom_vline(aes(xintercept = x_min, colour = type)) +
            geom_vline(aes(xintercept = x_max, colour = type)) +
            geom_segment(aes(x = x_min, xend = x_max, y = arrow_height, yend = arrow_height, col = type), arrow = arrow()) +
            geom_segment(aes(x = x_max, xend = x_min, y = arrow_height, yend = arrow_height, col = type), arrow = arrow()) +
            theme_light() +
            theme(plot.title = element_text(size=24),
                  axis.title.x = element_text(size=18),
                  axis.title.y = element_text(size=18)) + 
            transition_states(shift, transition_length = 1, state_length = 1)
    
    animate(p, nframes = 400, fps = 40, renderer = gifski_renderer(file = './material/zero.gif'),
            height = 500, width = 1000)
    
}


stretching_vis <- function(){
    set.seed(12345)
    
    tibble_1 <- tibble(x = sort(runif(20, 0, 5)), 
                       x_min = min(x),
                       y = 1.02*(x-x_min) + rnorm(20, sd = 0.2),
                       x_max = max(x))
    
    x_1_interval <- tibble_1$x_max[1] - tibble_1$x_min[1]
    
    tibble_2u <- tibble(x = sort(runif(20, 0, 4)),
                        type = "non stretched",
                        x_min = min(x),
                        y = 1.03*(x - x_min) + rnorm(20, sd = 0.2),
                        x_max = max(x))
    
    x_2_interval <- tibble_2u$x_max[1] - tibble_2u$x_min[1]
    
    stretch_fac = x_1_interval / x_2_interval
    
    tibble_2s <- tibble(x = (tibble_2u$x - tibble_2u$x_min + tibble_1$x_min) * stretch_fac,
                        y = tibble_2u$y,
                        type = "stretched",
                        x_min = min(x),
                        x_max = max(x))
    
    tibble_2 <- rbind(tibble_2u, tibble_2s)
    
    
    p <- ggplot(data = tibble_2, aes(x = x, y = y)) +
            geom_point(aes(x = x, y = y), fill = "green", col = "blue", shape = 23, size = 5) +
            geom_point(data = tibble_1, aes(x = x, y = y), fill = "red", col = "blue", shape = 23, size = 5) +
            ggtitle("Acceptable Stretching") +
            xlab("Angle") + ylab("Torque") +
            xlim(0, 7) + ylim(-1.5, 7) +
            geom_vline(aes(xintercept = x_min), col = "green") +
            geom_vline(aes(xintercept = x_max), col = "green") +
            geom_vline(data = tibble_1, aes(xintercept = x_min), col = "red") +
            geom_vline(data = tibble_1, aes(xintercept = x_max), col = "red") +
            geom_segment(aes(x = x_min, xend = x_max, y = -0.8, yend = -0.8), col = "green", arrow = arrow()) +
            geom_segment(aes(x = x_max, xend = x_min, y = -0.8, yend = -0.8), col = "green", arrow = arrow()) +
            geom_segment(data = tibble_1, aes(x = x_min, xend = x_max, y = -1.2, yend = -1.2), col = "red", arrow = arrow()) +
            geom_segment(data = tibble_1, aes(x = x_max, xend = x_min, y = -1.2, yend = -1.2), col = "red", arrow = arrow()) +
            theme_light() +
            theme(plot.title = element_text(size=24),
                  axis.title.x = element_text(size=18),
                  axis.title.y = element_text(size=18)) + 
            transition_states(type, transition_length = 1, state_length = 1)
    
    animate(p, nframes = 400, fps = 40, renderer = gifski_renderer(file = './material/stretch.gif'),
            height = 500, width = 1000)
        
}