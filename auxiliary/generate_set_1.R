# Function to generate a single observation for set 1:
random_dat <- function(grid, slope, out){
    args <- grid
    if(out == 0){
        vals <- runif(n = length(grid), min = 0, max = slope) * slope * args + rnorm(n = length(grid), mean = 0, sd = 0.05)
    }
    else{
        vals <- runif(n = length(grid), min = 0, max = 1.25 * slope) * 1.25 *slope * args + rnorm(n = length(grid), mean = 0, sd = 0.05)
    }
    
    return(list(args = grid,
                vals = vals))
}

# Function to generate the whole set 1:
generate_set_1 <- function(){
    
    # Set seed for reproducibility
    set.seed(17203476)

    # Choose comparratively small number of observations
    n <- 500
    ids <- as.character(1:n)

    # Choose ~5% of obserrvations as outliers
    outliers <- rbinom(n = n, size = 1, prob = 0.05)
    
    # Choose number of measurements for each observation
    lengths <- sample(x = 10:100, size = n, replace = TRUE)
    
    # Find points of measurement for each observation
    # 0 and 1 are part of each grid to ensure identical measuring interval
    grids <- purrr::map(.x = lengths,
                        .f = function(l) c(0, sort(runif(n = l-2, min = 0, max = 1)), 1))
    
    # Create observations
    functions <- map(.x = 1:n,
                     .f = function(i) random_dat(grid = grids[[i]], slope = 1.02, out = outliers[i]))
}



