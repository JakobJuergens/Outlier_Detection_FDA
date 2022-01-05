#' Placeholder
#'
#' @param grid: Grid where the observations take palce
#' @param slope: Slope parameter for the generated curves
#' @param out: Vector conatining zeroes where no outlier should be generated 
#' and ones where an outlier is generated
#'
#' @return Placeholder
random_dat_1 <- function(grid, slope, out) {
  args <- grid
  if (out == 0) {
    vals <- runif(n = length(grid), min = 0.8, max = 1.2) * slope * args + rnorm(n = length(grid), mean = 0, sd = 0.05)
  } else {
    vals <- runif(n = length(grid), min = 1, max = 1.4) * 1.2 * slope * args + rnorm(n = length(grid), mean = 0, sd = 0.1)
  }

  return(list(
    args = grid,
    vals = vals
  ))
}

#' Placeholder
#'
#' @param n_obs: number of observations to be generated
#'
#' @return Placeholder
#' @export
generate_set_1 <- function(n_obs = 500) {

  # Set seed for reproducibility
  set.seed(17203476)

  # Choose comparratively small number of observations
  n <- n_obs
  ids <- as.character(1:n)

  # Choose ~5% of observations as outliers
  outliers <- rbinom(n = n, size = 1, prob = 0.05)

  # Choose number of measurements for each observation
  lengths <- sample(x = 10:100, size = n, replace = TRUE)

  # Find points of measurement for each observation
  # 0 and 1 are part of each grid to ensure identical measuring interval
  grids <- purrr::map(
    .x = lengths,
    .f = function(l) c(0, sort(runif(n = l - 2, min = 0, max = 1)), 1)
  )

  # Create observations
  functions <- map(
    .x = 1:n,
    .f = function(i) random_dat_1(grid = grids[[i]], slope = 1.02, out = outliers[i])
  )

  # return list of objects of interest
  return(list(data = functions, ids = ids, outliers = which(outliers == 1)))
}

#' Placeholder
#'
#' @param grid: Grid where the observations take palce
#' @param slope: Slope parameter for the generated curves
#' @param out: Vector conatining zeroes where no outlier should be generated 
#'
#' @return Placeholder
random_dat_2 <- function(grid, slope, out){
  args <- grid
  if(out == 0){
    vals <- runif(n = length(grid), min = 0.8, max = 1.2) * slope * args + rnorm(n = length(grid), mean = 0, sd = 0.05)
  }
  else if(out == 1){
    vals <- runif(n = length(grid), min = 1, max = 1.4) * 1.2 * slope * args + rnorm(n = length(grid), mean = 0, sd = 0.1)
  }
  else if(out == 2){
    vals <- 1 / (1 + exp(-3*(args - 0.5))) + rnorm(n = length(grid), mean = 0, sd = 0.05)
  }
  else if(out == 3){
    vals <- 2 / (1 + exp(-3*args)) - 1 + rnorm(n = length(grid), mean = 0, sd = 0.05)
  }
  else if(out == 4){
    vals <- (exp(args) - 1) / 1.7183 + rnorm(n = length(grid), mean = 0, sd = 0.05)
  }
  else{
    vals <- runif(n = length(grid))
  }
  
  return(list(args = grid,
              vals = vals))
}

#' Placeholder
#'
#' @param n_obs: number of observations to be generated
#'
#' @return Placeholder
#' @export
generate_set_2 <- function(n_obs = 10000){
  
  # Set seed for reproducibility
  set.seed(17203476)
  
  # Choose comparatively large number of observations
  n <- n_obs
  ids <- as.character(1:n)
  
  # Choose ~5% of observations as outliers
  outliers <- sample(x = 0:5, size = n, prob = c(0.95, rep(0.01, times = 5)), replace = TRUE)
  
  # Choose number of measurements for each observation
  lengths <- sample(x = 10:100, size = n, replace = TRUE)
  
  # Find points of measurement for each observation
  # 0 and 1 are part of each grid to ensure identical measuring interval
  grids <- purrr::map(.x = lengths,
                      .f = function(l) c(0, sort(runif(n = l-2, min = 0, max = 1)), 1))
  
  # Create observations
  functions <- map(.x = 1:n,
                   .f = function(i) random_dat_2(grid = grids[[i]], slope = 1.02, out = outliers[i]))
  
  return(list(data = functions, ids = ids, outliers = which(outliers != 0)))                 
}

#' Placeholder
#'
#' @param grid: Grid where the observations take palce
#' @param slope: Slope parameter for the generated curves
#' @param out: Vector conatining zeroes where no outlier should be generated 
#'
#' @return Placeholder
random_dat_3 <- function(grid, slope, out){
  args <- grid
  if(out == 0){
    vals <- runif(n = length(grid), min = 0.8, max = 1.2) * slope * args + rnorm(n = length(grid), mean = 0, sd = 0.05)
  }
  else if(out == 1){
    vals <- runif(n = length(grid), min = 1, max = 1.4) * 1.2 * slope * args + rnorm(n = length(grid), mean = 0, sd = 0.1)
  }
  else if(out == 2){
    vals <- (slope*max(args)) / (1 + exp(-3*(args - 0.5))) + rnorm(n = length(grid), mean = 0, sd = 0.05)
  }
  else if(out == 3){
    vals <- (slope*max(args)) * (2 / (1 + exp(-3*args)) - 1) + rnorm(n = length(grid), mean = 0, sd = 0.05)
  }
  else if(out == 4){
    vals <- (slope*max(args)) * (exp(args) - 1) / (exp(max(args)) - 1) + rnorm(n = length(grid), mean = 0, sd = 0.05)
  }
  else{
    vals <- runif(n = length(grid), min = 0, max = (slope*max(args)))  
  }
  
  return(list(args = grid,
              vals = vals))
}

#' Placeholder
#'
#' @param n_obs: number of observations to be generated
#'
#' @return Placeholder
#' @export
generate_set_3 <- function(n_obs = 30000){
  
  # Set seed for reproducibility
  set.seed(17203476)
  
  # Choose comparatively large number of observations
  n <- n_obs
  ids <- as.character(1:n)
  
  # Choose ~5% of observations as outliers
  outliers <- sample(x = 0:5, size = n, prob = c(0.95, rep(0.01, times = 5)), replace = TRUE)
  
  # Choose number of measurements for each observation
  lengths <- sample(x = 10:100, size = n, replace = TRUE)
  
  # Choose interval endpoint for each observation (make discrete set for ease of use)
  end_points <- sample(x = c(0.9, 1, 1.1, 1.5, 1.6, 1.7, 1.9, 2, 2.1), 
                       prob = c(0.05, 0.2, 0.05, 0.07, 0.15, 0.08, 0.1, 0.25, 0.05),
                       size = n,
                       replace = TRUE)
  
  # Find points of measurement for each observation
  # 0 and 1 are part of each grid to ensure identical measuring interval
  grids <- purrr::map(.x = lengths,
                      .f = function(l) c(0, sort(runif(n = l-2, min = 0, max = end_points[l])), end_points[l]))
  
  # Create observations
  functions <- map(.x = 1:n,
                   .f = function(i) random_dat_3(grid = grids[[i]], slope = 1.02, out = outliers[i]))
  
  # Save data in random access format
  saveList(functions, "./data/Set_3/functional.llo")
  saveRDS(ids, "./data/Set_3/ids.RDS")
  saveRDS(which(outliers != 0), "./data/Set_3/outliers.RDS")
  return(list(data = functions, ids = ids, outliers = which(outliers != 0)))
}