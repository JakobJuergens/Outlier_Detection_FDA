#' placeholder
#'
#'@param start: start point of the grid
#'@param end: end point of the grid
#'
#'@return placeholder
#'@export
grid_finder <- function(func_dat){
  measuring_interval <- c(min(func_dat[[1]]$args), max(func_dat[[1]]$args))
  return(seq(measuring_interval[1], measuring_interval[2], length.out = 30))
}

#' placeholder
#'
#'@param func_dat: list that contains the observations
#' each observation is a list, that contains two vectors of identical length: args and vals
#'@param grid: grid to use for approximation
#'
#'@return placeholder
#'@export
grid_approx_set_obs <- function(func_dat, grid) {
  res_mat <- matrix(data = unlist(
    map(.x = func_dat,
        .f = function(obs) grid_approx_obs(obs$args, obs$vals, grid))
  ), nrow = length(func_dat), byrow = TRUE)
  
  return(res_mat)
}