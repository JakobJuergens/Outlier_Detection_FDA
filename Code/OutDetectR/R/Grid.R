#' This function creates a grid that is used for the piece wise linear
#' approximation of functions. Currently this assumes that the observations
#' share a measuring interval and constructs an equdistant grid of specified
#' length.
#'
#' @param func_dat: Data used by functional procedures
#' @param length.out: number of points in the grid
#'
#' @return A vector that contains the grid points
#' @export
grid_finder <- function(func_dat, length.out = 100) {

  # Extract Measuring Interval from first functional observation
  measuring_interval <- c(min(func_dat[[1]]$args), max(func_dat[[1]]$args))
  # Create Equdistant grid
  grid <- seq(measuring_interval[1], measuring_interval[2], length.out = length.out)
  # Return grid
  return(grid)
}

#' This function approximates the functional observations given to it using a piece
#' wise linear approximation on a specified grid. The format of the data is unchanged
#'
#' @param func_dat: list that contains the observations:
#' each observation is a list, that contains two vectors of identical length: args and vals
#' @param grid: grid to use for approximation
#'
#' @return A list of identical length to func_dat. Each entry corresponds to the 
#' same entry in func_dat but is an approximation using linear interpolation on
#' the points given in grid.
#' @export
grid_approx_set_obs <- function(func_dat, grid) {
  # for each observation perform an approximation on the grid points
  approx_list <- map(
    .x = func_dat,
    .f = function(obs) {
      list(
        args = grid,
        vals = grid_approx_obs(obs$args, obs$vals, grid)
      )
    }
  )
  # return the approximated observations
  return(approx_list)
}

#' This function is a wrapper around grid_approx_set_obs.
#' It takes the approximated observations of identical length and transforms 
#' them into a matrix
#'
#' @param func_dat: list that contains the observations:
#' each observation is a list, that contains two vectors of identical length: args and vals
#' @param grid: grid to use for approximation
#'
#' @return A matrix of dimensions length(func_dat)xlength(grid) that contains 
#' the approximated values of the functional observations at the grid points
#' in each row.
#' @export
grid_approx_mat <- function(func_dat, grid){
  # call approximation in list form
  list_approx <- grid_approx_set_obs(func_dat = func_dat,
                                     grid = grid)
  # extract values
  approx_values <- map(.x = list_approx,
                       .f = function(obs) obs$vals)
  # combine in matrix
  mat_approx <- matrix(data = unlist(approx_values),
                       nrow = length(list_approx), ncols = length(grid),
                       byrow = TRUE)
  # return matrix
  return(mat_approx)
}