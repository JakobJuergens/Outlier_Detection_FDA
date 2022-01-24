#' This function takes a functional observation func_obs and stretches it to
#' fit a given measuring interval
#'
#' @param func_obs: a list that contains two vectors of identical length: args and vals
#' @param measuring_interval: a vector with 2 elements,
#' the start and end points of the desired measuring interval
#'
#' @return A functional observation in the usual format with the args element of
#' the list stretched to fit the chosen measuring interval
#' @export
stretch_obs <- function(func_obs, measuring_interval) {


  # calculate stretching factor
  phi <- {
    (measuring_interval[2] - measuring_interval[1]) /
      (max(func_obs$args) - min(func_obs$args))
  }

  # stretch arguments by appropriate factor
  args_stretched <- func_obs$args * phi

  # return in the format for functional observations
  return(list(
    args = args_stretched,
    vals = func_obs$vals
  ))
}

#' This function is a wrapper around stretch_obs. It takes a set of functional
#' observations func_dat and stretches each observation to fit a given measuring interval
#'
#' @param func_dat: list that contains the observations
#' each observation is a list, that contains two vectors of identical length: args and vals
#' @param measuring_interval: a vector with 2 elements,
#' the start and end points of the desired measuring interval
#'
#' @return A list of functional observations in the usual format with the args element of
#' each observation stretched to fit the chosen measuring interval
#' @export
stretch_data <- function(func_dat, measuring_interval) {

  # stretch all observations
  stretch_dat <- map(
    .x = func_dat,
    .f = function(func_obs) stretch_obs(func_obs, measuring_interval)
  )

  # return in the format for functional observations
  return(stretch_dat)
}
