#' This function zeroes an observation by shifting the arg values to begin at zero.
#'
#'@param func_obs: a list, that contains two vectors of identical length: args and vals
#'
#'@return a list, that contains two vectors of identical length: args and vals,
#' where args begins with zero.
#'@export
zero_obs <- function(func_obs){
  # create zeroed observation in usual format
  zeroed_func_obs <- list(args = func_obs$args - func_obs$args[1], 
                          vals = func_obs$vals)
  # return list
  return(zeroed_func_obs)
}

#' This function is a wrapper around zero_obs and zeroes all 
#' observations in the data set
#'
#'@param func_dat: list that contains the observations
#' each observation is a list, that contains two vectors of identical length: args and vals
#'
#'@return A list of zeroed observations
#'@export
zero_data <- function(func_dat){
  
  # create zeroed observation in usual format
  zeroed_func_dat <- purrr::map(.x = func_dat,
                                .f = zero_obs)
  # return list
  return(zeroed_func_dat)
}