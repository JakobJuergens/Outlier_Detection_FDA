#' Checks which measuring intervals are suitable to be compared to a given measuring interval
#' given an acceptable stretching parameter lambda.
#'
#'@param main_interval: vector of two elements: starting and end point of measuring interval
#'@param measuring_intervals: use output from measuring_int_mat
#'@param lambda: acceptable stretching parameter
#'@param ids: identifiers of individual observations
#'
#'@return A list containing two objects: ind, which contains the rownumber of the 
#' comparable observations in the matrix measuring_intervals and ids, which contains
#' the identifiers of the comparable observations
#'@export
comparable_obs_finder <- function(main_interval, measuring_intervals, lambda, ids){
  
  # Determine comparable observations by checking interval endpoints
  comparable <- which(measuring_intervals[,2] >= main_interval[2]/lambda 
                      & measuring_intervals[,2] <= main_interval[2]*lambda)
  
  # Return the correspoding indices and the ids of the comparable observations
  return(list(ind = comparable,
              ids = ids[comparable]))
}