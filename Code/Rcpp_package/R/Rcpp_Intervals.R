#' placeholder
#'
#'@param func_dat: list that contains the observations
#' each observation is a list, that conatins two vectors of identical length: args and vals
#'
#'@return placeholder
#'@export
measuring_int <- function(func_dat){
  intervals <- matrix(data = unlist(map(.x = func_dat,
                                        .f = function(obs) c(min(obs$args), max(obs$args)))),
                      nrow = length(func_dat), byrow = TRUE)
  
  return(intervals)                     
}

#' placeholder
#'
#'@param measuring_intervals: use output from measuring_int()
#'
#'@return placeholder
#'@export
unique_intervals <- function(measuring_intervals){
  
  # for finding unique entries transforming to a list is easier
  interval_list <- map(.x = seq_len(nrow(measuring_intervals)), 
                       .f = function(i) measuring_intervals[i,])
  
  # find unique entries                             
  unique_intervals <- unique(interval_list)
  
  # combine into matrix again                         
  unique_matrix <- matrix(data = unlist(unique_intervals), 
                          nrow = length(unique_intervals),
                          byrow = TRUE)
  
  # return matrix where each row contains the beginning and end points of a unique measuring interval
  # from the data set
  return(unique_matrix)                         
}