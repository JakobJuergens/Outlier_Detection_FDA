#' placeholder
#'
#'@param func_dat: list that contains the observations
#' each observation is a list, that contains two vectors of identical length: args and vals
#'@param ids: identifiers of individual observations
#'@param alpha: quantile of least deep observations to drop before bootstrapping 
#' (in approximation of C - optional if C is specified)
#'@param ids: B: number of smoothed bootstrap samples to use 
#' (in approximation of C - optional if C is specified)
#'@param gamma: tuning parameter for smoothed bootstrap
#'
#'@return placeholder
#'@export
detection_wrap <- function(func_dat, ids, alpha, B, gamma = 0.05){
  
  # determine the grid for approximation
  grid <- grid_finder(func_dat)
  
  # Approximate by linear interpolation
  matr_dat <- grid_approx_set_obs(func_dat, grid)
  
  # calculate h-modal depths
  fdepths <- hM_depth(matr_dat, grid)
  
  # Approximate a value of C
  C_appr <- approx_C(matr_dat = matr_dat, fdepths = fdepths, alpha = alpha, B = B, gamma = gamma, grid = grid)
  
  # Perform the outlier classification procedure for the approximated value of C
  flagged <- outlier_detection(matr_dat = matr_dat, ids = ids, grid = grid, C = C_appr)
  
  # Return the list of outlier ids and outlier indices - these are useful in different cases
  return(flagged)
}