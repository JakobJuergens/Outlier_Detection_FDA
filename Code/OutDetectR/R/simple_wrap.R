#' This function performs the identification procedure implemented in this package
#' in the simplest scenario. It serves as an important building block for the
#' more high-level functions.
#' Important assumptions are the following:
#' - observations in func_dat are zeroed and have the same measuring interval
#'
#' @param func_dat: list that contains the observations
#' each observation is a list, that contains two vectors of identical length: args and vals
#' @param ids: identifiers of individual observations
#' @param alpha: quantile of least deep observations to drop before bootstrapping
#' (in approximation of C - optional if C is specified)
#' @param ids: B: number of smoothed bootstrap samples to use
#' (in approximation of C - optional if C is specified)
#' @param gamma: tuning parameter for smoothed bootstrap
#'
#' @return Returns a list containing two objects:
#' 1. outlier_ids: a vector containing the ids of the observations identified as outliers
#' 2. outlier_ind: a vector containing the position of the the observations identified as outliers in the func_dat list
#' @export
detection_wrap <- function(func_dat, ids, alpha, B, gamma = 0.05) {

  # determine the grid for approximation
  grid <- grid_finder(func_dat = func_dat)

  # Approximate by linear interpolation
  matr_dat <- grid_approx_mat(func_dat = func_dat, grid = grid)

  # calculate h-modal depths
  fdepths <- hM_depth(valueMatrix = matr_dat, grid = grid)

  # Approximate a value of C
  C_appr <- approx_C(
    matr_dat = matr_dat, fdepths = fdepths, alpha = alpha,
    B = B, gamma = gamma, grid = grid
  )

  # Perform the outlier classification procedure for the approximated value of C
  flagged <- outlier_detection(
    matr_dat = matr_dat, ids = ids, grid = grid, C = C_appr)

  # Return the list of outlier ids and outlier indices - these are useful in different cases
  # contains the objects outlier_ids and outlier_ind
  return(flagged)
}
