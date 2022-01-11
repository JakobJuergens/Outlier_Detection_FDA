#' Uses a bootstrapping procedure to approximate the cutoff value for the identification
#' of outliers
#'
#' @param matr_dat: data in matrix form - each row contains the grid approximations of one observation
#' @param fdepths: corresponding depths for the observations
#' @param alpha: quantile of least deep observations to drop before bootstrapping
#' @param B: number of smoothed bootstrap samples to use
#' @param gamma: tuning parameter for smoothed bootstrap
#' @param grid: grid used in approximation of matr_dat
#'
#' @return placeholder
approx_C <- function(matr_dat, fdepths, alpha, B, gamma, grid) {

  # set smoothing mode to on
  smoothing <- TRUE
  # infer number of observations from length of depth vector
  n <- length(fdepths)
  # Get number of elements in grid
  grid_length <- length(grid)
  # determine threshold to drop observations with lowest depth values
  depth_thr <- tryCatch(
    {
      quantile(x = fdepths, probs = alpha, na.rm = FALSE)
    },
    error = function(cond){
      stop(fdepths, 'approx_C - I ran into this error!')
    }
  )
  # drop observations for bootstrapping
  matr_dat_red <- matr_dat[fdepths >= depth_thr, ]

  n_red <- dim(matr_dat_red)[1]

  # Determine vcov-matrix for smoothed bootstrapping
  Sigma_x <- cov(matr_dat_red)

  my_vcov <- gamma * Sigma_x
  
  # Draw bootstrap samples from data set
  fsamples <- map(
    .x = 1:B,
    .f = function(inds) matr_dat_red[sample(x = 1:n_red, size = n, replace = TRUE), ]
  )

  # Create smoothing components for bootstrapping
  smoothing_components <- map(
    .x = 1:B,
    .f = function(x) mvrnorm(n = n, mu = rep(0, times = grid_length), Sigma = my_vcov)
  )

  # Obtain smoothed bootstrap samples
  smoothed_BS_samples <- map(
    .x = 1:B,
    .f = function(b) fsamples[[b]] + smoothing_components[[b]]
  )

  # Calculate depths for each smoothed bootstrap sample
  bootstrap_depths <- map(
    .x = 1:B,
    .f = function(b) hM_depth(smoothed_BS_samples[[b]], grid)
  )

  # Calculate first percentile from depths of smoothed bootstrap samples
  one_perc_quantiles <- unlist(map(
    .x = bootstrap_depths,
    .f = function(sample) quantile(sample, probs = 0.01, na.rm = FALSE)
  ))

  # return median of first percentiles
  return(median(one_perc_quantiles))
}

#' This function performs one iteration of the algorithm, including the 
#' calculation of functional depths, the approximation of C and the 
#' flagging of observations with depths lower than C
#'
#' @param matr_dat: data in matrix form - each row contains the grid approximations of one observation
#' @param alpha: quantile of least deep observations to drop before bootstrapping
#' (in approximation of C - optional if C is specified)
#' @param: B: number of smoothed bootstrap samples to use
#' (in approximation of C - optional if C is specified)
#' @param: gamma: tuning parameter for smoothed bootstrap
#' @param: ids: identifiers of individual observations
#' @param: grid: grid used in approximation of matr_dat
#' @param: C: should be provided. Otherwise C will be approximated in each step of the iteration
#'
#' @return placeholder
outlier_iteration <- function(matr_dat, alpha = 0.05, B = 50, gamma, ids, grid, C = NULL) {

  # Calculating functional depths using a function from ./auxiliary/Rcpp_functions.cpp
  fdepths <- hM_depth(matr_dat, grid)

  if (missing(C)) {
    # Approximating C
    C <- approx_C(
      matr_dat = matr_dat, fdepths = fdepths, alpha = alpha,
      B = B, gamma = gamma, grid = grid
    )
  }

  # Flagging observations with depths lower than the cutoff value C
  outliers <- which(fdepths < C)

  return(list(
    matr_dat = matr_dat[-outliers, ],
    ids = ids[-outliers],
    outlier_ids = ids[outliers]
  ))
}

#'  This function serves as a wrapper for outlier_iteration and iterates the 
#'  process until no new observations are flagged.
#'
#' @param matr_dat: data in matrix form - each row contains the grid approximations of one observation
#' @param alpha: quantile of least deep observations to drop before bootstrapping
#' (in approximation of C - optional if C is specified)
#' @param B: number of smoothed bootstrap samples to use
#' (in approximation of C - optional if C is specified)
#' @param gamma: tuning parameter for smoothed bootstrap
#' @param ids: identifiers of individual observations
#' @param grid: grid used in approximation of matr_dat
#' @param C: should be provided. Otherwise C will be approximated in each step of the iteration
#'
#' @return placeholder
#' @export
outlier_detection <- function(matr_dat, alpha = 0.05, B = 100, gamma = 0.05, ids, grid, C = NULL) {
  tmp_ids <- ids
  # Initialize empty vectors for position of flagged observations in func_dat
  # and ids of flagged observations
  outlier_ids <- c()

  # loop that continues until an iteration does not flag any new observations
  condition <- TRUE
  while (condition) {

    # perform iteration
    iter_res <- outlier_iteration(
      matr_dat = matr_dat, alpha = alpha, B = B, gamma = gamma, 
      ids = tmp_ids, grid = grid, C = C)
    
    new_outliers <- iter_res$outlier_ids

    # if there are no new flagged observations stop loop
    if (length(new_outliers) == 0) {
      condition <- FALSE
    } else {
      # otherwise: add flagged observations to vector
      outlier_ids <- c(outlier_ids, new_outliers)
      # reduce data to non-flagged observations
      matr_dat <- iter_res$matr_dat
      # reduce ids to non-flagged observations
      tmp_ids <- iter_res$ids
    }
  }

  # return identifiers of flagged observations and position of these flagged observations in the data set
  return(list(
    outlier_ids = outlier_ids,
    outlier_ind = which(is.element(ids, outlier_ids))
  ))
}
