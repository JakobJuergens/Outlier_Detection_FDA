#' This function determines the factor for stretch_sample_detection
#' if sample_size > n_comparables
#'
#' @param n_comparables: number of comparable observations
#' @param sample_size: chosen sample size for sampling procedure
#' @param n_samples: chosen number of samples 
#'
#' @return An integer that's used for weighing the non-sampled steps in
#' stretch_sample_detection
sampling_factor <- function(n_comparables, sample_size, n_samples) {
  return(ceiling(sample_size / n_comparables * n_samples))
}

#' This function determines
#'
#' @param n_comparables: Number of comparable observations
#' @param sample_size: Chosen sample size for sampling procedure
#' @param expn: Chosen expected number of draws any observation appears in per 
#' unique interval it is comparable to.
#'
#' @return A number that's used as the number of samples drawn in each
#' sampling step.
sampling_number <- function(n_comparables, sample_size, expn) {
  # calculate probability of appearing in a specific sample
  prob_each_sample <- sample_size / n_comparables
  
  # calculate number of number of samples necessary for an expected number
  # of occurences leq expn
  n_samples <- ceiling(expn/prob_each_sample)
  
  # return n_samples
  return(n_samples)
}
