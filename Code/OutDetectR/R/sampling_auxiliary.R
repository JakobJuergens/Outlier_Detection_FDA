#' This function determines the factor for stretch_sample_detection
#' if sample_size > n_comparables
#'
#' @param n_comparables:
#' @param sample_size:
#' @param n_samples:
#'
#' @return An integer that's used for weighing the non-sampled steps in
#' stretch_sample_detection
sampling_factor <- function(n_comparables, sample_size, n_samples) {
  return(ceiling(sample_size / n_comparables * n_samples))
}

#' This function determines
#'
#' @param n_comparables:
#' @param sample_size:
#'
#' @return An integer that's used for weighing the non-sampled steps in
#' stretch_sample_detection
sampling_number <- function(n_comparables, sample_size) {
  return(x)
}
