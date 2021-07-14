# func_dat: list that contains the observations
# each observation is a list, that conatins two vectors of identical length: args and vals

measuring_int <- function(func_dat){
    intervals <- map(.x = func_dat,
                     .f = function(obs) c(min(obs$args), max(obs$args)))
    return(intervals)                     
}
                     
# start: start point of the grid
# end: end point of the grid

grid_finder <- function(func_dat){
  return(seq(0, 1, length.out = 100))
}
                     
# func_dat: list that contains the observations
# each observation is a list, that conatins two vectors of identical length: args and vals
# grid: grid to use for approximation

grid_approx_set_obs <- function(func_dat, grid) {
  res_mat <- matrix(data = unlist(
      map(.x = func_dat,
          .f = function(obs) grid_approx_obs(obs$args, obs$vals, grid))
    ), nrow = length(func_dat), byrow = TRUE)
                    
  return(res_mat)
}
                    
# matr_dat: data in matrix form - each row contains the grid approximations of one observation
# fdepths: correponding depths for the observations
# alpha: quantile of least deep observations to drop before bootstrapping
# B: number of smoothed bootstrap samples to use
# gamma: tuning parameter for smoothed bootstrap
# grid: grid used in approximation of matr_dat

approx_C <- function(matr_dat, fdepths, alpha, B, gamma, grid) {
  
  # infer number of observations from length of depth vector
  n <- length(fdepths)
  # Get number of elements in grid
  grid_length <- length(grid) 
  # determine threshold to drop observations with lowest depth values    
  depth_thr <- quantile(x = fdepths, probs = alpha)
  # drop observations for bootstrapping    
  matr_dat_red <- matr_dat[fdepths >= depth_thr, ]
  n_red <- dim(matr_dat_red)[1]
    
  # Determine vcov-matrix for smoothed bootstrapping    
  Sigma_x <- cov(matr_dat_red)
  my_vcov <- gamma*Sigma_x 

  # Draw bootstrap samples from data set  
  fsamples <- map(.x = 1:B,
                  .f = function(inds) matr_dat_red[sample(x = 1:n_red, size = n, replace = TRUE), ])
  
  # Create smoothing components for bootstrapping                  
  smoothing_components <- map(.x = 1:B,
                              .f = function(x) mvrnorm(n = n, mu = rep(0, times = grid_length), Sigma = my_vcov))
  
  # Obtain smoothed bootstrap samples                                  
  smoothed_BS_samples <- map(.x = 1:B,
                             .f = function(b) fsamples[[b]] + smoothing_components[[b]])

  # Calculate depths for each smoothed bootstrap sample                             
  bootstrap_depths <- map(.x = 1:B,
                          .f = function(b) hM_depth(smoothed_BS_samples[[b]], grid))

  # Calculate first percentile from depths of smoothed bootstrap samples                          
  one_perc_quantiles <- unlist(map(.x = bootstrap_depths,
                                   .f = function(sample) quantile(sample, probs = 0.01)))
  
  # return median of first percentiles                                   
  return(median(one_perc_quantiles))
}
                                   
# matr_dat: data in matrix form - each row contains the grid approximations of one observation
# alpha: quantile of least deep observations to drop before bootstrapping (in approximation of C - optional if C is specified)
# B: number of smoothed bootstrap samples to use (in approximation of C - optional if C is specified)
# gamma: tuning parameter for smoothed bootstrap
# ids: identifiers of individual observations
# grid: grid used in approximation of matr_dat
# C: should be provided. Otherwise C will be apprroximated in each step of the iteration

outlier_iteration <- function(matr_dat, alpha = 0.05, B = 50, gamma, ids, grid, C = NULL){
    
  # Calculating functional depths using a function from ./auxiliary/Rcpp_functions.cpp  
  fdepths <- hM_depth(matr_dat, grid)
  
  if(missing(C)){
      # Approximating C  
      C <- approx_C(matr_dat = matr_dat, fdepths = fdepths, alpha = alpha,
                    B = B, gamma = gamma, grid = grid)
  }
  
  # Flagging observations with depths lower than the cutoff value C  
  outliers <- which(fdepths < C)
    
  return(list(matr_dat = matr_dat[-outliers, ],
              ids = ids[-outliers],
              outlier_ids = ids[outliers]))
}
                                   
# func_dat: list that contains the observations
# each observation is a list, that contains two vectors of identical length: args and vals
# alpha: quantile of least deep observations to drop before bootstrapping (in approximation of C - optional if C is specified)
# B: number of smoothed bootstrap samples to use (in approximation of C - optional if C is specified)
# gamma: tuning parameter for smoothed bootstrap
# ids: identifiers of individual observations
# grid: grid used for the approximation
# C: should be provided. Otherwise C will be approximated in each step of the iteration

outlier_detection <- function(matr_dat, alpha = 0.05, B = 100, gamma = 0.05, ids, grid, C = NULL){
    
    tmp_ids <- ids
    # Initialize empty vectors for position of flagged observations in func_dat and ids of flagged observations
    outlier_ids <- c()
    
    # loop that continues until an iteration does not flag any new observations
    condition <- TRUE
    while(condition){
        
        # perform iteration
        iter_res <- outlier_iteration(matr_dat = matr_dat, alpha = alpha, B = B, gamma = gamma, ids = tmp_ids, grid = grid, C = C)
        new_outliers <- iter_res$outlier_ids
        
        # if there are no new flagged observations stop loop
        if(length(new_outliers) == 0){condition <- FALSE}
        else{
          #otherwise: add flagged observations to vector
          outlier_ids <- c(outlier_ids, new_outliers)
          # reduce data to non-flagged observations
          matr_dat <- iter_res$matr_dat
          # reduce ids to non-flagged observations
          tmp_ids <- iter_res$ids 
        }
    }
    
    # return identifiers of flagged observations and position of these flagged observations in the data set
    return(list(outlier_ids = outlier_ids,
                outlier_ind = which(is.element(ids, outlier_ids))))
}                                
                                   
detection_wrap <- function(func_dat, ids, alpha, B, gamma = 0.05){
    
    grid <- grid_finder(func_dat)
    
    matr_dat <- grid_approx_set_obs(func_dat, grid)
    
    fdepths <- hM_depth(matr_dat, grid)
    
    C_appr <- approx_C(matr_dat = matr_dat, fdepths = fdepths, alpha = alpha, B = B, gamma = gamma, grid = grid)

    flagged <- outlier_detection(matr_dat = matr_dat, ids = ids, grid = grid, C = C_appr)
    
    return(flagged)
}
                                   
# list_path: path to the random access list of the data set (generated by package largeList)
# index: index of observations to use in the procedure
# alpha: quantile of least deep observations to drop before bootstrapping (in approximation of C)
# B: number of smoothed bootstrap samples to use (in approximation of C)
# gamma: tuning parameter for smoothed bootstrap

random_access_par_helper <- function(list_path, ids, index, alpha, B, gamma){
    
    func_dat <- readList(file = list_path, index = index)

    sample_flagged <- detection_wrap(func_dat = func_dat, ids = ids, alpha = alpha, B = B, gamma = gamma)
    
    return(sample_flagged$outlier_ids)
}
                                   
# cl: cluster object generated by parallel package
# n_obs: number of observations in data set
# n_samples: number of samples to use
# sample_size: number of obserrvations to use in each sample
# alpha: quantile of least deep observations to drop before bootstrapping (in approximation of C)
# B: number of smoothed bootstrap samples to use (in approximation of C)
# gamma: tuning parameter for smoothed bootstrap
# list_path: path to the random access list of the data set (generated by package largeList)

sampling_wrap <- function(cl, n_obs, n_samples, sample_size, alpha, B, gamma, list_path){  
    
    ids <- 1:n_obs
    
    # Initialize vectors described in the theoretical section
    num_samples <- rep(x = 0, times = n_obs)
    num_outliers <- rep(x = 0, times = n_obs)
    frac_outliers <- rep(x = 1, times = n_obs)
    
    # Draw indexes for sampling from functional data without replacement
    sample_inds <- map(.x = 1:n_samples, 
                       .f = function(i) sample(x = 1:n_obs, size = sample_size, replace = FALSE))
    
    # Determine how often each observation appeared in the samples and update the vector                      
    freq_samples <- tabulate(unlist(sample_inds))  
    num_samples[1:length(freq_samples)] <- num_samples[1:length(freq_samples)] + freq_samples                   
    
    # Perform the outlier classification procedure on the chosen samples parallelized
    # with the function clusterApplyLB() from the parallel package                       
    sample_flagged_par <- clusterApplyLB(cl = cl,
                                         x = sample_inds,
                                         fun = function(smpl){
                                             random_access_par_helper(list_path = list_path, ids = ids[smpl],
                                                                      index = smpl, alpha = alpha, 
                                                                      B = B, gamma = gamma)})  
    
    # Determine how often each observation were flagged in the samples and update the vector                     
    freq_outliers <- tabulate(unlist(sample_flagged_par))
    num_outliers[1:length(freq_outliers)] <- num_outliers[1:length(freq_outliers)] + freq_outliers
    
    # Determine fraction of samples each observation was flagged as an outlier in                       
    certainties <- unlist(map(.x = 1:n,
                              .f = function(i) ifelse(num_samples[i] != 0, num_outliers[i]/num_samples[i], 1)))
    
    # Returrn list containing the three central vectors: num_samples, num_outliers, certainties                                  
    return(list(num_samples = num_samples,
                num_outliers = num_outliers,
                certainties = certainties))                              
}
                              
                              