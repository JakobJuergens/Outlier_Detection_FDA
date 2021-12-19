### clear workspace
rm(list = ls())

### load necessary CRAN libraries
library(MASS)
library(tidyverse)
library(largeList)
library(parallel)
library(Rcpp)

### load my own library
install.packages('../OutDetectR_1.0.tar.gz', repos = NULL, type = 'source')
library(OutDetectR)

### set up parameters
# sample size for sampling procedure
sample_size <- 100
endanzug_path <- "~/F/data_local/Projekt_AMEIUS_Daten/"

### load real Endanzug-Data if possible
real_data <- TRUE
if (real_data) {
  endanzug_data <- readRDS(file = paste0(endanzug_path, "schra.RDS"))
}

### extract observation angles and torque
grids <- endanzug_data$aArr_endanzug
vals <- endanzug_data$mArr_endanzug

### select subset to test the algorithm
set.seed(42)
n <- 10000
test_set <- sort(
  sample(x = 1:dim(endanzug_data)[1], size = n, replace = FALSE)
)

### extract corresponding grids, vals and ids
test_grids <- grids[test_set]
test_vals <- vals[test_set]
test_ids <- endanzug_data[test_set, "prodNo"]

### bring the test_observations into the usual format
test_data <- purrr::map(
  .x = 1:n,
  .f = function(i) list(args = grids[[i]], vals = vals[[i]])
)

### zero observations to allow for stretching
test_data_zero <- OutDetectR::zero_data(test_data)

### get measuring intervals from test data
test_ints <- OutDetectR::measuring_int_mat(test_data_zero)

### for every observation in the test set find the comparable observations
# this is automated in the sampling procedure but is currently needed
# to make sure that every observation has a sufficient number of comparable
# observations for the sampling procedure
comparables_test <- map(
  .x = 1:n,
  .f = function(i) {
    OutDetectR::comparable_obs_finder(
      main_interval = test_ints[i, ], lambda = 1.2,
      measuring_intervals = test_ints, ids = test_ids[[1]]
    )
  }
)

# get number of comparable observations
n_comparables <- map(
  .x = comparables_test,
  .f = function(obs) length(obs$ind)
)

# find observations that have too few comparable coutnerparts
too_few <- which(n_comparables < sample_size)

# iterate until there are no observations left that have fewer
# comparable counterparts than sample-size
inds <- 1:n
i <- 1
while (!all(n_comparables > sample_size)) {
  print(i)
  comparables_check <- map(
    .x = inds,
    .f = function(j) {
      OutDetectR::comparable_obs_finder(
        main_interval = test_ints[j, ], lambda = 1.2,
        measuring_intervals = test_ints[inds, ], ids = as_vector(test_ids[inds, ])
      )
    }
  )

  # exclude observations with too few comparable observations
  n_comparables <- map(
    .x = comparables_check,
    .f = function(obs) length(obs$ind)
  )

  too_few <- which(n_comparables < sample_size)

  if (length(too_few) > 0) {
    # print(too_few)
    inds <- inds[-too_few]
  }

  i <- i + 1
}

### reduce data set to the observations that have a sufficient number of comparable
# observations for sampling procedure
reduced_data <- test_data[inds]
reduced_ints <- test_ints[inds, ]
reduced_ids <- test_ids[inds, ][[1]]
reduced_n <- length(inds)

### find unique measuring intervals in data set
unique_intervals <- OutDetectR::unique_intervals(reduced_ints)

### save data as largeList for further processing
OutDetectR::largeListify(
  func_dat = reduced_data,
  path = paste0(endanzug_path, "test_red_data.llo")
)

### Use Sampling procedure for reduced data set
# create cluster
cl <- makeForkCluster(detectCores() - 1)

# try using the sampling procedure
test_procedure <- OutDetectR::stretch_sample_detection(
  cl = cl, list_path = paste0(endanzug_path, "test_red_data.llo"),
  measuring_intervals = reduced_ints, lambda = 1.2, n_samples = 100,
  sample_size = 50, alpha = 0.05, B = 50, gamma = 0.05
)

stopCluster(cl)
