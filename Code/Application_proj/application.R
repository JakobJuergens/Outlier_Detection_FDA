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
sample_size <- 1000
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
n <- 25000
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

### reduce data set to the observations that have a sufficient number of comparable
# observations for sampling procedure
reduced_data <- test_data #[inds]
reduced_ints <- test_ints #[inds, ]
reduced_ids <- test_ids #[inds, ][[1]]
reduced_n <- n #length(inds)

### find unique measuring intervals in data set
unique_intervals <- OutDetectR::unique_intervals(reduced_ints)

### save data as largeList for further processing
# changed to different folder to improve IO performance
# M.2 NvME SSD instead of Harddrive
OutDetectR::largeListify(
  func_dat = reduced_data,
  path = '~/Documents/tmp_data/test_data.llo'
    #paste0(endanzug_path, "test_red_data.llo")
)

### Use Sampling procedure for reduced data set
# create cluster
cl <- makeForkCluster(5)

# set different seeds just to be sure
clusterCall(cl = cl, fun = function(i) {
  set.seed(as.numeric(Sys.getpid()))
  print(as.numeric(Sys.getpid()))}
)

# try using the sampling procedure
test_procedure <- OutDetectR::stretch_sample_detection(
  cl = cl, list_path = '~/Documents/tmp_data/test_data.llo',
  measuring_intervals = reduced_ints, lambda = 1.05, # n_samples = 10,
  sample_size = sample_size, expn = 5, alpha = 0.05, B = 100, gamma = 0.05,
  debug = TRUE
)

stopCluster(cl)

saveRDS(test_procedure, file = '~/F/data_local/Projekt_AMEIUS_Daten/stretch_sample_test.RDS')
