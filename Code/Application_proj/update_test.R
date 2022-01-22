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

version_check()

### set up parameters
# sample size for sampling procedure
sample_size <- 250

# generate synthetic test set
test_obj <- readRDS(file = 'Results/synth_input3.RDS')

# generate new observation
new_obj <- generate_set_3(n_obs = 1)

# get measuring intervals
synth_ints <- OutDetectR::measuring_int_mat(test_obj$data)

# get unique measuring intervals
unique_intervals <- OutDetectR::unique_intervals(synth_ints)

# load results from original test
test_procedure <- readRDS(file = 'Results/synth_sample3.RDS')

# create cluster
cl <- makeForkCluster(5)

# set different seeds just to be sure
clusterCall(cl = cl, fun = function(i) {
  set.seed(as.numeric(Sys.getpid()))
  print(as.numeric(Sys.getpid()))}
)

# run updating procedure
update_procedure <- OutDetectR::stretch_sample_updating(
  cl = cl, new_observation = new_obj$data[[1]], list_path = '~/Documents/tmp_data/synth_data3.llo', 
  lambda = 1.2, measuring_intervals = synth_ints, sample_size = sample_size, expn = 8, 
  alpha = 0.05, B = 100, gamma = 0.05, num_samples_prev = test_procedure$num_samples, 
  num_outliers_prev = test_procedure$num_outliers, debug = TRUE)

stopCluster(cl)

saveRDS(update_procedure, file = 'Results/synth_update.RDS')