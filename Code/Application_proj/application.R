library(MASS)
library(tidyverse)
library(largeList)
library(parallel)
library(Rcpp)
install.packages('../OSEproject_1.0.tar.gz', repos = NULL, type ="source")
library(OSEproject)

set.seed(42)

real_data <- TRUE
if (real_data) {
  endanzug_path <- "~/F/data_local/Projekt_AMEIUS_Daten/"
  endanzug_data <- readRDS(file = paste0(endanzug_path, "schra.RDS"))
}

grids <- endanzug_data$aArr_endanzug
vals <- endanzug_data$mArr_endanzug

# choose subset to test the algorithm
n <- 10000
test_set <- sort(sample(
  x = 1:dim(endanzug_data)[1],
  size = n, replace = FALSE
))

# generate subsets for testing the algorithm
test_grids <- grids[test_set]
test_vals <- vals[test_set]

# bring into format that's used by the implementation
test_data <- purrr::map(
  .x = 1:n,
  .f = function(i) list(args = grids[[i]], vals = vals[[i]])
)

test_ids <- endanzug_data[test_set, "prodNo"]

# zero observations to allow for stretching
test_data_zero <- OSEproject::zero_observations(test_data)

test_ints <- measuring_int(test_data_zero)

comparables_test <- map(
  .x = 1:n,
  .f = function(i) {
    comparable_obs_finder(
      main_interval = test_ints[i, ], lambda = 1.2,
      measuring_intervals = test_ints, ids = test_ids[[1]]
    )
  }
)

sample_size <- 100

# exclude observations with too few comparable observations

n_comparables <- map(
  .x = comparables_test,
  .f = function(obs) length(obs$ind)
)

too_few <- which(n_comparables < sample_size)
inds <- 1:n
i <- 1
while (!all(n_comparables > sample_size)) {
  
  print(i)
  comparables_check <- map(
    .x = inds,
    .f = function(i) {
      comparable_obs_finder(
        main_interval = test_ints[i, ], lambda = 1.2,
        measuring_intervals = test_ints[inds, ], ids = as_vector(test_ids[inds,])
      )
    }
  )

  # exclude observations with too few comparable observations
  n_comparables <- map(
    .x = comparables_check,
    .f = function(obs) length(obs$ind)
  )
  
  too_few <- which(n_comparables < sample_size)
  
  if(length(too_few) > 0){
    print(too_few)
    inds <- inds[-too_few]
  }
  i <- i+1
}

reduced_data <- test_data[inds]
reduced_ints <- test_ints[inds,]
reduced_ids <- test_ids[inds,][[1]]
reduced_n <- length(inds)

unique_intervals <- unique_intervals(reduced_ints)

# comparables_check <- map(
#   .x = 1:reduced_n,
#   .f = function(i) {
#     comparable_obs_finder(
#       main_interval = reduced_ints[i, ], lambda = 1.2,
#       measuring_intervals = reduced_ints, ids = reduced_ids[[1]]
#     )
#   }
# )
# 
# n_comparables <- map(
#   .x = comparables_check,
#   .f = function(obs) length(obs$ind)
# )


# save data as largeList
saveList(reduced_data, paste0(endanzug_path, "test_red_data.llo"))

# Sampling procedure
cl <- makeForkCluster(detectCores() - 1)

test_procedure <- OSEproject::dectection_zr_smpl(
  cl = cl, list_path = paste0(endanzug_path, "test_red_data.llo"), 
  measuring_intervals = reduced_ints, n_obs = reduced_n, lambda = 1.2, n_samples = 100,
  sample_size = 50, alpha = 0.05, B = 50, gamma = 0.05
)

stopCluster(cl)
