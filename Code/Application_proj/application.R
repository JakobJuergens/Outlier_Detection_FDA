library(MASS)
library(tidyverse)
library(largeList)
library(parallel)
library(Rcpp)
# install.packages('../OSEproject_1.0.tar.gz', repos = NULL, type ="source")
library(OSEproject)

set.seed(42)

real_data <- TRUE
if(real_data){
  endanzug_path <- '~/F/data_local/Projekt_AMEIUS_Daten/schra.RDS'
  endanzug_data <- readRDS(file = endanzug_path)
}

grids <- endanzug_data$aArr_endanzug
vals <- endanzug_data$mArr_endanzug

# choose subset to test the algorithm
test_set <- sort(sample(x = 1:dim(endanzug_data)[1], 
                 size = 10000, replace = FALSE))

# generate subsets for testing the algorithm
test_grids <- grids[test_set]
test_vals <- vals[test_set]

# bring into format that's used by the implementation

