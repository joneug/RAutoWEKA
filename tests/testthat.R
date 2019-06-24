# Make shure testthat is installed
if(!require(testthat)){
  install.packages("testthat")
  library(testthat)
}

# Make shure mlbench is installed
if(!require(mlbench)){
  install.packages("mlbench")
  library(mlbench)
}

# Load packge
library(RAutoWEKA)

# Run tests
test_check("RAutoWEKA")
