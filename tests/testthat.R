# Make shure testthat is installed
if(!require(testthat)){
  install.packages("testthat", repos = "https://cran.uni-muenster.de")
  library(testthat)
}

# Make shure mlbench is installed
if(!require(mlbench)){
  install.packages("mlbench", repos = "https://cran.uni-muenster.de")
  library(mlbench)
}

# Load packge
library(RAutoWEKA)

# Run tests
test_check("RAutoWEKA")
