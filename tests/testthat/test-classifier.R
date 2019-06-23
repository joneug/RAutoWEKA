context("test-classifier")

testthat::test_that("autoWekaClassifier works", {
  autoWekaClassifier(Species ~ ., iris, timeLimit = 1)
})
