context("test-classifier")

testthat::test_that("buildAutoWekaClassifier works", {
  buildAutoWekaClassifier(Species ~ ., iris, timeLimit = 1)
})
