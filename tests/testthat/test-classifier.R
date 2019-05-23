context("test-classifier")

testthat::test_that("buildAutoWekaClassifier works", {
  buildAutoWekaClassifier(iris, timeLimit = 1)
})
