context("test-util")

testthat::test_that("buildAutoWekaClassifier works", {
  download.file("https://storm.cis.fordham.edu/~gweiss/data-mining/weka-data/iris.arff", "iris.arff")
  buildAutoWekaClassifier("iris.arff", timeLimit = 1, noCrossValidation = TRUE)
  file.remove("iris.arff")
})
