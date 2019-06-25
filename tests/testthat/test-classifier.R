context("test-classifier")

testthat::test_that("RAutWEKA classifier works", {
  data(iris)
  # # # # # # # # # # # # #
  # Test autoWekaClassifier
  # # # # # # # # # # # # #
  formula = Species ~ .
  seed = 42
  timeLimit = 1
  memoryLimit = 1025
  numberOfConfigs = 2
  metric = "precision"
  parallelRuns = 2
  outputDebugInfo = TRUE

  classifier <- autoWekaClassifier(formula, iris, NULL, na.omit, seed, timeLimit, memoryLimit, numberOfConfigs, metric, parallelRuns, outputDebugInfo)

  # Test parameters
  testthat::expect_equal(classifier$awc$getSeed(), seed)
  testthat::expect_equal(classifier$awc$getTimeLimit(), timeLimit)
  testthat::expect_equal(classifier$awc$getMemLimit(), memoryLimit)
  testthat::expect_equal(classifier$awc$getnBestConfigs(), numberOfConfigs)
  testthat::expect_equal(classifier$awc$getParallelRuns(), parallelRuns)
  testthat::expect_equal(classifier$awc$getDebug(), outputDebugInfo)
  irisNAs <- iris
  irisNAs[nrow(irisNAs) + 1,] = list(Sepal.Length = NA, Sepal.Width = 1.0, Petal.Length = NA, Petal.Width = 3.0, Species = "setosa")
  testthat::expect_that(autoWekaClassifier(formula, irisNAs, na.action = na.fail), testthat::throws_error("missing values in object"))

  # Test classifier
  testthat::expect_is(classifier, "RAutoWEKAClassifier")
  testthat::expect_true(metric %in% names(classifier))
  testthat::expect_equal(classifier$levels, c("setosa", "versicolor", "virginica"))
  testthat::expect_equal(classifier$numInstances, 150)
  testthat::expect_equal(ncol(classifier$mf), 5)
  testthat::expect_equal(nrow(classifier$mf), 150)
  testthat::expect_equal(length(classifier$classPredicitions), 150)
  testthat::expect_equal(levels(classifier$classPredicitions), classifier$levels)
  testthat::expect_equal(ncol(classifier$probabilityPredictions), 3)
  testthat::expect_equal(nrow(classifier$probabilityPredictions), 150)
  testthat::expect_equal(colnames(classifier$probabilityPredictions), classifier$levels)
  testthat::expect_equal(classifier$classPredicitions, predict(classifier, type = "class"))
  testthat::expect_equal(classifier$probabilityPredictions, predict(classifier, type = "probability"))

  # # # # # # # # # # # # #
  # Test predict
  # # # # # # # # # # # # #
  classPredicitions = predict(classifier, iris[1:10,-5], type = c("class"))
  testthat::expect_equal(length(classPredicitions), 10)
  testthat::expect_equal(levels(classPredicitions), classifier$levels)
  probabilityPredictions = predict(classifier, iris[1:10,-5], type = c("probability"))
  testthat::expect_equal(ncol(probabilityPredictions), 3)
  testthat::expect_equal(nrow(probabilityPredictions), 10)
  testthat::expect_equal(colnames(probabilityPredictions), classifier$levels)

  # # # # # # # # # # # # #
  # Test model.frame
  # # # # # # # # # # # # #
  testthat::expect_equal(model.frame(classifier), classifier$mf)

  # # # # # # # # # # # # #
  # Test terms
  # # # # # # # # # # # # #
  testthat::expect_equal(terms(classifier), classifier$terms)
})
