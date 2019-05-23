#' Build Auto-WEKA classifier
#'
#' This functions builds an Auto-WEKA classifier.
#'
#' @param seed the seed for the random number generator (defaults to 123).
#'
#' Original parameter name: \code{-seed}.
#' @param timeLimit the (approximate) time limit for tuning in minutes (defaults to 15).
#'
#' Original parameter name: \code{-timeLimit}.
#' @param memoryLimit the memory limit for runs in MiB (defaults to 1024).
#'
#' Original parameter name: \code{-memLimit}.
#' @param numberOfConfigs the amount of best configurations to output (defaults to 1).
#'
#' Original parameter name: \code{-nBestConfigs}.
#' @param metric the metric to optimise (defaults to \code{errorRate}). Available metrics:
#'
#' areaAboveROC, areaUnderROC, avgCost, correct, correlationCoefficient, errorRate, falseNegativeRate, falsePositiveRate, fMeasure, incorrect, kappa, kBInformation, kBMeanInformation, kBRelativeInformation, meanAbsoluteError, pctCorrect, pctIncorrect, precision, relativeAbsoluteError, rootMeanSquaredError, rootRelativeSquaredError, weightedAreaUnderROC, weightedFalseNegativeRate, weightedFalsePositiveRate, weightedFMeasure, weightedPrecision, weightedRecall, weightedTrueNegativeRate, weightedTruePositiveRate
#'
#' Original parameter name: \code{-metric}.
#' @param parallelRuns the number of parallel runs (defaults to 1). Caution: this option is experimental.
#'
#' Original parameter name: \code{-parallelRuns}.
#' @param outputDebugInfo logical indicating to run the classifier in debug mode which may output additional information (defaults to \code{FALSE}).
#'
#' Original parameter name: \code{-output-debug-info}.
#' @param noCapabiltiesCheck logical indicating to not check the classifier capabilities before the classifier is built (defaults to \code{FALSE}). Use with caution.
#'
#' Original parameter name: \code{-do-not-check-capabilities}.
#' @param decimalPlaces the number of decimal places for the output of numbers in the model (defaults to 2).
#'
#' Original parameter name: \code{-num-decimal-places}.
#' @param batchSize the desired batch size for batch prediction (defaults to 100).
#'
#' Original parameter name: \code{-batch-size}.
#' @return an Auto-WEKA classifier
#' @examples
#' \dontrun{
#' buildAutoWekaClassifier(iris, timeLimit = 1)
#' }
#' @export
buildAutoWekaClassifier <- function(data, seed = 123, timeLimit = 15, memoryLimit = 1024, numberOfConfigs = 1, metric = "errorRate",
                                    parallelRuns = 1, outputDebugInfo = FALSE, noCapabiltiesCheck = FALSE, decimalPlaces = 2, batchSize = 100) {
  # Map data to WEKA instances
  wekaInstances = read_model_frame_into_Weka(data)

  # Map function parameters to character vector
  runOptions <- vector()
  runOptions <- c(runOptions, "-seed", seed)
  runOptions <- c(runOptions, "-timeLimit", timeLimit)
  runOptions <- c(runOptions, "-memLimit", memoryLimit)
  runOptions <- c(runOptions, "-nBestConfigs", numberOfConfigs)
  runOptions <- c(runOptions, "-metric", metric)
  runOptions <- c(runOptions, "-parallelRuns", parallelRuns)
  if(outputDebugInfo)
    runOptions <- c(runOptions, "-output-debug-info")
  if(noCapabiltiesCheck)
    runOptions <- c(runOptions, "-do-not-check-capabilities")
  runOptions <- c(runOptions, "-num-decimal-places", decimalPlaces)
  runOptions <- c(runOptions, "-batch-size", batchSize)

  # Start redirecting Java Standard Error Output
  exitFunction <- startRedirectingJavaStdErrOutput()

  # Build AutoWEKAClassifier
  autoWEKAClassifier <- rJava::.jnew("de.wwu.is.stat.automl.RAutoWEKAClassifier") # subclass of weka.classifiers.meta.AutoWEKAClassifier
  rJava::.jcall(autoWEKAClassifier, "V", "setOptions", rJava::.jarray(runOptions))
  rJava::.jcall(autoWEKAClassifier, "V", "buildClassifier", wekaInstances)

  # Stop redirecting Java Standard Error Output and output errors if outputDebugInfo = T
  stopRedirectingJavaStdErrOut(exitFunction,outputDebugInfo)

  classifyInstance <- function() {

  }

  distributionForInstance <- function() {

  }

  # TODO:
  # - map WEKA output to R
  # - create RWeka configured classifier and add to result
  # - RWeka's predictions_for_instances needed?
  # - custom print / summary etc. function for output

  result <- list()
  result[["awc"]] <- autoWEKAClassifier
  result[["capabilities"]] <- rJava::.jcall(rJava::.jcall(autoWEKAClassifier, "Lweka/core/Capabilities;", "getCapabilities"), "S", "toString")

  result[["classifier"]] <-

  result[[metric]] <- rJava::.jcall(autoWEKAClassifier, "D", "measureEstimatedMetricValue")

  evaluation <- rJava::.jcall(autoWEKAClassifier, "Lweka/classifiers/Evaluation;", "getEval")
  result[["cor"]] <- rJava::.jcall(evaluation, "D", "correlationCoefficient")
  result[["mae"]] <- rJava::.jcall(evaluation, "D", "meanAbsoluteError")
  result[["rmse"]] <- rJava::.jcall(evaluation, "D", "rootMeanSquaredError")
  rae <- rJava::.jcall(evaluation, "D", "relativeAbsoluteError", check = FALSE) # throws java.lang.Exception
  if (is.null(rJava::.jgetEx())) { # Check for excpetion
    result[["rae"]] = rae
  } else {
    rJava::.jcheck(silent = TRUE) # Clear exception
  }
  result[["rrse"]] <- rJava::.jcall(evaluation, "D", "rootRelativeSquaredError")
  result[["numInstances"]] <- rJava::.jcall(evaluation, "D", "numInstances")

  structure(result, class = "RAutoWEKAClassifier")
}

#' Print
#'
#' @rdname buildAutoWekaClassifier
#' @export
print.RAutoWEKAClassifier <- function(x, ...) {
    writeLines(rJava::.jcall(x$awc, "S", "toString"))
    invisible(x)
}

#' Summary
#'
#' @rdname buildAutoWekaClassifier
#' @export
summary.RAutoWEKAClassifier <- function(x, ...) {
  writeLines(rJava::.jcall(x$awc, "S", "toString"))
  invisible(x)
}
