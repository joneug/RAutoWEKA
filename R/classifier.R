#' Build Auto-WEKA classifier
#'
#' This functions builds an Auto-WEKA classifier.
#'
#' @param formula a model formua.
#'
#' @param data the data to build the classifier on.
#'
#' @param subset an optional vector specifying a subset of observations to be used when building the classifier (see \code{\link[stats]{model.frame}}).
#'
#' @param na.action a function that specifies how to deal with NAs (see \code{\link[stats]{model.frame}}).
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
#'
#' @return an Auto-WEKA classifier.
#'
#' @seealso \code{\link[RWeka]{make_Weka_classifier}}
#'
#' @examples
#' classifier = buildAutoWekaClassifier(Species ~ ., iris, timeLimit = 1)
#' predict(classifier, iris[1:10,-5])
#'
#' @export
buildAutoWekaClassifier <- function(formula, data, subset, na.action, seed = 123, timeLimit = 15, memoryLimit = 1024, numberOfConfigs = 1, metric = "errorRate",
                                    parallelRuns = 1, outputDebugInfo = FALSE, noCapabiltiesCheck = FALSE, decimalPlaces = 2, batchSize = 100) {
  # Create model frame from call (adapted from RWeka::make_Weka_classifier and stats:lm)
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  mf <- mf[c(1L, match(c("formula", "data", "subset", "na.action"), names(mf), 0L))]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")

  # Map data to WEKA instances
  instances = read_model_frame_into_Weka(mf)

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
  autoWEKAClassifier <- rJava::.jnew("de/wwu/is/RAutoWEKA/RAutoWEKAClassifier") # subclass of weka/classifiers/meta/AutoWEKAClassifier
  rJava::.jcall(autoWEKAClassifier, "V", "setOptions", rJava::.jarray(runOptions))
  rJava::.jcall(autoWEKAClassifier, "V", "buildClassifier", instances)

  # Stop redirecting Java Standard Error Output and output errors if outputDebugInfo = T
  stopRedirectingJavaStdErrOut(exitFunction,outputDebugInfo)

  # Create result list
  result <- list()
  # Auto WEKA Classifier object
  result[["awc"]] <- autoWEKAClassifier
  # Capabilities
  result[["capabilities"]] <- rJava::.jcall(rJava::.jcall(autoWEKAClassifier, "Lweka/core/Capabilities;", "getCapabilities"), "S", "toString")
  # Value for metric to optimize
  result[[metric]] <- rJava::.jcall(autoWEKAClassifier, "D", "measureEstimatedMetricValue")
  # Error metrics
  evaluation <- rJava::.jcall(autoWEKAClassifier, "Lweka/classifiers/Evaluation;", "getEval")
  cor <- rJava::.jcall(evaluation, "D", "correlationCoefficient", check = FALSE) # throws java.lang.Exception
  if (is.null(rJava::.jgetEx())) { # Check for excpetion
    result[["cor"]] <- cor
  } else {
    rJava::.jcheck(silent = TRUE) # Clear exception
  }
  result[["mae"]] <- rJava::.jcall(evaluation, "D", "meanAbsoluteError")
  result[["rmse"]] <- rJava::.jcall(evaluation, "D", "rootMeanSquaredError")
  rae <- rJava::.jcall(evaluation, "D", "relativeAbsoluteError", check = FALSE) # throws java.lang.Exception
  if (is.null(rJava::.jgetEx())) { # Check for excpetion
    result[["rae"]] <- rae
  } else {
    rJava::.jcheck(silent = TRUE) # Clear exception
  }
  result[["rrse"]] <- rJava::.jcall(evaluation, "D", "rootRelativeSquaredError")
  # Instances
  result[["instances"]] <- instances
  # Number of instances
  result[["numInstances"]] <- rJava::.jcall(evaluation, "D", "numInstances")
  # Levels
  result[["levels"]] <- levels(mf[[1L]])
  # Call
  result[["call"]] <- cl
  # Model frame
  result[["mf"]] <- mf
  result[["terms"]] <- mt
  # Predictions
  result[["classPredicitions"]] <- classifyInstances(result, instances)
  result[["probabilityPredictions"]] <- distributionForInstances(result, instances)

  # Set class of result list
  structure(result, class = "RAutoWEKAClassifier")
}

#' Calculates the class membership for given test instances.
#'
#' @param object the classifier object.
#' @param instances the instances to classify.
#'
#' @return the class memberships.
#'
#' @noRd
classifyInstances <- function(object, instances) {
  prediction <- rJava::.jcall(object$awc, "[D", "classifyInstances", instances)
  is.na(prediction) <- is.nan(prediction)
  prediction <- factor(object$levels[prediction + 1L], levels = object$levels)

  return(prediction)
}

#' Calculates the class membership probabilities for given test instances.
#'
#' @param object the classifier object.
#' @param instances the instances to classify.
#'
#' @return the class membership probabilities.
#'
#' @noRd
distributionForInstances <- function(object, instances) {
  numInstances = rJava::.jcall(instances, "I", "numInstances")
  prediction <- rJava::.jcall(object$awc, "[D", "distributionForInstances", instances)
  # Reshape vector to matrix
  predictionMatrix <- matrix(prediction, nrow = numInstances, byrow = TRUE)
  rowNames <- attr(instances, ".dimnames")[[1L]]
  dimnames(predictionMatrix) <- list(rowNames, object$levels)

  return(predictionMatrix)
}

#' Print
#' S3 method for class 'RAutoWEKAClassifier'
#'
#' @rdname buildAutoWekaClassifier
#'
#' @export
print.RAutoWEKAClassifier <- function(x, ...) {
    writeLines(rJava::.jcall(x$awc, "S", "toString"))
    invisible(x)
}

#' S3 method for class 'RAutoWEKAClassifier'
#'
#' @rdname buildAutoWekaClassifier
#'
#' @export
summary.RAutoWEKAClassifier <- function(object, ...) {
  writeLines(rJava::.jcall(object$awc, "S", "toString"))
  invisible(object)
}

#' S3 method for class 'RAutoWEKAClassifier'
#'
#' @rdname buildAutoWekaClassifier
#'
#' @export
predict.RAutoWEKAClassifier <- function(object, newdata, type = c("class", "probability"), ...) {
  type <- match.arg(type)

  if (missing(newdata) || is.null(newdata)) {
    if(type == "class") {
      return(object$classPredicitions)
    } else if (type == "probability") {
      return(object$probabilityPredictions)
    }
  }

  mf <- model.frame(delete.response(terms(object)), newdata, na.action = object$call$na.action)
  classes <- factor(NA, levels = object$levels)
  mf <- cbind(CLASS = classes, mf)
  instances <- read_model_frame_into_Weka(mf)

  if(type == "class") {
    prediction <- classifyInstances(object, instances)
  } else if(type == "probability") {
    prediction <- distributionForInstances(object, instances)
  }

  return(prediction)
}

#' S3 method for class 'RAutoWEKAClassifier'
#'
#' @rdname buildAutoWekaClassifier
#'
#' @export
model.frame.RAutoWEKAClassifier <- function(formula, ...) {
  return(formula$mf)
}

#' S3 method for class 'RAutoWEKAClassifier'
#'
#' @rdname buildAutoWekaClassifier
#'
#' @export
terms.frame.RAutoWEKAClassifier <- function(x, ...) {
  return(x$terms)
}
