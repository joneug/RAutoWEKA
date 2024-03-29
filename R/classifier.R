#' Auto-WEKA Classifier
#'
#' This functions builds an Auto-WEKA classifier. Note that Auto-WEKA also supports regression algorithms, although the function name would not suggest that. It has been decided to stick to Auto-WEKA's naming as it is in line with WEKA's terminology (which also only uses the term classifier due to historic reasons).
#'
#' @param formula a model formula.
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
#' \code{areaAboveROC}, \code{areaUnderROC}, \code{avgCost}, \code{correct}, \code{correlationCoefficient}, \code{errorRate}, \code{falseNegativeRate}, \code{falsePositiveRate}, \code{fMeasure}, \code{incorrect}, \code{kappa}, \code{kBInformation}, \code{kBMeanInformation}, \code{kBRelativeInformation}, \code{meanAbsoluteError}, \code{pctCorrect}, \code{pctIncorrect}, \code{precision}, \code{relativeAbsoluteError}, \code{rootMeanSquaredError}, \code{rootRelativeSquaredError}, \code{weightedAreaUnderROC}, \code{weightedFalseNegativeRate}, \code{weightedFalsePositiveRate}, \code{weightedFMeasure}, \code{weightedPrecision}, \code{weightedRecall}, \code{weightedTrueNegativeRate}, \code{weightedTruePositiveRate}.
#'
#' Original parameter name: \code{-metric}.
#' @param parallelRuns the number of parallel runs (defaults to 1). Caution: this option is experimental.
#'
#' Original parameter name: \code{-parallelRuns}.
#' @param outputDebugInfo logical indicating to run the classifier in debug mode which may output additional information (defaults to \code{FALSE}).
#'
#' @return The Auto-WEKA classifier built. The result is represented as a list of the following elements (if applicable):
#' \describe{
#'   \item{\code{call}}{the call that has been made on the function.}
#'   \item{\code{mf}}{the model frame.}
#'   \item{\code{terms}}{the terms of the given forumula.}
#'   \item{\code{instances}}{a S4 object holding the input data as WEKA instances.}
#'   \item{\code{awc}}{a S4 object representing the classifier built.}
#'   \item{\code{cor}}{the correlation coefficient.}
#'   \item{\code{mae}}{the mean absolute error.}
#'   \item{\code{rmse}}{the root mean square error.}
#'   \item{\code{rae}}{the relative absolute error.}
#'   \item{\code{rrse}}{the root relative rquared Error.}
#'   \item{\code{numInstances}}{the number of instances.}
#'   \item{\code{classPredicitions}}{the class memberships for the given test instances.}
#'   \item{\code{probabilityPredictions}}{the class membership probabilities for given test instances.}
#' }
#' In addition, the chosen metric is contained in the result list as well.
#'
#' @author Implementation partially based on the function \code{\link[RWeka]{make_Weka_classifier}} contained in the \href{https://cran.r-project.org/package=RWeka}{RWeka} package.
#'
#' @references
#' C. Thornton, F. Hutter, H. H. Hoos, and K. Leyton-Brown (2013). Auto-WEKA: Combined selection and hyperparameter optimization of classification algorithms. In \emph{Proceedings of the 19th ACM SIGKDD international conference on Knowledge discovery and data mining} (pp. 847-855). ACM.
#'
#' F. Hutter, L. Kotthoff, and J. Vanschoren (2019). Automatic Machine Learning: Methods, Systems, Challenges.
#'
#' K. Hornik, C. Buchta, and A. Zeileis (2009). Open-source machine learning: R meets Weka. \emph{Computational Statistics}, \bold{24}/2, 225--232.
#'
#' Eibe Frank, Mark A. Hall, and Ian H. Witten (2016). The WEKA Workbench. Online Appendix for "Data Mining: Practical Machine Learning Tools and Techniques", Morgan Kaufmann, Fourth Edition, 2016.
#'
#' @examples
#' \dontrun{
#' ## Iris Example
#' data(iris)
#' # Build classifier with time limit of 5 minutes
#' classifier <- autoWekaClassifier(Species ~ ., iris, timeLimit = 5)
#' # Get summary
#' summary(classifier)
#' # Predict class memberships
#' predict(classifier, iris[1:10,-5])
#' # Predict class membership probabilities
#' predict(classifier, iris[1:10,-5], type = "probability")
#'
#' ## mlbench Exmaple
#' require("mlbench")
#' data("HouseVotes84", package = "mlbench")
#' # Build classifier with changed metric
#' classifier <- autoWekaClassifier(Class ~ ., HouseVotes84, metric = "precision")
#' predict(classifier, HouseVotes84[1:10, -1])
#' predict(classifier, HouseVotes84[1:10, -1], type = "probability")
#' }
#' @export
autoWekaClassifier <- function(formula, data, subset, na.action, seed = 123, timeLimit = 15, memoryLimit = 1024, numberOfConfigs = 1,
                               metric = c("errorRate", "areaAboveROC", "areaUnderROC", "avgCost", "correct", "correlationCoefficient",
                                          "falseNegativeRate", "falsePositiveRate", "fMeasure", "incorrect", "kappa", "kBInformation",
                                          "kBMeanInformation", "kBRelativeInformation", "meanAbsoluteError", "pctCorrect", "pctIncorrect",
                                          "precision", "relativeAbsoluteError", "rootMeanSquaredError", "rootRelativeSquaredError",
                                          "weightedAreaUnderROC", "weightedFalseNegativeRate", "weightedFalsePositiveRate", "weightedFMeasure",
                                          "weightedPrecision", "weightedRecall", "weightedTrueNegativeRate", "weightedTruePositiveRate"),
                                    parallelRuns = 1, outputDebugInfo = FALSE) {
  # Create result list
  result <- list()

  # Create model frame from call (see e.g. stats:lm)
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  mf <- mf[c(1L, match(c("formula", "data", "subset", "na.action"), names(mf), 0L))]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")

  # Levels
  result[["levels"]] <- levels(mf[[1L]])
  # Call
  result[["call"]] <- cl
  # Model frame
  result[["mf"]] <- mf
  result[["terms"]] <- mt

  # Map to WEKA instances
  instances = mapToInstances(mf)
  result[["instances"]] <- instances

  # Map function parameters to character vector
  runOptions <- vector()
  runOptions <- c(runOptions, "-seed", seed)
  runOptions <- c(runOptions, "-timeLimit", timeLimit)
  runOptions <- c(runOptions, "-memLimit", memoryLimit)
  runOptions <- c(runOptions, "-nBestConfigs", numberOfConfigs)
  metric <- match.arg(metric)
  runOptions <- c(runOptions, "-metric", metric)
  runOptions <- c(runOptions, "-parallelRuns", parallelRuns)
  if(outputDebugInfo)
    runOptions <- c(runOptions, "-output-debug-info")

  # Start redirecting Java Standard Error Output
  exitFunction <- startRedirectingJavaStdErrOutput()

  # Build AutoWEKAClassifier
  autoWEKAClassifier <- rJava::.jnew("de/wwu/is/RAutoWEKA/RAutoWEKAClassifier") # subclass of weka/classifiers/meta/AutoWEKAClassifier
  rJava::.jcall(autoWEKAClassifier, "V", "setOptions", rJava::.jarray(runOptions))
  rJava::.jcall(autoWEKAClassifier, "V", "buildClassifier", instances)

  # Stop redirecting Java Standard Error Output and output errors if outputDebugInfo = T
  stopRedirectingJavaStdErrOut(exitFunction,outputDebugInfo)

  # Auto WEKA Classifier object
  result[["awc"]] <- autoWEKAClassifier
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
  # Number of instances
  result[["numInstances"]] <- rJava::.jcall(evaluation, "D", "numInstances")
  # Predictions
  if(!is.null(result$levels))
    result[["classPredicitions"]] <- classifyInstances(result, instances)
  result[["probabilityPredictions"]] <- distributionForInstances(result, instances)

  # Set class of result list
  return(structure(result, class = "RAutoWEKAClassifier"))
}

#' Calculates the class memberships for given test instances.
#'
#' @param object the classifier object.
#' @param instances the instances to classify.
#'
#' @return the class memberships.
#'
#' @noRd
classifyInstances <- function(object, instances) {
  # Implementation adapted from the RWeka package (https://cran.r-project.org/package=RWeka)
  prediction <- rJava::.jcall(object$awc, "[D", "classifyInstances", instances)
  is.na(prediction) <- is.nan(prediction)
  if(!is.null(object$levels))
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
  # Implementation adapted from the RWeka package (https://cran.r-project.org/package=RWeka)
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
#' @rdname autoWekaClassifier
#'
#' @export
print.RAutoWEKAClassifier <- function(x, ...) {
    writeLines(rJava::.jcall(x$awc, "S", "toString"))
    invisible(x)
}

#' S3 method for class 'RAutoWEKAClassifier'
#'
#' @rdname autoWekaClassifier
#'
#' @export
summary.RAutoWEKAClassifier <- function(object, ...) {
  writeLines(rJava::.jcall(object$awc, "S", "toString"))
  invisible(object)
}

#' S3 method for class 'RAutoWEKAClassifier'
#'
#' @param newdata the data to classify.
#' @param type the prediction type to output (defaults to \code{class}). Available types:
#' \describe{
#'   \item{\code{class}}{to get class memberships}
#'   \item{\code{probability}}{to get class membership probabilities}
#' }
#'
#' @rdname autoWekaClassifier
#'
#' @export
predict.RAutoWEKAClassifier <- function(object, newdata, type = c("class", "probability"), ...) {
  # Implementation adapted from the RWeka package (https://cran.r-project.org/package=RWeka)
  type <- match.arg(type)

  if (missing(newdata) || is.null(newdata)) {
    if(type == "class") {
      return(object$classPredicitions)
    } else if (type == "probability") {
      return(object$probabilityPredictions)
    }
  }

  mf <- model.frame(delete.response(terms(object)), newdata, na.action = object$call$na.action)
  classes <- NULL
  if(!is.null(object$levels)) {
    classes <- factor(NA, levels = object$levels)
  } else {
    classes <- NA_real_
  }
  mf <- cbind(CLASS = classes, mf)
  instances <- mapToInstances(mf)

  if(type == "class") {
    prediction <- classifyInstances(object, instances)
  } else if(type == "probability") {
    prediction <- distributionForInstances(object, instances)
  }

  return(prediction)
}

#' S3 method for class 'RAutoWEKAClassifier'
#'
#' @rdname autoWekaClassifier
#'
#' @export
model.frame.RAutoWEKAClassifier <- function(formula, ...) {
  return(formula$mf)
}

#' S3 method for class 'RAutoWEKAClassifier'
#'
#' @rdname autoWekaClassifier
#'
#' @export
terms.RAutoWEKAClassifier <- function(x, ...) {
  return(x$terms)
}
