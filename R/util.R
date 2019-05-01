#' Build Auto-WEKA classifier
#'
#' This functions builds an Auto-WEKA classifier.
#'
#' @param trainingFile the name of the file containing the training data. This can be the path to a local file or a URL.
#'
#' Original parameter name: \code{-t}.
#' @param testingFile the name of the file containing the test data. If missing, a cross-validation will be performed on the training data. This can be the path to a local file or a URL.
#'
#' Original parameter name: \code{-T}.
#' @param classIndex the index of the class attribute (defaults to last). Possible values are \code{first}, \code{last} or the actual index.
#'
#' Original parameter name: \code{-c}.
#' @param numberOfFolds the number of folds for cross-validation (defaults to 10).
#'
#' Original parameter name: \code{-x}.
#' @param noCrossValidation logical indicating if cross validation should be pervormed (defualts to \code{FALSE}).
#'
#' Original parameter name: \code{-no-cv}.
#' @param forceBatchTraining logical indicating to train classifier in batch mode instead of incrementally (defualts to \code{FALSE}).
#'
#' Original parameter name: \code{-force-batch-training}.
#' @param splitPercentage the percentage for splitting into train/test set (e.g. 66).
#'
#' Original parameter name: \code{-split-percentage}.
#' @param preserveOrder logical indicating to preserve the order in the percentage split (defaults to \code{FALSE}).
#'
#' Original parameter name: \code{-preserve-order}.
#' @param randomNumberSeed the random number seed for cross-validation or percentage split (defaults to 1).
#'
#' Original parameter name: \code{-s}.
#' @param costMatrixFile the name of the file containing the cost matrix.
#'
#' Original parameter name: \code{-m}.
#' @param toggle a comma separated list of metric names to toggle in the output. All metrics are output by default with the exception of 'Coverage' and 'Region size'. Available metrics:
#'
#' Correct, Incorrect, Kappa, Total cost, Average cost, KB relative, KB information, Correlation, Complexity 0, Complexity scheme, Complexity improvement, MAE, RMSE, RAE, RRSE, Coverage, Region size, TP rate, FP rate, Precision, Recall, F-measure, MCC, ROC area, PRC area
#'
#' Original parameter name: \code{-toggle}.
#' @param modelInputFile the name of the model input file. In case the filename ends with \code{.xml}, a PMML file is loaded or, if that fails, options are loaded from the XML file.
#'
#' Original parameter name: \code{-l}.
#' @param modelOutputFile the name of the model output file. In case the filename ends with \code{.xml}, only the options are saved to the XML file, not the model.
#'
#' Original parameter name: \code{-d}.
#' @param noStatistics logical indicating not to output statistics for training data (defaults to \code{FALSE}).
#'
#' Original parameter name: \code{-v}.
#' @param onlyStatistics logical indicating to output statistics only, not the classifier (defaults to \code{FALSE}).
#'
#' Original parameter name: \code{-o}.
#' @param noPerClassStatistics logical indicating to not output statistics for each class (defaults to \code{FALSE}).
#'
#' Original parameter name: \code{-do-not-output-per-class-statistics}.
#' @param informationTheoreticStatistics logical indicating to output information-theoretic statistics (defaults to \code{FALSE}).
#'
#' Original parameter name: \code{-k}.
#' @param classificationOutputType the type of output to generate for the classification. Available types:
#'
#' CSV, CSVTab, HTML, PlainText, XML, Null
#'
#' Original parameter name: \code{-classifications}.
#' @param cumulativeMarginDistribution logical indicating to only output the cumulative margin distribution.
#'
#' Original parameter name: \code{-r}.
#' @param optionsXml the name of a XML file or a XML string contianing the options.
#'
#' Original parameter name: \code{-xml}.
#' @param thresholdFile the name of the file to save the threshold data to. The format is determined by the file extension (e.g. \code{.arff} for ARFF format or \code{.csv} for CSV).
#'
#' Original parameter name: \code{-threshold-file}.
#' @param thresholdLabel the class label to determine the threshold data for (defaults to the first label).
#'
#' Original parameter name: \code{-threshold-label}.
#' @param noPredictions logical indicating to not collect predictions in order to conserve memory (defaults to \code{FALSE}).
#'
#' Original parameter name: \code{-no-predictions}.
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
#' buildAutoWekaClassifier("http://storm.cis.fordham.edu/~gweiss/data-mining/weka-data/iris.arff", timeLimit = 1, noCrossValidation = TRUE)
#' }
#' @export
buildAutoWekaClassifier <- function(trainingFile, testingFile, classIndex = "last", numberOfFolds = 10, noCrossValidation = FALSE,
                                    forceBatchTraining = FALSE, splitPercentage, preserveOrder = FALSE, randomNumberSeed = 1, costMatrixFile,
                                    toggle, modelInputFile, modelOutputFile, noStatistics = FALSE, onlyStatistics = FALSE,
                                    noPerClassStatistics = FALSE, informationTheoreticStatistics = FALSE, classificationOutputType,
                                    cumulativeMarginDistribution = FALSE, optionsXml, thresholdFile, thresholdLabel, noPredictions = FALSE,
                                    seed = 123, timeLimit = 15, memoryLimit = 1024, numberOfConfigs = 1, metric = "errorRate",
                                    parallelRuns = 1, outputDebugInfo = FALSE, noCapabiltiesCheck = FALSE, decimalPlaces = 2,
                                    batchSize = 100) {
  runOptions = vector()

  if(!missing(trainingFile))
    runOptions = c(runOptions, "-t", trainingFile)
  if(!missing(testingFile))
    runOptions = c(runOptions, "-T", testingFile)
  runOptions = c(runOptions, "-c", classIndex)
  runOptions = c(runOptions, "-x", numberOfFolds)
  if(noCrossValidation)
    runOptions = c(runOptions, "-no-cv")
  if(forceBatchTraining)
    runOptions = c(runOptions, "-force-batch-training")
  if(!missing(splitPercentage))
    runOptions = c(runOptions, "-split-percentage", splitPercentage)
  if(preserveOrder)
    runOptions = c(runOptions, "-preserve-order")
  runOptions = c(runOptions, "-s", randomNumberSeed)
  if(!missing(costMatrixFile))
    runOptions = c(runOptions, "-m", costMatrixFile)
  if(!missing(toggle))
    runOptions = c(runOptions, "-toggle", toggle)
  if(!missing(modelInputFile))
    runOptions = c(runOptions, "-l", modelInputFile)
  if(!missing(modelOutputFile))
    runOptions = c(runOptions, "-d", modelOutputFile)
  if(noStatistics)
    runOptions = c(runOptions, "-v")
  if(onlyStatistics)
    runOptions = c(runOptions, "-o")
  if(noPerClassStatistics)
    runOptions = c(runOptions, "-do-not-output-per-class-statistics")
  if(informationTheoreticStatistics)
    runOptions = c(runOptions, "-k")
  if(!missing(classificationOutputType)) {
    packageBase = "\"weka.classifiers.evaluation.output.prediction."
    className = switch(classificationOutputType,
                       "CSV" = paste(packageBase, "CSV\"", sep = ""),
                       "CSVTab" = paste(packageBase, "CSV -use-tab\"", sep = ""),
                       "HTML" = paste(packageBase, "HTML\"", sep = ""),
                       "PlainText" = paste(packageBase, "PlainText\"", sep = ""),
                       "XML" = paste(packageBase, "XML\"", sep = ""),
                       "Null" = paste(packageBase, "Null\"", sep = "")
    )
    runOptions = c(runOptions, "-classifications", className)
    # TODO: Additional options are available in weka.classifiers.evaluation.output.prediction.AbstractOutput
  }
  if(cumulativeMarginDistribution)
    runOptions = c(runOptions, "-r")
  if(!missing(optionsXml))
    runOptions = c(runOptions, "-xml", optionsXml)
  if(!missing(thresholdFile))
    runOptions = c(runOptions, "-threshold-file", thresholdFile)
  if(!missing(thresholdLabel))
    runOptions = c(runOptions, "-threshold-label", thresholdLabel)
  if(noPredictions)
    runOptions = c(runOptions, "-no-predictions")
  runOptions = c(runOptions, "-timeLimit", timeLimit)
  runOptions = c(runOptions, "-memLimit", memoryLimit)
  runOptions = c(runOptions, "-nBestConfigs", numberOfConfigs)
  runOptions = c(runOptions, "-metric", metric)
  runOptions = c(runOptions, "-parallelRuns", parallelRuns)
  if(outputDebugInfo)
    runOptions = c(runOptions, "-output-debug-info")
  if(noCapabiltiesCheck)
    runOptions = c(runOptions, "-do-not-check-capabilities")
  runOptions = c(runOptions, "-num-decimal-places", decimalPlaces)
  runOptions = c(runOptions, "-batch-size", batchSize)

  autoWEKAClassifier = rJava::.jnew("weka.classifiers.meta.AutoWEKAClassifier")
  rJava::J("weka.classifiers.AbstractClassifier")$runClassifier(
    autoWEKAClassifier,
    rJava::.jarray(runOptions)
  )
}
