#' Starts redirecting Java Standard Error Output to an dedicated \code{OutputStream}.
#'
#' @return a callback function to stop redirecting.
#'
#' @noRd
startRedirectingJavaStdErrOutput <- function() {
  # Redirect Java Standard Error Output
  err <- rJava::.jfield("java/lang/System", name = "err")
  errBos <- rJava::.jcast(rJava::.jnew("java/io/ByteArrayOutputStream"), "java/io/OutputStream")
  rJava::.jcall("java/lang/System", "V", "setErr", rJava::.jnew("java/io/PrintStream", errBos))

  # To be called by the caller of this function to stop redirecting Java Standard Output
  stopRedirectFunction <- function(print) {
    # Stop redirecting Java Standard Error Output
    rJava::.jcall("java/lang/System", "V", "setErr", err)
    if(print)
      message(rJava::.jcall(errBos, "Ljava/lang/String;", "toString"), appendLF = FALSE)
  }

  return(stopRedirectFunction)
}

#' Stops redirecting Java Standard Error Output and optionally prints the errors that occured during redirection.
#'
#' @param exitFunction the callback function to stop redirecting.
#' @param print logical indicating to print errors that occured during redirection (defaults to \code{FALSE}).
#'
#' @noRd
stopRedirectingJavaStdErrOut <- function(exitFunction, print = FALSE) {
  stopifnot(is.function(exitFunction))
  stopifnot(is.logical(print))

  do.call(exitFunction, list(print))
}

#' Maps a model frame to WEKA instances.
#'
#' @param mf the model frame.
#' @return a WEKA \code{Instances} object.
#'
#' @noRd
mapToInstances <- function(mf) {
  # Implementation adapted from the RWeka package (https://cran.r-project.org/package=RWeka)
  attributeNames <- names(mf)
  attributeInfos <- rJava::.jnew("java/util/ArrayList", as.integer(length(mf)))

  # Extract attributes
  for (i in seq_along(mf)) {
    if(is.logical(mf[[i]]))
      mf[[i]] <- factor(mf[[i]])

    attribute <- NULL

    if(is.factor(mf[[i]])) {
      levels <- rJava::.jnew("java/util/ArrayList", as.integer(nlevels(mf[[i]])))
      sapply(levels(mf[[i]]), function(k) rJava::.jcall(levels, "Z", "add", rJava::.jcast(rJava::.jnew("java/lang/String", k), "java/lang/Object")))
      mf[[i]] <- as.double(mf[[i]]) - 1
      attribute <- rJava::.jnew("weka/core/Attribute", attributeNames[i], rJava::.jcast(levels, "java/util/List"))
    } else if(is.character(mf[[i]])) {
      attribute <- rJava::.jnew("weka/core/Attribute", attributeNames[i], rJava::.jnull("java/util/List"))
      mf[[i]] <- as.factor(mf[[i]])
      index <- sapply(levels(mf[[i]]), function(k) rJava::.jcall(attribute, "I", "addStringValue", k))
      if(any(index < 0))
        stop("Mapping of character failed")
      mf[[i]] <- as.double(index[as.integer(mf[[i]])])
    } else if(inherits(mf[[i]], "Date")) {
      attribute <- rJava::.jnew("weka/core/Attribute", attributeNames[i], "yyyy-MM-dd")
      mf[[i]] <- rJava::.jcall("de/wwu/is/RAutoWEKA/Util", "[D", "parseDate", attribute, rJava::.jarray(format(mf[[i]])), NA_character_)
    } else if(inherits(mf[[i]], "POSIXt")) {
      attribute <- rJava::.jnew("weka/core/Attribute", attributeNames[i], "yyyy-MM-dd HH:mm:ss")
      mf[[i]] <- rJava::.jcall("de/wwu/is/RAutoWEKA/Util", "[D", "parseDate", attribute, rJava::.jarray(format(mf[[i]], tz = "")), NA_character_)
    } else if(is.numeric(mf[[i]]))
      attribute <- rJava::.jnew("weka/core/Attribute", attributeNames[i])
    else
      stop("Unknown type")

    rJava::.jcall(attributeInfos, "Z", "add", rJava::.jcast(attribute, "java/lang/Object"))
  }

  # Add instances
  numberOfInstances <- dim(mf)[1L]
  instances <- rJava::.jnew("weka/core/Instances", "R_data_frame",  attributeInfos, as.integer(numberOfInstances))

  classIndex <- attr(attr(mf, "terms"), "response")
  if(is.null(classIndex))
    classIndex <- 1L

  if(classIndex > 0L)
    rJava::.jcall(instances, "V", "setClassIndex", as.integer(classIndex - 1L))

  mf <- unlist(mf, use.names = FALSE)
  mf[is.na(mf)] <- NaN

  rJava::.jcall("de/wwu/is/RAutoWEKA/Util", "V", "addInstances", instances, rJava::.jarray(mf), as.integer(numberOfInstances))

  return(instances)
}
