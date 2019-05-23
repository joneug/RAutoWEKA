startRedirectingJavaStdErrOutput <- function() {
  # Redirect Java Standard Error Output
  err <- rJava::.jfield("java/lang/System", name = "err")
  errBos <- rJava::.jcast(rJava::.jnew("java/io/ByteArrayOutputStream"), "java/io/OutputStream")
  rJava::.jcall("java/lang/System", "V", "setErr", rJava::.jnew("java/io/PrintStream", errBos))

  # To be called by the caller of this function to stop redirecting Java Standard Output
  stopRedirectFunction <- function(print) {
    ## Stop redirecting Java Standard Error Output
    rJava::.jcall("java/lang/System", "V", "setErr", err)
    if(print)
      message(rJava::.jcall(errBos, "Ljava/lang/String;", "toString"), appendLF = FALSE)
  }

  return(stopRedirectFunction)
}

stopRedirectingJavaStdErrOut <- function(exitFunction, print) {
  stopifnot(is.function(exitFunction))
  stopifnot(is.logical(print))

  do.call(exitFunction, list(print))
}
