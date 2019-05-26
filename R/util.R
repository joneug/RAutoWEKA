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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Adapted from RWeka (0.4-40)
# Authors: Kurt Hornik [aut, cre], Christian Buchta [ctb], Torsten Hothorn [ctb], Alexandros Karatzoglou [ctb], David Meyer [ctb], Achim Zeileis [ctb]
# Link: https://cran.r-project.org/package=RWeka
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

read_model_frame_into_Weka <-
  function(mf)
  {
    ## Argh.  We cannot necessarily assume that there is a Weka-sense
    ## "class attributes" (response variable), but for classifiers we
    ## currently employ .default_data_handler_for_classifiers() to drop
    ## unused variables, which also eliminates the model frame terms.
    ## Hence, cannot simply do
    ##   idx <- attr(terms(mf), "response")
    ## and need the following hack instead:
    idx <- attr(attr(mf, "terms"), "response")
    if(is.null(idx)) idx <- 1L
    ## Still better than previous versions which hard-wired the 1L.
    read_data_into_Weka(mf, idx)
  }


read_data_into_Weka <-
  function(x, classIndex = 0L)
  {
    ## FastVector was deprecated in Weka >= 3-7-1. Now we have to use
    ## the List interface (see the cast of ArrayList in the Attribute
    ## constructor).

    ## See the Weka 3-5-7 source code for this insanity (e.g., string).
    ## Note that the class index, if any, must be set as an attribute.

    ## Be nice.
    if(!is.data.frame(x))
      x <- as.data.frame(x)

    ## As Weka instance objects do not have case/row names, we store
    ## such information in the R container for the Weka instances.  For
    ## simplicity, we store the dimnames including the (variable) names
    ## also contained in the Weka instances.
    dx <- dim(x)
    dnx <- dimnames(x)

    ## Build attribute information
    attname <- names(x)
    attinfo <- rJava::.jnew("java/util/ArrayList",
                            as.integer(length(x)))
    for (i in seq_along(x)) {
      ## Make logicals into Weka nominals.
      if(is.logical(x[[i]]))
        x[[i]] <- factor(x[[i]])
      attribute <-
        if(is.factor(x[[i]])) {
          levels <- rJava::.jnew("java/util/ArrayList",
                                 as.integer(nlevels(x[[i]])))
          sapply(levels(x[[i]]), function(k)
            rJava::.jcall(levels, "Z", "add",
                          rJava::.jcast(rJava::.jnew("java/lang/String", k),
                                        "java/lang/Object")))
          ## shift to Weka's internal coding
          x[[i]] <- as.double(x[[i]]) - 1
          rJava::.jnew("weka/core/Attribute", attname[i],
                       rJava::.jcast(levels, "java/util/List"))
        }
      else if(is.character(x[[i]])) {
        att <- rJava::.jnew("weka/core/Attribute", attname[i],
                            rJava::.jnull("java/util/List"))
        x[[i]] <- as.factor(x[[i]])
        index <- sapply(levels(x[[i]]), function(k)
          rJava::.jcall(att, "I", "addStringValue", k))
        if(any(index < 0))
          stop("pushing to Type 'string' failed")
        x[[i]] <- as.double(index[as.integer(x[[i]])])

        att
      }
      else if(inherits(x[[i]], "Date")) {
        att <- rJava::.jnew("weka/core/Attribute", attname[i],
                            "yyyy-MM-dd")
        x[[i]] <- rJava::.jcall("de/wwu/is/RAutoWEKA/Util", "[D", "parseDate", att,
                                rJava::.jarray(format(x[[i]])),
                                NA_character_)
        att
      }
      else if(inherits(x[[i]], "POSIXt")) {
        att <- rJava::.jnew("weka/core/Attribute", attname[i],
                            "yyyy-MM-dd HH:mm:ss")
        ## Normalize to local time.
        x[[i]] <- rJava::.jcall("de/wwu/is/RAutoWEKA/Util", "[D", "parseDate", att,
                                rJava::.jarray(format(x[[i]], tz = "")),
                                NA_character_)
        att
      }
      else if(is.numeric(x[[i]]))
        rJava::.jnew("weka/core/Attribute", attname[i])
      else
        stop("Type not implemented")
      rJava::.jcall(attinfo, "Z", "add",
                    rJava::.jcast(attribute, "java/lang/Object"))
    }

    ## Build instances.
    n <- dim(x)[1L]                     # number of instances
    instances <- rJava::.jnew("weka/core/Instances",
                              "R_data_frame",  # FIXME
                              attinfo,
                              as.integer(n))   # capacity

    ## Set class index.
    if(classIndex > 0L)
      rJava::.jcall(instances, "V", "setClassIndex",
                    as.integer(classIndex - 1L))

    ## Populate.
    x <- unlist(x, use.names = FALSE)
    x[is.na(x)] <- NaN                  # Weka missing value.

    rJava::.jcall("de/wwu/is/RAutoWEKA/Util", "V", "addInstances",
                  instances, rJava::.jarray(x), as.integer(n))

    ## Note that using dim and dimnames attributes would result in a
    ## matrix, which seems a bad idea.
    .structure(instances, .dim = dx, .dimnames = dnx)
  }

## <NOTE>
## Ideally we would like to add an S3 class to Weka instance objects
## (i.e., jobjRefs to weka.core.Instances objects), but this does not
## work seemlessly (yet?).  E.g., when doing
##   structure(instances,
##             class = unique(c("Weka_instances", class(instances))))
## a subsequent
##   .jcast(instances, "weka/core/Instances")
## will fail with
##   Error in getClass(cl) :
##   c("\"Weka_instances\" is not a defined class", "\"jobjRef\" is not a defined class")
##  Calls: example ... .jcast -> @<- -> slot<- -> checkSlotAssignment -> getClass
## Not clear if this should work or not, and note that of course things
## would work along the lines of
##   .jcast(structure(instances, class = "jobjRef"),
##          "weka/core/Instances")
## Alternatively, we could have a container class
##   structure(.Data = instances, .Meta = list(......),
##             class = "Weka_instances")
## and directly extract the data "slot" in package computations ...
##
## If we start exposing Weka instances to some extent (e.g., optionally
## in a fitted classifier) then something classed would be good.  We
## could then provide methods like the following:
dim.Weka_instances <-
  function(x)
    attr(x, ".dim")
dimnames.Weka_instances <-
  function(x)
    attr(x, ".dimnames")
print.Weka_instances <-
  function(x, ...)
  {
    writeLines(rJava::.jcall(x, "S", "toString"))
    invisible(x)
  }
summary.Weka_instances <-
  function(x, ...)
  {
    writeLines(rJava::.jcall(x, "S", "toSummaryString"))
  }
## (Not perfect because this returns nothing useful.  We could of course
## parse the toSummaryString() results ...)
as.data.frame.Weka_instances <-
  function(x, row.names = NULL, ...)
  {
    if(is.null(row.names))
      row.names <- attr(x, ".dimnames")[[1L]]
    .structure(read_instances_from_Weka(x), row.names = row.names)
  }
## and so on ...

.structure <-
  function(x, ...)
    `attributes<-`(x, c(attributes(x), list(...)))
