# Adapted from RWeka (0.4-40)
# Authors: Kurt Hornik [aut, cre], Christian Buchta [ctb], Torsten Hothorn [ctb], Alexandros Karatzoglou [ctb], David Meyer [ctb], Achim Zeileis [ctb]
# Link: https://cran.r-project.org/package=RWeka

read.arff <-
function(file)
{
    ## Copy the data from a connection to a temporary file.
    if(!is.character(file)) {
        if(!inherits(file, "connection"))
            stop("Argument 'file' must be a character string or connection.")
        con <- file
        if(!isOpen(con, "r")) {
            open(con, "r")
            on.exit(close(con))
        }
        file <- tempfile()
        on.exit(unlink(file), add = TRUE)
        writeLines(readLines(con), file)
    }

    ## Read the ARFF file into Weka.
    reader <- rJava::.jnew("java/io/FileReader", file)
    instances <- rJava::.jnew("weka/core/Instances",
                       .jcast(reader, "java/io/Reader"))

    read_instances_from_Weka(instances)
}


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

write.arff <-
function(x, file, eol = "\n")
{
    ## <NOTE>
    ## We could also write from Weka.  However, in case of a connection
    ## we would have to write to a temporary file and then copy back.
    ## </NOTE>
    if(file == "")
        file <- stdout()
    else if(is.character(file)) {
        file <- file(file, 'w')
        on.exit(close(file))
    }
    else if(!isOpen(file, "w")) {
        open(file, "w")
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("Argument 'file' must be a character string or connection.")

    .write_ARFF_to_con(x, file, eol)
}

.write_ARFF_to_con <-
function(x, con, eol)
    UseMethod(".write_ARFF_to_con")

.write_ARFF_to_con.default <-
function(x, con, eol)
{
    if(!is.data.frame(x) && !is.matrix(x))
        x <- data.frame(x)

    instances <- read_data_into_Weka(x)
    text <- rJava::.jcall(instances, "Ljava/lang/String;", "toString")

    writeLines(unlist(strsplit(text, "\n", fixed = TRUE)), con, sep = eol)
}

.write_ARFF_to_con.simple_triplet_matrix <-
function(x, con, eol)
{
    ## Suggested by Stefan Wilhelm <wilhelm@financial.com>.

    ## Extract attribute names from x,
    ## If colnames(x) is NULL, use dummy variable names "V1", "V2"...
    ## Use low-level access as further below we do this anyways.
    cnames <- x$dimnames[[2L]]
    if(is.null(cnames))
        cnames <- sprintf("V%d", seq_len(x$ncol))

    ## Data types for attributes in ARFF.
    ## Since we have a matrix, all columns have the same data type.
    dtype <- switch(EXPR = mode(x$v),
                    character = "string",
                    numeric = "numeric",
                    logical = "{0,1}",
                    "string")

    ## Write ARFF Header
    writeLines(c("@relation R_simple_triplet_matrix", ""),
               con, sep = eol)

    ## Write ARFF Attributes
    writeLines(c(sprintf("@attribute '%s' %s", cnames, dtype), ""),
               con, sep = eol)

    ## Write ARFF Data
    writeLines("@data", con, sep = eol)
    ## Logical values TRUE/FALSE will be represented as 1/0.
    if(mode(x$v) == "logical")
        x$v <- as.numeric(x$v)
    for(i in seq_len(x$nrow)) {
        ind <- x$i == i
        ## Note that missing values are encoded as "?".
        ## (http://weka.wikispaces.com/ARFF+%28developer+version%29)
        v <- x$v[ind]
        v[is.na(v)] <- "?"
        ## Note that index in sparse ARFF starts with 0.
        writeLines(sprintf("{%s}",
                           paste(sprintf("%d %s", x$j[ind] - 1L, v),
                                 collapse = ",")),
                   con, sep = eol)
    }
}

## <NOTE>
## Ideally, we would have a common R/Weka data frame object which can be
## used bidirectionally, and only converts from R data frame to Weka
## instances and vice versa when needed to optimize performance (e.g.,
## when repeatedly using Weka filters).  Currently, this is not really
## possible.
## </NOTE>

read_instances_from_Weka <-
function(x)
{
    ## See Weka 3-5-7 classes Instances and Attribute.

    ## Get attribute information
    out <- vector("list", rJava::.jcall(x, "I", "numAttributes"))
    for (i in seq_along(out)) {
        ## In Weka missing values are coded as NaN and the cast
        ## to double should ensure this for all attribute types.
        out[[i]] <- rJava::.jcall(x, "[D", "attributeToDoubleArray",
                           as.integer(i - 1L))
        attribute <- rJava::.jcall(x, "Lweka/core/Attribute;",
                            "attribute", as.integer(i - 1L))
        names(out)[i] <- rJava::.jcall(attribute, "S", "name")
        ## See the Constant Field Values in the Weka documentation.
        switch(rJava::.jcall(attribute, "I", "type") + 1L,
           {   ## 0 numeric (nothing todo)

               is.na(out[[i]]) <- is.nan(out[[i]])
           },

           {   ## 1 nominal (Weka value code = R level code - 1)

               idx <- seq(.jcall(attribute, "I", "numValues")) - 1L
               is.na(out[[i]]) <- is.nan(out[[i]])
               out[[i]] <- factor(out[[i]], levels = idx)
               levels(out[[i]]) <-
                   vapply(idx,
                          function(k)
                              rJava::.jcall(attribute, "S", "value",
                                     as.integer(k)),
                          "")
               ## Assume logical (see below).
               if(all(match(levels(out[[i]]), c("FALSE", "TRUE"),
                       nomatch = 0L)))
                   out[[i]] <- out[[i]] == "TRUE"
           },

           {   ## 2 string (same as 1 but return as character)

               idx <- seq(rJava::.jcall(attribute, "I", "numValues")) - 1L
               is.na(out[[i]]) <- is.nan(out[[i]])
               out[[i]] <- factor(out[[i]], levels = idx)
               levels(out[[i]]) <-
                   vapply(idx,
                          function(k)
                              rJava::.jcall(attribute, "S", "value",
                                     as.integer(k)),
                          "")
               out[[i]] <- as.character(out[[i]])
           },

           {   ## 3 date (the direct approach is not reliable)
               ##

               ## Format date.
               out[[i]] <- rJava::.jcall("RWekaInterfaces", "[Ljava/lang/String;",
                                  "formatDate", attribute, rJava::.jarray(out[[i]]),
                                  NA_character_)
	       ## Fix for R >= 2.13.x
	       is.na(out[[i]]) <- out[[i]] == "NA"
               ## Represent date in local time.
               out[[i]] <- as.POSIXct(out[[i]], tz = "")
           },

           {   ## 4 relational
               stop("Type 'relational' currently not implemented.")
           },

           {   ## unknown
               stop("Type not implemented")
           }
        )
    }
    ## NOTE that Weka codes a missing class attribute as -1.
    classIndex <- rJava::.jcall(x, "I", "classIndex") + 1L
    if(classIndex && classIndex != length(out))
        out <- c(out[-classIndex], out[classIndex])

    ## Prevent garbling of attribute names, etc.
    data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
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
                x[[i]] <- rJava::.jcall("RWekaInterfaces", "[D", "parseDate", att,
                                        rJava::.jarray(format(x[[i]])),
                                 NA_character_)
                att
            }
            else if(inherits(x[[i]], "POSIXt")) {
                att <- rJava::.jnew("weka/core/Attribute", attname[i],
                             "yyyy-MM-dd HH:mm:ss")
                ## Normalize to local time.
                x[[i]] <- rJava::.jcall("RWekaInterfaces", "[D", "parseDate", att,
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
    rJava::.jcall("RWekaInterfaces", "V", "addInstances",
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
