#' @title Aggregate variables in LSA datasets
#'
#' @description \code{lsa.aggregate.vars} aggregates continuous variables by group and appends them to the dataset.
#'
#' @param data.file       The file containing \code{lsa.data} object. Either this or
#'                        \code{data.object}
#'                        shall be specified, but not both. See details.
#' @param data.object     The object in the memory containing \code{lsa.data} object. Either this
#'                        or \code{data.file} shall be specified, but not both. See details.
#' @param group.vars      Variable(s) to aggregate the \code{src.variables} by. If no grouping
#'                        variables are provided, the \code{src.variables} will be aggregated on
#'                        country level. See details.
#' @param src.variables   Names of the variables to aggregate. Accepts only continuous variables.
#'                        No PV variables are accepted. See details.
#' @param new.variables   The names of the new, aggregated variables to append to the dataset.
#'                        See details.
#' @param new.var.labels  Optional, vector of strings to add as variable labels for the
#'                        \code{new.variables}. See details.
#' @param aggr.fun        Function to apply when aggregating the \code{variable}. Accepts
#'                        \code{mean} (default), \code{median}, or \code{mode}. See details.
#' @param out.file        Full path to the \code{.RData} file to be written. If missing, the
#'                        original object will be overwritten in the memory. See examples.
#'
#' @details
#' The function aggregates continuous variables in large-scale assessments' data. The aggregation can be done by groups defined by the \code{group.vars}. Multiple grouping variables can be specified. All aggregations are done within each country separately.
#'
#' Either \code{data.file} or \code{data.object} shall be provided as source of data. If both of them are provided, the function will stop with an error message.
#'
#' The \code{src.variables} specifies the variables that shall be aggregated. Only continuous variables are accepted. PVs are not accepted.
#'
#' The \code{new.variables} argument is optional and specifies the names of the new variables aggregated from the \code{src.variables}. The sequence of the \code{new.variables} names is the same as the \code{src.variables}. If the \code{new.variables} argument is omitted, the function will create the names automatically, appending \code{AGGR} at the end of the \code{src.variables} and store the aggregated variable data under these names. If provided, the number of \code{new.variables} must be the same as the number of \code{src.variables}.
#'
#' The \code{new.var.labels} is optional. Regardless whether \code{new.variables} are provided, if \code{new.var.labels} are provided, they will be assigned to the \code{new.variables} generated from the aggregation. If neither \code{new.variables} not \code{new.var.labels} are provided, the function will automatically generate \code{new.variables} (see above) and copy the variable labels from \code{src.variables} to the newly generated variables, appending \code{Aggregated} at the beginning. The argument takes a vector with the same number of elements as the number of variable names in \code{src.variables}.
#'
#' The \code{aggr.fun} specifies the function to be applied when performing the aggregation. The acceptable values are \code{mean} (default), \code{median} and \code{mode}. Using these methods, the aggregation will be performed by groups defined by the \code{group.vars} within each country.
#'
#' If full path to \code{.RData} file is provided to \code{out.file}, the data.set will be written to that file. If no, the complemeted data will remain in the memory.
#'
#' @return
#' A \code{lsa.data} object in memory (if \code{out.file} is missing) or \code{.RData} file containing \code{lsa.data} object with the new aggregated variables.
#'
#' @examples
#'
#' # Aggregate the PIRLS 2021 Students Like Reading and the Home Resources for Learning scales per
#' # school and save the dataset into a file, overwiriting it. The names for the new variables
#' # are automatically generated.
#' \dontrun{
#' lsa.aggregate.vars(data.file = "C:/Data/PIRLS_2021_Student_Miss_to_NA.RData",
#' src.variables = c("ASBGSLR", "ASBGHRL"), group.vars = "IDSCHOOL",
#' out.file = "/tmp/test.RData")
#' }
#'
#' # Same as the above, but assign custom variable names and their labels, and write the data to
#' # the memory instead of saving it on the disk.
#' \dontrun{
#' lsa.aggregate.vars(data.file = "C:/Data/PIRLS_2021_Student_Miss_to_NA.RData",
#' src.variables = c("ASBGSLR", "ASBGHRL"), new.variables = c("LIKEREAD", "LRNRES"),
#' new.var.labels = c("Aggregated like reading", "Aggregated learning resources"),
#' group.vars = "IDSCHOOL",
#' out.file = "/tmp/test.RData")
#' }
#'
#' @seealso \code{\link{lsa.convert.data}}
#' @export
lsa.aggregate.vars <- function(data.file, data.object, group.vars, src.variables, new.variables, new.var.labels, aggr.fun, out.file) {
  tmp.options <- options(scipen = 999, digits = 22)
  on.exit(expr = options(tmp.options), add = TRUE)
  if(missing(new.variables)) {
    new.variables <- paste0(src.variables, "DCZJ")
  }
  if(!missing(new.variables) & missing(new.var.labels)) {
    new.var.labels <- NULL
  } else if(!missing(new.variables) & !missing(new.var.labels)) {
    new.var.labels <- new.var.labels
  } else if(missing(new.variables) & missing(new.var.labels)) {
    new.var.labels <- NULL
  }
  if(!missing(new.variables) & length(new.variables) < length(src.variables)) {
    stop('\nThe number of "new.variables" is less than the number of "src.variables". All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(length(grep(pattern = paste(unname(unlist(studies.all.design.variables[["PV.roots"]])), collapse = "|"), x = src.variables)) > 0) {
    stop('\nPlausible values variables were passed to "src.variables". Aggregation of plausible values is not permitted. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(!missing(new.variables) & !is.null(new.var.labels) & !is.atomic(new.var.labels)) {
    stop('\nThe "new.var.labels" argument accepts only atomic vectors (i.e. no lists or other kinds of objects). All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(!is.null(new.var.labels) & length(new.variables) != length(new.var.labels)) {
    stop('\nThe number of "new.var.labels" must be equal to the number of "new.variables". All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  warnings.collector <- list()
  if(!missing(data.file) == TRUE && !missing(data.object) == TRUE) {
    stop('\nEither "data.file" or "data.object" has to be provided, but not both. All operations stop here. Check your input.\n\n', call. = FALSE)
  } else if(!missing(data.file)) {
    if(file.exists(data.file) == FALSE) {
      stop('\nThe file specified in the "data.file" argument does not exist. All operations stop here. Check your input.\n\n', call. = FALSE)
    }
    ptm.data.import <- proc.time()
    data <- copy(import.data(path = data.file))
    used.data <- deparse(substitute(data.file))
    message('\nData file ', used.data, ' imported in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.data.import}[[3]], "%H:%M:%OS3"))
  } else if(!missing(data.object)) {
    if(length(all.vars(match.call())) == 0) {
      stop('\nThe object specified in the "data.object" argument is quoted, is this an object or a path to a file? All operations stop here. Check your input.\n\n', call. = FALSE)
    }
    if(!exists(all.vars(as.list(match.call())[["data.object"]]))) {
      stop('\nThe object specified in the "data.object" argument does not exist. All operations stop here. Check your input.\n\n', call. = FALSE)
    }
    data <- copy(data.object)
    used.data <- deparse(substitute(data.object))
    message('\nUsing data from object "', used.data, '".')
  }
  if(!"lsa.data" %in% class(data)) {
    stop('\nThe data is not of class "lsa.data". All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(any(new.variables %in% colnames(data))) {
    stop('\nOne or more variables specified in "new.variables" already exists in the data set. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  number.of.countries <- length(unique(data[ , get(key(data))]))
  if(number.of.countries == 1) {
    message("\nValid data from one country have been found. Some computations can be rather intensive. Please be patient.\n")
  } else if(number.of.countries > 1) {
    message("\nValid data from ", number.of.countries, " countries have been found. Some computations can be rather intensive. Please be patient.\n")
  }
  vars.list <- get.analysis.and.design.vars(data)
  if(any(vars.list[["src.variables"]] %in% unname(unlist(studies.all.design.variables[["sampling.vars"]])))) {
    stop('\nSampling design variables were passed to "src.variables". Aggregation of sampling design variables is not permitted. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(isFALSE(all(sapply(X = data[ , mget(vars.list[["src.variables"]])], FUN = class) == "numeric"))) {
    stop('\nThe "src.variables" contain names of non-numeric variables. Factor and character variables are not accepted. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  ptm.aggregation <- proc.time()
  if(missing(aggr.fun)) {
    aggr.fun <- "mean"
  } else {
    aggr.fun <- aggr.fun
  }
  if(!aggr.fun %in% c("mean", "median", "mode")) {
    stop('\nUnknown aggregation function is passed to "aggr.fun". The accepted functions are "mean", "median" or "mode". All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  data[ , (new.variables) := lapply(.SD, function(i) {
    if(aggr.fun == "mean") {
      mean(x = i, na.rm = TRUE)
    } else if(aggr.fun == "median") {
      median(x = i, na.rm = TRUE)
    } else if(aggr.fun == "mode") {
      compute.unweighted.mode(x = i, na.rm = TRUE)
    }
  }), .SDcols = src.variables, by = c(key(data), vars.list[["group.vars"]])]
  if(length(grep(pattern = "DCZJ$", x = colnames(data))) > 0) {
    orig.vars <- TRUE
  } else {
    orig.vars <- FALSE
  }
  if(isTRUE(orig.vars) & is.null(new.var.labels)) {
    src.labels <- lapply(X = data[ , mget(src.variables)], FUN = function(i) {
      paste("Aggregated", attr(x = i, which = "variable.label"))
    })
    names(src.labels) <- new.variables
    lapply(X = new.variables, FUN = function(i) {
      data[ , setattr(x = eval(parse(text = i)), name = "variable.label", value = src.labels[[i]])]
    })
    setnames(x = data, old = new.variables, new = gsub(pattern = "DCZJ$", replacement = "AGGR", x = new.variables))
  } else if(isTRUE(orig.vars) & !is.null(new.var.labels)) {
    new.var.labels <- as.list(new.var.labels)
    names(new.var.labels) <- new.variables
    lapply(X = new.variables, FUN = function(i) {
      data[ , setattr(x = eval(parse(text = i)), name = "variable.label", value = new.var.labels[[i]])]
    })
    setnames(x = data, old = new.variables, new = gsub(pattern = "DCZJ$", replacement = "AGGR", x = new.variables))
  } else if(isFALSE(orig.vars) & !is.null(new.var.labels)) {
    new.var.labels <- as.list(new.var.labels)
    names(new.var.labels) <- new.variables
    lapply(X = new.variables, FUN = function(i) {
      data[ , setattr(x = eval(parse(text = i)), name = "variable.label", value = new.var.labels[[i]])]
    })
  }
  if(length(unique(data[ , get(key(data))])) == 1 & length(src.variables) == 1) {
    message('One variable in one country aggregated in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.aggregation}[[3]], "%H:%M:%OS3"))
  } else if(length(unique(data[ , get(key(data))])) > 1 & length(src.variables) == 1) {
    message('One variable in all ', length(unique(data[ , get(key(data))])), " countries aggregated in ", format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.aggregation}[[3]], "%H:%M:%OS3"))
  } else if(length(unique(data[ , get(key(data))])) == 1 & length(src.variables) > 1) {
    message('All ', length(src.variables), ' variables in one country aggregated in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.aggregation}[[3]], "%H:%M:%OS3"))
  } else if(length(unique(data[ , get(key(data))])) > 1 & length(src.variables) > 1) {
    message('All ', length(src.variables), " variables in all ", length(unique(data[ , get(key(data))])), " countries aggregated in ", format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.aggregation}[[3]], "%H:%M:%OS3"))
  }
  ptm.data.export <- proc.time()
  if(missing(out.file) & missing(data.object)) {
    new.mem.obj.name <- gsub(pattern = "\\.RData$", replacement = "", x = basename(data.file))
    assign(x = new.mem.obj.name, value = data, pos = parent.frame())
  } else if(missing(out.file) & !missing(data.object)) {
    assign(x = used.data, value = data, pos = parent.frame())
  } else if(!missing(out.file)) {
    assign(x = gsub(pattern = "\\.RData$", replacement = "", x = basename(out.file)), value = data)
    data <- NULL
    save(list = gsub(pattern = "\\.RData", replacement = "", x = basename(out.file)), file = out.file, compress = FALSE)
  }
  if(!missing(out.file)) {
    message('\nData file "', basename(out.file), '" exported in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.data.export}[[3]], "%H:%M:%OS3"))
  } else {
    message('\nData object ', gsub(pattern = '\\.RData|"', replacement = '', x = basename(used.data)), ' written to memory in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.data.export}[[3]], "%H:%M:%OS3"))
  }
  message("")
}
