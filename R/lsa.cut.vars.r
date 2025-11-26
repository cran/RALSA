#' @title Cut continuous variables into discrete categorical
#'
#' @description \code{lsa.cut.vars} cuts continuous variables into discrete ones using user-defined ranges. For example, some continuous scales in large-scale assessments and surveys can be converted into two, three or more categories depending on the cut-points provided by the user.
#'
#' @param data.file       The file containing \code{lsa.data} object. Either this or
#'                        \code{data.object}
#'                        shall be specified, but not both. See details.
#' @param data.object     The object in the memory containing \code{lsa.data} object. Either this
#'                        or \code{data.file} shall be specified, but not both. See details.
#' @param src.variables   Names of the variables to cut into categories. Accepts only continuous
#'                        variables. No PV variables are accepted. See details.
#' @param new.variables   The names of the new, cut variables to append to the dataset.
#'                        See details.
#' @param new.var.labels  Optional, vector of strings to add as variable labels for the
#'                        \code{new.variables}. See details.
#' @param cut.points      Vector of numeric values to cut the \code{src.variables} between.
#'                        See details.
#' @param value.labels    Optional, character vector of values to assign to the newly formed
#'                        categorical discrete values in the \code{new.variables}. See details.
#' @param out.file        Full path to the \code{.RData} file to be written. If missing, the
#'                        original object will be overwritten in the memory. See examples.
#'
#' @details
#' The function cuts continuous variables in large-scale assessments' data in to variables with discrete values. The resulting variables can be numeric or categorical (i.e. factors) depending on if value labels for the new values are provided.
#'
#' Either \code{data.file} or \code{data.object} shall be provided as source of data. If both of them are provided, the function will stop with an error message.
#'
#' The \code{src.variables} specifies the variables that shall be cut. Only continuous variables are accepted. Multiple \code{src.variables} can be passed. These will be split at the same cut points (see below). PVs are not accepted.
#'
#' The \code{new.variables} argument is optional and specifies the names of the new discrete variables from the \code{src.variables}. The sequence of the \code{new.variables} names is the same as the \code{src.variables}. If the \code{new.variables} argument is omitted, the function will create the names automatically, appending \code{CUT} at the end of the \code{src.variables} and store the discrete variable data under these names. If provided, the number of \code{new.variables} must be the same as the number of \code{src.variables}.
#'
#' The \code{new.var.labels} is optional. Regardless whether \code{new.variables} are provided, if \code{new.var.labels} are provided, they will be assigned to the \code{new.variables} generated from the discretization. If neither \code{new.variables} not \code{new.var.labels} are provided, the function will automatically generate \code{new.variables} (see above) and copy the variable labels from \code{src.variables} to the newly generated variables, appending \code{Cut} at the beginning. The argument takes a vector with the same number of elements as the number of variable names in \code{src.variables}.
#'
#' \code{cut.points} is a mandatory argument. It specifies the ranges (from-to) in the original variables to be cut into discrete categories. There can be multiple \code{cut.points}, the new values will be the ranges between them. For example, if the \code{3.29309}, \code{7.97028}, \code{9.98618}, and \code{10.99411} cut points are passed, there will be five categories in the resulting discrete variables, as follow:
#' \enumerate{
#'    \item 1 - from lowest up to 3.29309;
#'    \item 2 - from above 3.29309 up to 7.97028;
#'    \item 3 - from above 7.97028 up to 9.98618;
#'    \item 4 - from above 9.98618 up to 10.99411; and
#'    \item 5 - from above 10.99411 to the highest value.
#' }
#'
#' The \code{cut.points} must be within the range of the \code{src.variables}. Otherwise the function will stop with an error.
#'
#' The \code{value.labels} is optional. If omitted, the values in the new discrete variables will be numeric (integers). If the data was exported with \code{missing.to.NA = FALSE} (i.e. user-defined missings are kept) the missing values will remain as they are. If the \code{value.labels} are provided, the new values will be converted to factor levels. If the data was exported with \code{missing.to.NA = FALSE} the names of missing values will be assigned to factor levels too. Either way, the missing values will remain as missing values and handled properly by the analysis functions. If \code{missing.to.NA = TRUE} (i.e. setting the user-defined missing values to \code{NA}), the \code{NA} values will remain as \code{NA} in the resulting discrete \code{new.variables}.
#'
#' If full path to \code{.RData} file is provided to \code{out.file}, the data.set will be written to that file. If no, the complemented data will remain in the memory.
#'
#' @return
#' A \code{lsa.data} object in memory (if \code{out.file} is missing) or \code{.RData} file containing \code{lsa.data} object with the new discrete variables.
#'
#' @examples
#'
#' # Produce new discrete variables from the PIRLS 2021 Students Like Reading and the
#' # Home Resources for Learning scales. The values for the new variables are
#' # numeric. Save the dataset into a file, overwriting it. The names for the new
#' # variables are automatically generated.
#' \dontrun{
#' lsa.cut.vars(data.file = "C:/Data/PIRLS_2021_Student_Miss_to_NA.RData",
#' src.variables = c("ASBGSLR", "ASBGHRL"),
#' cut.points = c(4.1, 7.9, 9.9, 10.7),
#' out.file = "C:/Data/PIRLS_2021_Student_Miss_to_NA.RData")
#' }
#'
#' # Same as the above, but assign custom variable names, value labels for the new categorical
#' # variables, custom variable labels. Write the data to the memory instead of saving it on
#' # the disk.
#' \dontrun{
#' lsa.cut.vars(data.file = "C:/Data/PIRLS_2021_Student_Miss_to_NA.RData",
#' src.variables = c("ASBGSLR", "ASBGHRL"),
#' new.variables = c("ASBGSLRREC", "ASBGHRLREC"),
#' new.var.labels = c("Categorical like reading", "Categorical learning resources"),
#' cut.points = c(4.1, 7.9, 9.9, 10.7),
#' value.labels = c("Very low", "Low", "Medium", "High", "Very high"),
#' out.file = "C:/Data/PIRLS_2021_Student_Miss_to_NA.RData")
#' }
#'
#' @seealso \code{\link{lsa.convert.data}}, \code{\link{lsa.recode.vars}}
#' @export
lsa.cut.vars <- function(data.file, data.object, src.variables, new.variables, new.var.labels, cut.points, value.labels, out.file) {
  tmp.options <- options(scipen = 999, digits = 22)
  on.exit(expr = options(tmp.options), add = TRUE)
  if(missing(new.variables)) {
    new.variables <- paste0(src.variables, "DCZJ")
  } else {
    new.variables <- new.variables
  }
  if(!missing(new.variables) & missing(new.var.labels)) {
    new.var.labels <- NULL
  } else if(!missing(new.variables) & !missing(new.var.labels)) {
    new.var.labels <- new.var.labels
  } else if(missing(new.variables) & missing(new.var.labels)) {
    new.var.labels <- NULL
  }
  if(missing(value.labels)) {
    value.labels <- NULL
  } else {
    value.labels <- value.labels
  }
  if(length(new.variables) != length(src.variables)) {
    stop('\nThe number of "new.variables" must be equal to the number of "src.variables". All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(length(grep(pattern = paste(unname(unlist(studies.all.design.variables[["PV.roots"]])), collapse = "|"), x = src.variables)) > 0) {
    stop('\nPlausible values variables were passed to "src.variables". Cutting plausible values into categorical is not permitted. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(!missing(new.variables) & !is.null(new.var.labels) & !is.atomic(new.var.labels)) {
    stop('\nThe "new.var.labels" argument accepts only atomic vectors (i.e. no lists or other kinds of objects). All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(!is.null(new.var.labels) & length(new.variables) != length(new.var.labels)) {
    stop('\nThe number of "new.var.labels" must be equal to the number of "new.variables". All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(!is.null(value.labels) & length(value.labels) != (length(cut.points) + 1)) {
    stop('\nThe number of new categories and their labels are not equal. All operations stop here. Check your input.\n\n', call. = FALSE)
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
  if(isFALSE(all(sapply(X = data[ , mget(src.variables)], FUN = is.numeric)))) {
    stop('\nOnly numeric continuous variables can be passed to "src.variables". All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(any(new.variables %in% colnames(data))) {
    stop('\nOne or more variables specified in "new.variables" already exists in the data set. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  cut.points.out.of.range <- lapply(X = data[ , mget(src.variables)], FUN = function(i) {
    min.below <- cut.points[1] < min(i, na.rm = TRUE)
    max.above <- cut.points[length(cut.points)] > max(i, na.rm = TRUE)
    c(min.below, max.above)
  })
  if(any(unlist(cut.points.out.of.range))) {
    stop('\nSome of the "cut.points" are out of range of the variables passed to "src.variables". The minimum and maximum values of the "src.variables" can be checked using "lsa.data.diag". All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  number.of.countries <- length(unique(data[ , get(key(data))]))
  if(number.of.countries == 1) {
    message("\nValid data from one country have been found. Some computations can be rather intensive. Please be patient.\n")
  } else if(number.of.countries > 1) {
    message("\nValid data from ", number.of.countries, " countries have been found. Some computations can be rather intensive. Please be patient.\n")
  }
  vars.list <- get.analysis.and.design.vars(data)
  ptm.cutting <- proc.time()
  data[ , (new.variables) := lapply(.SD, FUN = function(i) {
    tmp.missings.attr <- attr(x = i, which = "missings")
    cut.ranges <- c(-Inf, cut.points, Inf)
    if(is.null(value.labels)) {
      i <- ifelse(test = !i %in% tmp.missings.attr, yes = as.integer(cut(x = i, breaks = cut.ranges, dig.lab = 0, right = FALSE)), no = i)
    } else {
      i <- ifelse(test = !i %in% tmp.missings.attr, yes = cut(x = i, breaks = cut.ranges, right = FALSE), no = i)
      i <- factor(x = i, labels = c(value.labels, names(tmp.missings.attr)))
      data[ , setattr(x = i, name = "missings", value = names(tmp.missings.attr))]
      return(i)
    }
    if(!is.null(tmp.missings.attr)) {
      setattr(x = i, name = "missings", value = tmp.missings.attr)
    } else {
      i
    }
  }), .SDcols = src.variables]
  if(length(grep(pattern = "DCZJ$", x = colnames(data))) > 0) {
    orig.vars <- TRUE
  } else {
    orig.vars <- FALSE
  }
  if(isTRUE(orig.vars) & is.null(new.var.labels)) {
    src.labels <- lapply(X = data[ , mget(src.variables)], FUN = function(i) {
      paste("Cut", attr(x = i, which = "variable.label"))
    })
    names(src.labels) <- new.variables
    lapply(X = new.variables, FUN = function(i) {
      data[ , setattr(x = eval(parse(text = i)), name = "variable.label", value = src.labels[[i]])]
    })
    setnames(x = data, old = new.variables, new = gsub(pattern = "DCZJ$", replacement = "CUT", x = new.variables))
  } else if(isTRUE(orig.vars) & !is.null(new.var.labels)) {
    new.var.labels <- as.list(new.var.labels)
    names(new.var.labels) <- new.variables
    lapply(X = new.variables, FUN = function(i) {
      data[ , setattr(x = eval(parse(text = i)), name = "variable.label", value = new.var.labels[[i]])]
    })
    setnames(x = data, old = new.variables, new = gsub(pattern = "DCZJ$", replacement = "CUT", x = new.variables))
  } else if(isFALSE(orig.vars) & !is.null(new.var.labels)) {
    new.var.labels <- as.list(new.var.labels)
    names(new.var.labels) <- new.variables
    lapply(X = new.variables, FUN = function(i) {
      data[ , setattr(x = eval(parse(text = i)), name = "variable.label", value = new.var.labels[[i]])]
    })
  }
  if(length(unique(data[ , get(key(data))])) == 1 & length(src.variables) == 1) {
    message('One variable in one country cut into categories in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.cutting}[[3]], "%H:%M:%OS3"))
  } else if(length(unique(data[ , get(key(data))])) > 1 & length(src.variables) == 1) {
    message('One variable in all ', length(unique(data[ , get(key(data))])), " countries cut into categories in ", format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.cutting}[[3]], "%H:%M:%OS3"))
  } else if(length(unique(data[ , get(key(data))])) == 1 & length(src.variables) > 1) {
    message('All ', length(src.variables), ' variables in one country cut into categories in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.cutting}[[3]], "%H:%M:%OS3"))
  } else if(length(unique(data[ , get(key(data))])) > 1 & length(src.variables) > 1) {
    message('All ', length(src.variables), " variables in all ", length(unique(data[ , get(key(data))])), " countries cut into categories in ", format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.cutting}[[3]], "%H:%M:%OS3"))
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
