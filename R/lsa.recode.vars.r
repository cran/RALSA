#' @title Recode variables in large-scale assessments' data sets
#'
#' @description Utility function to recode variables in objects or data sets containing objects of class \code{lsa.data}, taking care of user-defined missing values, if specified.
#'
#' @param data.file       Full path to the \code{.RData} file containing \code{lsa.data} object.
#'                        Either this or \code{data.object} shall be specified, but not both.
#'                        See details.
#' @param data.object     The object in the memory containing \code{lsa.data} object. Either this or
#'                        \code{data.file} shall be specified, but not both. See details.
#' @param src.variables   Names of the source variables with the same class whose values shall be
#'                        recoded. See details.
#' @param new.variables   Optional, vector of variable names to be created with the recoded values
#'                        with the same length as \code{src.variables}. If missing, the \code{src.variables}
#'                        will be overwritten.
#' @param old.new         String with the recoding instructions matching the length of the factor
#'                        levels (or unique values in case of numeric or character variables) in
#'                        the variables. See details and examples.
#' @param new.labels      The new labels if the \code{src.variables} variables are of class \code{factor}
#'                        or labels to be assigned to the recoded values (i.e. turning variables of class
#'                        \code{numeric} or \code{character} into factors) with the same length as the
#'                        new desired values. See details.
#' @param missings.attr   Optional, list of character vectors to assign user-defined missing values
#'                        for each recoded variable. See details and examples.
#' @param variable.labels Optional, string vector with the new variable labels to be assigned.
#'                        See details.
#' @param out.file        Full path to the \code{.RData} file to be written. If missing, the object
#'                        will be written to memory. See examples.
#'
#' @details
#' Before recoding variables of interest, it is worth running the \code{lsa.vars.dict} to check their properties.
#'
#' Either \code{data.file} or \code{data.object} shall be provided as source of data. If both of them are provided, the function will stop with an error message.
#'
#' The variable names passed to \code{src.variables} must be with the same class and structure, i.e. same number of levels and same labels in case of \code{factor} variables, or the same unique values in case of \code{numeric} or \code{character} variables. If the classes differ, the function will stop with an error. If the unique values and/or labels differ, the function would execute the recodings, but will drop a warning.
#'
#' The \code{new.variables} is optional. If provided, the recoded values will be saved under the provided new variable names and the \code{src.variables} will remain unchanged. If missing, the variables passed in \code{src.variables} will be overwritten. Note that the number of names passed to \code{src.variables} and \code{new.variables} must be the same.
#'
#' The \code{old.new} (old values to new values) is the recoding scheme to be evaluated and executed provided as a characters string in the form of \code{"1=1;2=1;3=2;4=3"}. In this example it means "recode 1 into 1, 2 into one, 3 into 2, and 4 into 3". Note that all available values have to be included in the recoding statement, even if they are not to be changed. In this example, if we omit recoding 1 into 1, 1 will be set to NA during the recoding. This recoding definition works with factor and numeric variables. For character variables the individual values have to be defined in full, e.g. \code{"'No time'='30 minutes or less';'30 minutes or less'='30 minutes or less';'More than 30 minutes'='More than 30 minutes';'Omitted or invalid'='Omitted or invalid'"} because these cannot be reliably referred to by position (as for factors) or actual number (as for numeric).
#'
#' The \code{new.labels} assigns new labels to factor variables. Their length must be the same as for the newly recoded values. If the variables passed to \code{src.variabes} are character or numeric, and \code{new.labels} are provided, the recoded variables will be converted to factors. If, on the other hand, the \code{src.variables} are factors and no \code{new.labels} are provided, the variables will be converted to numeric.
#'
#' Note that the \code{lsa.convert.data} has two options: keep the user-defined missing values (\code{missing.to.NA = FALSE}) and set the user-defined missing values to NA (\code{missing.to.NA = TRUE}). The former option will provide an attribute with user-defined missing values attached to each variable they have been defined for, the latter will not (i.e. will assign all user-defined missing values to NA). In case variables from data converted with the former option are recoded, user-defined missing values have to be supplied to \code{missings.attr}, otherwise (if all available values are recoded) the user-defined missing values will appear as valid codes. Not recoding the user-defined missing codes available in the data will automatically set them to \code{NA}. In either case, the function will drop a warning. On the other hand, if the data was exported with \code{missing.to.NA = TRUE}, there will be no attributes with user-defined missing codes and omitting \code{missings.attr} will issue no warning. User-defined missing codes can, however, be added in this case too, if necessary. The \code{missings.attr} has to be provided as a list where each component is a vector with the values for the missing codes. See the examples.
#'
#' The \code{variable.labels} argument provides the variable labels to be assigned to the recoded variables. If omitted and \code{new.variables} are provided the newly created variables will have no variable labels. If provided, and \code{new.variables} are not provided, they will be ignored.
#' If full path to \code{.RData} file is provided to \code{out.file}, the data.set will be written to that file. If no, the data will remain in the memory.
#'
#' @return
#' A \code{lsa.data} object in memory (if \code{out.file} is missing) or \code{.RData} file containing \code{lsa.data} object with the recoded values for the specified variables.
#' In addition, the function will print tables for the specified variables before and after recoding them to check if all recodings were done as intended. In addition, it will print warnings if different issues have been encountered.
#'
#' @examples
#' # Recode PIRLS 2016 student variables "ASBG10A" (How much time do you spend using a computer or
#' # tablet to do these activities for your schoolwork on a normal school day? Finding and reading
#' # information) and "ASBG10B" (How much time do you spend using a computer or tablet to do these
#' # activities for your schoolwork on a normal school day? Preparing reports and presentations).
#' # Both variables are factors, the original valid values (1 - "No time",
#' # 2 - "30 minutes or less", 3 - "More than 30 minutes") are recoded to
#' # 1 - "No time or 30 minutes" and 2 - "More than 30 minutes", collapsing the first two.
#' # The missing value "Omitted or invalid" which originally appears as 4th has to be recoded to
#' # 3rd. The "Omitted or invalid" is assigned as user-defined missing value for both variables.
#' # The data is saved on disk as a new data set.
#' \dontrun{
#' lsa.recode.vars(data.file = "C:/temp/test.RData", src.variables = c("ASBG10A", "ASBG10B"),
#' new.variables = c("ASBG10A_rec", "ASBG10B_rec"),
#' variable.labels = c("Recoded ASBG10A", "Recoded ASBG10B"),
#' old.new = "1=1;2=1;3=2;4=3",
#' new.labels = c("No time or 30 minutes", "More than 30 minutes", "Omitted or invalid"),
#' missings.attr = list("Omitted or invalid", "Omitted or invalid"),
#' out.file = "C:/temp/test_new.RData")
#' }
#'
#' # Similar to the above, recode PIRLS 2016 student variables "ASBG10A" and "ASBG10B", this time
#' # leaving the original categories (1 - "No time", 2 - "30 minutes or less",
#' # 3 - "More than 30 minutes") as they are, but changing the user-defined missing values
#' # definition (1 - "No time" becomes user-defined missing).
#' # The recoded data remains in the memory.
#' \dontrun{
#' lsa.recode.vars(data.file = "C:/temp/test.RData", src.variables = c("ASBG10A", "ASBG10B"),
#' new.variables = c("ASBG10A_rec", "ASBG10B_rec"),
#' variable.labels = c("Recoded ASBG10A", "Recoded ASBG10B"), old.new = "1=1;2=2;3=3;4=4",
#' new.labels = c("No time", "30 minutes or less", "More than 30 minutes", "Omitted or invalid"),
#' missings.attr = list(c("No time", "Omitted or invalid"), c("No time", "Omitted or invalid")))
#' }
#'
#' # Similar to the first example, this time overwriting the original variables. The first valid
#' # value (1 - "No time") is set to NA (note that no new value and factor level is provided for
#' # it in "new.labels"), the rest of the values are redefined, so the factor starts from 1,
#' # as it always does in R.
#' \dontrun{
#' lsa.recode.vars(data.file = "C:/temp/test.RData", src.variables = c("ASBG10A", "ASBG10B"),
#' variable.labels = c("Recoded ASBG10A", "Recoded ASBG10B"), old.new = "2=1;3=2;4=3",
#' new.labels = c("30 minutes or less", "More than 30 minutes", "Omitted or invalid"),
#' missings.attr = list("Omitted or invalid"),
#' out.file = "C:/temp/test_new.RData")
#' }
#' # The databases rarely contain character variables and the numeric variables have too many
#' # unique values to be recoded using the function. The following two examples are just for
#' # demonstration purpose on how to recode character and numeric variables.
#'
#' # Convert the "ASBG04" (number of books at home) from ePIRLS 2016 to numeric and recode the
#' # values of the new variable, collapsing the first two and the last two valid values.
#' # The data remains in the memory.
#' \dontrun{
#' load("/tmp/test.RData")
#' test[ , ASBG04NUM := as.numeric(ASBG04)]
#' table(test[ , ASBG04NUM])
#' lsa.recode.vars(data.object = test, src.variables = "ASBG04NUM",
#' old.new = "1=1;2=1;3=2;4=3;5=3;6=4",
#' missings.attr = list("Omitted or invalid" = 4))
#'
#' # Similar to the above, this time converting "ASBG03" to character, collapsing its categories
#' # of frequency of using the test language at home to two ("Always or almost always" and
#' # "Sometimes or never").
#' \dontrun{
#' load("/tmp/test.RData")
#' test[ , ASBG03CHAR := as.character(ASBG03)]
#' table(test[ , ASBG03CHAR])
#' # Add the lines together to be able to run the following
#' lsa.recode.vars(data.object = test, src.variables = "ASBG03CHAR",
#' old.new = "'I always speak <language of test> at home'='Always or almost always';
#' 'I almost always speak <language of test> at home'='Always or almost always';
#' 'I sometimes speak <language of test> and sometimes speak another language at home'=
#' 'Sometimes or never';'I never speak <language of test> at home'='Sometimes or never';
#' 'Omitted or invalid'='Omitted or invalid'",
#' missings.attr = list("Omitted or invalid"))
#' }
#'
#' }
#' @seealso \code{\link{lsa.convert.data}}, \code{\link{lsa.vars.dict}}
#' @export


lsa.recode.vars <- function(data.file, data.object, src.variables, new.variables, old.new, new.labels, missings.attr, variable.labels, out.file) {
  
  tmp.options <- options(scipen = 999, digits = 22)
  on.exit(expr = options(tmp.options), add = TRUE)
  
  if(!missing(new.variables) && length(new.variables) != length(src.variables)) {
    stop('The number of names provided to "src.variables" and "new.variables" differs.  All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  
  warnings.collector <- list()
  
  if(!missing(data.file) == TRUE && !missing(data.object) == TRUE) {
    stop('Either "data.file" or "data.object" has to be provided, but not both. All operations stop here. Check your input.\n\n', call. = FALSE)
  } else if(!missing(data.file)) {
    if(file.exists(data.file) == FALSE) {
      stop('The file specified in the "data.file" argument does not exist. All operations stop here. Check your input.\n\n', call. = FALSE)
    }
    
    ptm.data.import <- proc.time()
    data <- copy(import.data(path = data.file))
    used.data <- deparse(substitute(data.file))
    message('\nData file ', used.data, ' imported in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.data.import}[[3]], "%H:%M:%OS3"))
    
    
  } else if(!missing(data.object)) {
    if(!exists(all.vars(match.call()))) {
      stop('The object specified in the "data.object" argument does not exist. All operations stop here. Check your input.\n\n', call. = FALSE)
    }
    
    data <- copy(data.object)
    
    used.data <- deparse(substitute(data.object))
    message('\nUsing data from object "', used.data, '".')
  }
  
  if(!"lsa.data" %in% class(data)) {
    stop('\nThe data is not of class "lsa.data". All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  
  vars.classes <- unique(sapply(X = data[ , mget(src.variables)], FUN = class))
  
  if(length(vars.classes) > 1) {
    stop('The variables passed to "src.variables" are not of the same class. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  
  vars.unique.values <- sapply(X = data[ , mget(src.variables)], FUN = function(i) {
    length(unique(i))
  })
  
  if(length(unique(vars.unique.values)) > 1) {
    stop('The variables passed to "src.variables" do not have the same number of distinct values, this means they have different properties. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  
  
  if(vars.classes == "factor" & missing(new.labels)) {
    warnings.collector[["no.labels.for.factor"]] <- 'The variables passed to "src.variables" are factors, but no new labels are defined in "new.labels". Thus, the variable is converted to numeric. Check your input.'
  }
  
  ptm.data.recode <- proc.time()
  
  var.labels <- sapply(X = data[ , mget(src.variables)], FUN = function(i) {
    attr(x = i, which = "variable.label")
  })
  
  var.labels <- data.table(V1 = names(var.labels), V2 = var.labels)
  
  tmp.data <- copy(data[ , mget(src.variables)])
  
  if(vars.classes == "factor") {
    vars.levels <- lapply(X = tmp.data, FUN = function(i) {
      levels(i)
    })
  } else if(vars.classes == "numeric" | vars.classes == "character") {
    vars.levels <- lapply(X = tmp.data, FUN = function(i) {
      unique(na.omit(i))
    })
  }
  
  missings.attributes <- lapply(X = tmp.data, attr, which = "missings")
  
  if(vars.classes == "factor") {
    tmp.data[ , (colnames(tmp.data)) := lapply(.SD, as.numeric)]
  }
  
  tmp.colnames <- paste0(colnames(tmp.data) , "_tmp")
  if(vars.classes != "character") {
    tmp.data <- copy(tmp.data[ , (tmp.colnames) := NA_real_])
  } else {
    tmp.data <- copy(tmp.data[ , (tmp.colnames) := NA_character_])
  }
  
  recoding.scheme <- as.list(unlist(str_split(string = old.new, pattern = "[[:space:]]*\\;[[:space:]]*")))
  
  recoding.scheme <- recoding.scheme[recoding.scheme != ""]
  
  recoding.scheme <- lapply(X = recoding.scheme, FUN = function(i) {
    i <- unlist(str_split(string = i, pattern = "[[:space:]]*\\=[[:space:]]*"))
  })
  
  recoding.statements <- lapply(X = recoding.scheme, FUN = function(i) {
    paste0("tmp.data[", src.variables, " == ", i[1], ", ", tmp.colnames, " := ", i[2], "]")
  })
  
  if(vars.classes != "character") {
    values.to.recode <- as.numeric(unique(unlist(recoding.scheme)))
  } else {
    values.to.recode <- unique(unlist(recoding.scheme))
  }
  
  undeclared.values <- unique(unlist(lapply(X = tmp.data[ , mget(src.variables)], FUN = function(i) {
    1:length(unique(na.omit(i)))
  })))
  
  if(vars.classes != "character") {
    undeclared.values <- setdiff(x = undeclared.values, y = values.to.recode)
  } else {
    undeclared.values <- setdiff(x = undeclared.values, y = 1:length(values.to.recode))
  }
  
  
  if(length(undeclared.values) > 0) {
    warnings.collector[["less.recodings.than.avail"]] <- 'The number of values to recode supplied to the "old.new" argument is less than the number of unique values in the variable(s). The values not set for recoding are set to NA. This can have consequences for analysis. Check your input.'
  }
  
  eval(parse(text = unlist(recoding.statements)))
  
  if(!missing(new.labels)) {
    
    tmp.data[ , (tmp.colnames) := lapply(.SD, function(i) {
      
      if(length(unique(na.omit(i))) != length(new.labels)) {
        stop('The number of new labels in "new.labels" has a different length than the number of distinct values in the recoded variables. All operations stop here. Check the following:\n1. Your input.\n2. If all levels in the source variables have valid values for all levels.\n\n', call. = FALSE)
      } else {
        i <- factor(x = i, labels = new.labels)
      }
    }), .SDcols = tmp.colnames]
    
  }
  
  if(all(is.null(unlist(missings.attributes))) == FALSE & missing(missings.attr)) {
    warnings.collector[["no.missings.attribute"]] <- 'The variables passed to "src.variables" have a "missings" attribute, but no missing values\' labels were passed to the "missings.attr". This can have consequences for analysis. Check your input.'
  } else if(!missing(missings.attr)) {
    new.missings.attributes <- missings.attr
  }
  
  src.vars.freqs <- lapply(X = data[ , mget(src.variables)], FUN = function(i) {
    data.table(table(i, useNA = "always"))
  })
  
  names.src.vars.freqs <- as.list(names(src.vars.freqs))
  
  src.vars.freqs <- Map(f = function(input1, input2) {
    setnames(x = input1, c(paste0("Source_", input2), "N"))
  }, input1 = src.vars.freqs, input2 = names.src.vars.freqs)
  
  tmp.data[ , (src.variables) := NULL]
  
  if(missing(new.variables)) {
    setnames(x = tmp.data, gsub(pattern = "_tmp$", replacement = "", x = colnames(tmp.data)))
    data[ , (src.variables) := tmp.data]
  } else {
    setnames(x = tmp.data, new.variables)
    data[ , (new.variables) := tmp.data]
  }
  
  message('\nAll recodings done in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.data.recode}[[3]], "%H:%M:%OS3"), "\n")
  
  new.vars.freqs <- lapply(X = tmp.data, FUN = function(i) {
    data.table(table(i, useNA = "always"))
  })
  
  tmp.data <- NULL
  
  names.new.vars.freqs <- as.list(names(new.vars.freqs))
  
  new.vars.freqs <- Map(f = function(input1, input2) {
    setnames(x = input1, c(paste0("New_", input2), "N"))
  }, input1 = new.vars.freqs, input2 = names.new.vars.freqs)
  
  src.new.vars.freqs <- Map(f = function(input1, input2) {
    nrows.max <- max(nrow(input1), nrow(input2))
    
    if(nrow(input1) < nrows.max) {
      input1 <- rbindlist(l = list(input1, rbindlist(rep(list(setDT(as.list(c("NaN", NaN)))), times = nrows.max - nrow(input1)))))
    }
    
    if(nrow(input2) < nrows.max) {
      input2 <- rbindlist(l = list(input2, rbindlist(rep(list(setDT(as.list(c("NaN", NaN)))), times = nrows.max - nrow(input2)))), use.names = FALSE)
    }
    
    divider <- data.table("|||" = rep(x = "|||", times = nrows.max))
    cbind(input1, divider, input2)
  }, input1 = src.vars.freqs, input2 = new.vars.freqs)
  
  if(length(src.new.vars.freqs) == 1) {
    message("The following table contains the variable's frequencies before and after recoding it. Please check them carefully.\n")
  } else {
    message("The following tables contain the variables' frequencies before and after recoding them. Please check them carefully.\n")
  }
  
  lapply(X = src.new.vars.freqs, FUN = function(i) {
    message(paste0(paste(rep("+", times = unlist(options("width")) - 10), collapse = ""), "\n"))
    message(paste(capture.output(i), collapse = "\n"))
    message(paste0(paste(rep("+", times = unlist(options("width")) - 10), collapse = ""), "\n"))
  })
  
  message("")
  
  if(missing(variable.labels) & missing(new.variables)) {
    var.labels <- paste0("setattr(x = data[['", var.labels[ , V1], "']], name = 'variable.label', value = '", var.labels[ , V2], "')")
    eval(parse(text = var.labels))
  } else if(!missing(variable.labels) & missing(new.variables)) {
    var.labels <- paste0("setattr(x = data[['", var.labels[ , V1], "']], name = 'variable.label', value = '", var.labels[ , V2], "')")
    eval(parse(text = var.labels))
  } else if(!missing(variable.labels) & !missing(new.variables)) {
    var.labels <- paste0("setattr(x = data[['", new.variables, "']], name = 'variable.label', value = '", variable.labels, "')")
    eval(parse(text = var.labels))
  }
  
  if(exists("new.missings.attributes")) {
    
    set.new.missings <- function(input1, input2) {
      if(unique(sapply(data[ , mget(input1)], class)) != "numeric") {
        paste0("setattr(x = data[['", input1, "']], name = 'missings', value = c('", paste(input2, collapse = "', '"), "'))")
      } else {
        paste0("setattr(x = data[['", input1, "']], name = 'missings', value = c('", paste(input2, collapse = "', '"), "'))")
      }
    }
    
    if(!missing(new.variables)) {
      missings.attr.statements <- Map(f = set.new.missings, input1 = new.variables, input2 = new.missings.attributes)
    } else {
      missings.attr.statements <- Map(f = set.new.missings, input1 = src.variables, input2 = new.missings.attributes)
    }
    
    eval(parse(text = unlist(missings.attr.statements)))
    
    check.missings.in.values <- function(input1, input2) {
      if(class(data[ , get(input1)]) == "factor"){
        any(input2 %in% levels(data[ , get(input1)]) == FALSE)
      } else {
        any(input2 %in% unique(data[ , get(input1)]) == FALSE)
      }
    }
    
    if(!missing(new.variables)) {
      missings.not.in.values <- Map(f = check.missings.in.values, input1 = new.variables, input2 = new.missings.attributes)
    } else {
      missings.not.in.values <- Map(f = check.missings.in.values, input1 = src.variables, input2 = new.missings.attributes)
    }
    
    names.missings.not.in.values <- names(Filter(isTRUE, missings.not.in.values))
    
    if(length(names.missings.not.in.values) > 0) {
      warnings.collector[["missings.not.in.values"]] <- paste0('The following variables have been assigned user-defined missings which were not found in the actual data after the recoding took place: ', paste(names.missings.not.in.values, collapse = ", "), '. Check your input.')
    }
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
    message('\nData object ', used.data, ' written to memory in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.data.export}[[3]], "%H:%M:%OS3"))
  }
  
  message("")
  
  if(length(warnings.collector) > 0) {
    
    if(!is.null(warnings.collector[["less.recodings.than.avail"]])) {
      warning(warnings.collector[["less.recodings.than.avail"]], call. = FALSE)
    }
    
    if(!is.null(warnings.collector[["no.missings.attribute"]])) {
      warning(warnings.collector[["no.missings.attribute"]], call. = FALSE)
    }
    
    if(!is.null(warnings.collector[["no.labels.for.factor"]])) {
      warning(warnings.collector[["no.labels.for.factor"]], call. = FALSE)
    }
    
    if(!is.null(warnings.collector[["missings.not.in.values"]])) {
      warning(warnings.collector[["missings.not.in.values"]], call. = FALSE)
    }
    
  }
}
