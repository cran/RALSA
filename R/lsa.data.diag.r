#' @title Produce data diagnostic tables
#'
#' @description \code{lsa.data.diag} is a utility function which produces diagnostic tables for variables in an \code{lsa.data} object available in the memory or saved in an \code{.RData} file. The function can be used with regular \code{data.frame} or \code{data.table}, i.e. it is applicable not only to large-scale assessment data.
#'
#' @param data.file       The file containing \code{lsa.data} object. Either this or
#'                        \code{data.object}
#'                        shall be specified, but not both. See details.
#' @param data.object     The object in the memory containing \code{lsa.data} object. Either this
#'                        or \code{data.file} shall be specified, but not both. See details.
#' @param split.vars      Variable(s) to split the results by. If no split variables are
#'                        provided, the results will be computed on country level.
#'                        (if weights are used) or samples (if no weights are used). See details.
#' @param include.missing Shall the \code{NA} and user-defined missing values (if available) be
#'                        included as splitting categories for the variables in \code{split.vars}?
#'                        The default is \code{FALSE}. See details.
#' @param variables       Names of the variables to compute statistics for. If the variables are
#'                        factors or character, frequencies will be computed, and if they are
#'                        numeric, descriptives will be computed, unless \code{cont.freq = TRUE}.
#'                        See details.
#' @param cont.freq       Shall the values of the numeric categories be treated as categorical
#'                        to compute frequencies for? See details.
#' @param weight.var      The name of the variable containing the weights, if weighted statistics
#'                        are needed. If no name of a weight variable is provided, the function
#'                        will automatically select the default weight variable for the provided
#'                        \code{lsa.data}, depending on the respondent type. \code{"none"} is
#'                        for unweighted statistics. See details.
#' @param output.file     Full path to the output file including the file name. If omitted, a file
#'                        with a default file name "Analysis.xlsx" will be written to the working
#'                        directory (\code{getwd()}).
#' @param open.output     Logical, shall the output be open after it has been written? The default
#'                        (\code{TRUE}) opens the output in the default spreadsheet program
#'                        installed on the computer.
#'                        
#' @param ...             Further arguments.
#'
#' @details
#' The function produces data diagnostic tables for variables in an \code{lsa.data} set by the categories of splitting variables. The function is also applicable to data sets which are not of class \code{lsa.data}, a regular \code{data.frame} or a \code{data.table} are accepted as well. If the data is of class \code{lsa.data} and no \code{split.vars} variables are provided, the results will be automatically split and computed by country. The country ID variable will be added automatically, there is no need to specify it explicitly in \code{split.vars}. If the data is not of class \code{lsa.data} and no \code{split.vars} variables are provided, the results will be computed without any split.
#' 
#' Either \code{data.file} or \code{data.object} shall be provided as source of data. If both of them are provided, the function will stop with an error message.
#'
#' If variables are provided for the \code{split.vars} argument and \code{include.missing = TRUE}, the function will automatically add the \code{NA} and user-defined missing values from the \code{missings} attribute (if available) of the \code{split.vars} variables to the categories to split by and will compute statistics for the provided \code{variables} for these as well. See the documentation on \code{\link{lsa.convert.data}} for more details on the conversion of data with and without user-defined missing values.
#'
#' If no variable names are provided to \code{variables} all variables available in the data set will be added automatically, except the weighting and splitting variables, and statistics for all of them will be computed.
#'
#' If the variables provided to the \code{variables} argument are factor or character, the function will compute frequencies, percentages, valid percentages, and cumulative percentages. If the variables are numeric, the computed statistics will include the total number of cases, range, minimum, maximum, mean, variance, and standard deviation. If \code{cont.freq = TRUE}, then the numeric variables will be treated as factors.
#' 
#' If the data set is of class \code{lsa.data} and no weight variable is provided, the computed statistics will be automatically weighted by the default weight for the respondents' data in the object. If the name of a weight variable is provided, the statistics will be weighted by it. If \code{weight.var = "none"}, the computed statistics will be unweighted. If the data is not of class \code{lsa.data} and no \code{weight.var} is provided, the computed statistics will be unweighted. If a weight variable is provided, the computed statistics will be weighted by it.
#'
#' @return
#' A MS Excel (\code{.xlsx}) file (which can be opened in any spreadsheet program), as specified with the full path in the \code{output.file}. If the argument is missing, an Excel file with the generic file name "Analysis.xlsx" will be saved in the working directory (\code{getwd()}). The first sheet in the workbook is an \code{Index} sheet. All other sheets contain the computed statistics for the variables, one sheet per variable. The \code{Index} sheet contains columns with the names of the variables for which statistics are computed and their labels, if available. The names are clickable links, if clicked, they switch to the corresponding sheet with statistics for the corresponding variable. If the data is of class \code{lsa.data}, the \code{Index} sheet also contains information with the study name, cycle, respondent type and used weight. If the data is not of class \code{lsa.data}, the \code{Index} sheet contains information only which weight was used. Each sheet with statistics for a variable contains a clickable link to go back to the \code{Index} sheet, the variable name and label (if any), and the table with statistics for that variable.
#' 
#' @note
#' This function is intended only as utility function for diagnostic purposes, to inspect the variables prior to performing an actual analysis. It is **not** intended for actual analysis of large-scale assessments' data. Reporting statistics from it can and will lead to biased and erroneous conclusions.
#'
#' @examples
#' # Merge PIRLS 2016 school principal data for all countries
#' \dontrun{
#' lsa.merge.data(inp.folder = "C:/Data", file.types = list(acg = NULL),
#' out.file = "C:/Merged/Merged.RData")
#' }
#'
#' # Produce diagnostic tables for some factor (categorical) and numeric (continuous) variables
#' # by country
#' \dontrun{
#' lsa.data.diag(data.file = "C:/Merged/Merged.RData",
#' variables = c("ACBG05A", "ACBG04", "ACBGELS", "ACBGRRS"),
#' output.file = "C:/temp/test.xlsx", open.output = TRUE)
#' }
#'
#' # Repeat the above, splitting the results by country and percentage of students at school
#' # coming from economically affluent homes ("ACBG03B")
#' \dontrun{
#' lsa.data.diag(data.file = "C:/Merged/Merged.RData",
#' split.vars = "ACBG03B", variables = c("ACBG05A", "ACBG04", "ACBGELS", "ACBGRRS"),
#' output.file = "C:/temp/test.xlsx", open.output = TRUE)
#' }
#'
#' # Repeat the above, this time treating the numeric variables ("ACBGELS" and "ACBGRRS")
#' # as categorical
#' \dontrun{
#' lsa.data.diag(data.file = "C:/Merged/Merged.RData",
#' split.vars = "ACBG03B, include.missing = TRUE,
#' variables = c("ACBG05A", "ACBG04", "ACBGELS", "ACBGRRS"),
#' output.file = "C:/temp/test.xlsx", open.output = TRUE)
#' }
#' 
#' # Produce diag for all variables in the data set by country and percentage of students
#' # coming from economically affluent homes ("ASBG03B")
#' \dontrun{
#' lsa.data.diag(data.file = "C:/Merged/Merged.RData",
#' split.vars = "ACBG03B, output.file = "C:/temp/test.xlsx",
#' open.output = TRUE)
#' }
#'
#' @seealso \code{\link{lsa.convert.data}}
#' @export


lsa.data.diag <- function(data.file, data.object, split.vars, variables, weight.var, cont.freq = FALSE, include.missing = FALSE, output.file, open.output = TRUE, ...) {
  
  tmp.options <- options(scipen = 999, digits = 22)
  on.exit(expr = options(tmp.options), add = TRUE)
  
  convert.object <- function(obj) {
    if("variable.labels" %in% names(attributes(x = obj))) {
      var.labels <- as.list(attr(x = obj, which = "variable.labels"))
      names.var.labels <- names(var.labels)[names(var.labels) %in% colnames(obj)]
      var.labels <- var.labels[names.var.labels]
      vars.with.missing.labels <- setdiff(colnames(obj), names(var.labels))
      if(length(vars.with.missing.labels) > 0) {
        tmp.var.labels <- as.list(rep(x = NA, times = length(vars.with.missing.labels)))
        names(tmp.var.labels) <- vars.with.missing.labels
        var.labels <- c(var.labels, tmp.var.labels)
      }
      invisible(setDT(x = mapply(FUN = setattr, x = obj, name = "variable.label", value = var.labels, SIMPLIFY = FALSE)))
    } else {
      setDT(obj)
    }
  }
  
  if(!missing(data.file) & !missing(data.object)) {
    
    stop('Either "data.file" or "data.object" has to be provided, but not both. All operations stop here. Check your input.\n\n', call. = FALSE)
  } else if(missing(data.file) & missing(data.object)) {
    stop('Neither "data.file" nor "data.object" is provided. All operations stop here. Check your input.\n\n', call. = FALSE)
  } else if(!missing(data.file) & missing(data.object)) {
    
    if(file.exists(data.file) == FALSE) {
      stop('The file specified in the "data.file" argument does not exist. All operations stop here. Check your input.\n\n', call. = FALSE)
    }
    
    ptm.data.import <- proc.time()
    data.object <- copy(import.data(path = data.file))
    
    message('\nData file ', deparse(substitute(data.file)), ' imported in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.data.import}[[3]], "%H:%M:%OS3"))
    
    if(!"lsa.data" %in% class(data.object)) {
      data.object <- convert.object(obj = data.object)
    }
    
  } else if(!missing(data.object) & missing(data.file)) {
    if(length(all.vars(match.call())) == 0) {
      stop('The object specified in the "data.object" argument is quoted, is this an object or a path to a file? All operations stop here. Check your input.\n\n', call. = FALSE)
    }
    if(!exists(all.vars(match.call()))) {
      stop('The object specified in the "data.object" argument does not exist. All operations stop here. Check your input.\n\n', call. = FALSE)
    }
    
    message('\nUsing data from object "', deparse(substitute(data.object)), '".')
    
    if(!"lsa.data" %in% class(data.object)) {
      data.object <- convert.object(obj = data.object)
    }
    
  }
  
  passed.vars <- as.list(sys.call())[!names(as.list(sys.call())) %in% c("", "data.file", "data.object", "output.file", "open.output", "cont.freq", "include.missing", "no.CNT.split")]
  
  if(length(passed.vars) > 0) {
    passed.vars <- unlist(lapply(passed.vars, function(i) {
      as.character(i)[as.character(i) != "c"]
    }))
    
    if(any(!passed.vars[passed.vars != "none"] %in% colnames(data.object))) {
      stop('One or more variable names passed to the function arguments do not exist in the data. All operations stop here. Check your input.\n\n', call. = FALSE)
    }
  }
  
  if("lsa.data" %in% class(data.object)) {
    if(missing(weight.var)) {
      weight.var <- get.analysis.and.design.vars(data.object)[["weight.var"]]
    } else if(!missing(weight.var) && weight.var %in% colnames(data.object)) {
      weight.var <- weight.var
    } else if(!missing(weight.var) && weight.var == "none") {
      data.object[ , TMPWGT := rep(x = 1, times = nrow(data.object))]
      weight.var <- "TMPWGT"
    }
  } else {
    if(missing(weight.var) || weight.var == "none") {
      data.object[ , TMPWGT := rep(x = 1, times = nrow(data.object))]
      weight.var <- "TMPWGT"
    } else if(!missing(weight.var) && weight.var %in% colnames(data.object)) {
      weight.var <- weight.var
    }
  }
  
  tryCatch({
    
    further.args <- list(...)
    
    if(!missing(split.vars) && class(data.object) == "lsa.data" && "no.CNT.split" %in% names(further.args) && further.args[["no.CNT.split"]] != TRUE) {
      split.vars <- unique(c(key(data.object), split.vars))
    } else if(!missing(split.vars) && class(data.object) == "lsa.data" && !"no.CNT.split" %in% names(further.args)) {
      split.vars <- unique(c(key(data.object), split.vars))
    } else if(missing(split.vars) && class(data.object) == "lsa.data" && "no.CNT.split" %in% names(further.args) && further.args[["no.CNT.split"]] != TRUE) {
      split.vars <- unique(key(data.object))
    } else if(missing(split.vars) && class(data.object) == "lsa.data" && !"no.CNT.split" %in% names(further.args)) {
      split.vars <- unique(key(data.object))
    } else if(!missing(split.vars) && class(data.object) == "lsa.data" && "no.CNT.split" %in% names(further.args) && further.args[["no.CNT.split"]] == TRUE) {
      split.vars <- split.vars
    }
    
    if(missing(split.vars)) {
      if(missing(variables)) {
        variables <- colnames(data.object)[!colnames(data.object) == weight.var]
      }
      data.object <- copy(data.object[ , c(variables, weight.var), with = FALSE])
    } else {
      if(missing(variables)) {
        variables <- colnames(data.object)[!colnames(data.object) %in% c(split.vars, weight.var)]
      }
      data.object <- copy(data.object[ , c(split.vars, variables, weight.var), with = FALSE])
      setkeyv(x = data.object, split.vars)
    }
    
    message("\nTables for a total of ", length(variables), " variables will be produced.")
    
    if(!missing(split.vars) && length(split.vars) > 1 && class(data.object) == "lsa.data" && "no.CNT.split" %in% names(further.args) && further.args[["no.CNT.split"]] != TRUE) {
      message("The tables for each variable will be split by country ID and ", length(split.vars) - 1, " other variables.\n")
    } else if(!missing(split.vars) && length(split.vars) > 1 && class(data.object) == "lsa.data" && !"no.CNT.split" %in% names(further.args)) {
      message("The tables for each variable will be split by country ID and ", length(split.vars) - 1, " other variables.\n")
    } else if(!missing(split.vars) && length(split.vars[!key(data.object) %in% split.vars]) == 0 && class(data.object) == "lsa.data" && !"no.CNT.split" %in% names(further.args)) {
      message("The tables for each variable will be split by country ID.\n")
    } else if(!missing(split.vars) && length(split.vars[!key(data.object) %in% split.vars]) == 0 && class(data.object) == "lsa.data" && "no.CNT.split" %in% names(further.args) && further.args[["no.CNT.split"]] != TRUE) {
      message("The tables for each variable will be split by country ID.\n")
    } else if(missing(split.vars) && class(data.object) == "lsa.data" && "no.CNT.split" %in% names(further.args) && further.args[["no.CNT.split"]] == TRUE) {
      message("Tables will not be split by any variables.\n")
    } else if(!missing(split.vars) && class(data.object) == "lsa.data" && "no.CNT.split" %in% names(further.args) && further.args[["no.CNT.split"]] == TRUE) {
      message("The tables for each variable will be split by ", length(split.vars), " variables.\n")
    } else if(!missing(split.vars) && class(data.object) != "lsa.data") {
      message("The tables for each variable will be split by ", length(split.vars), " variables.\n")
    } else if(missing(split.vars) && class(data.object) != "lsa.data") {
      message("Tables will not be split by any variables.\n")
    }
    
    message("Some computations can be rather intensive. Please be patient.\n")
    
    ptm.computations <- proc.time()
    
    names.and.labels <- lapply(X = data.object[ , mget(variables)], FUN = function(i) {
      attr(x = i, which = "variable.label")
    })
    
    names.and.labels <- data.table(Names = names(names.and.labels), Labels = names.and.labels)
    
    if(cont.freq == TRUE) {
      if(missing(split.vars)) {
        data.object[ , (variables) := lapply(.SD, function(i) {
          if(any(class(i) != "factor")) {
            i <- factor(i)
          } else {
            i
          }
        })]
      } else {
        data.object[ , c(split.vars, variables) := lapply(.SD, function(i) {
          if(any(class(i) != "factor")) {
            i <- factor(i)
          } else {
            i
          }
        }), .SDcols = c(split.vars, variables)]
      }
    }
    
    if(cont.freq == FALSE) {
      data.object[ , (variables) := lapply(.SD, function(i) {
        if(class(i) == "numeric" && !is.null(attr(x = i, which = "missings"))) {
          i <- ifelse(test = i %in% attr(x = i, which = "missings"), yes = NA, no = i)
        } else {
          i
        }
      }), .SDcols = variables]
    }
    
    study.name <- NULL
    study.cycle <- NULL
    resp.type <- NULL
    if("lsa.data" %in% attr(x = data.object, which = "class")) {
      study.name <- attr(x = data.object, which = "study")
      study.cycle <- attr(x = data.object, which = "cycle")
      resp.type <- attr(x = data.object, which = "file.type")
      
      resp.type <- gsub(pattern = "<br/>", replacement = ", ", x = file.merged.respondents[[resp.type]])
    }
    
    if(!missing(split.vars) & include.missing == TRUE) {
      data.object[ , (split.vars) := lapply(.SD, function(i) {
        if(is.factor(i)) {
          i <- addNA(i)
          tmp.levels <- levels(unlist(i))
          tmp.levels <- c(tmp.levels[!is.na(tmp.levels)], "<NA>")
          i <- factor(i, levels = tmp.levels, labels = tmp.levels)
          i[is.na(i)] <- "<NA>"
          return(i)
        } else {
          i
        }
      }), .SDcols = split.vars]
    } else if(!missing(split.vars) & include.missing == FALSE) {
      data.object[ , (split.vars) := lapply(.SD, function(i) {
        if(is.factor(i) && !is.null(attr(x = i, which = "missings"))) {
          tmp.levels <- levels(i)[!levels(i) %in% attr(x = i, which = "missings")]
          i <- ifelse(test = i %in% attr(x = i, which = "missings"), yes = NA, no = i)
          i <- factor(x = i, labels = tmp.levels)
          return(i)
        } else if(is.numeric(i) || is.character(i) && !is.null(attr(x = i, which = "missings"))) {
          i <- ifelse(test = i %in% attr(x = i, which = "missings"), yes = NA, no = i)
          return(i)
        } else {
          return(i)
        }
      }), .SDcols = split.vars]
      data.object <- na.omit(object = data.object, cols = split.vars)
    }
    
    analysis.vars.missings.attr <- lapply(X = data.object[ , mget(variables)], FUN = function(i) {
      attr(x = i, which = "missings")
    })
    
    if(missing(split.vars)) {
      
      data.object[ , (variables) := lapply(.SD, function(i) {
        if(is.factor(i)) {
          droplevels(i)
        } else {
          i
        }
      }), .SDcols = variables]
      
    } else {
      
      data.object[ , c(variables, split.vars) := lapply(.SD, function(i) {
        if(is.factor(i)) {
          droplevels(i)
        } else {
          i
        }
      }), .SDcols = c(variables, split.vars)]
      
    }
    
    if(!missing(split.vars)) {
      data.object <- lapply(X = data.object[ , variables, with = FALSE], FUN = function(i) {
        i <- data.table(data.object[ , c(split.vars, weight.var), with = FALSE], i)
      })
      data.object <- lapply(X = data.object, FUN = function(i) {
        split(x = i, by = split.vars, drop = TRUE)
      })
    }
    
    if(missing(split.vars)) {
      desc.tables <- lapply(X = data.object[ , mget(colnames(data.object)[colnames(data.object) != weight.var])], FUN = function(i) {
        x <- cbind(as.data.table(i), data.object[ , get(weight.var)])
        if(is.factor(i) | is.character(i)) {
          x[ , N := .(N = sum(V2)), by = i]
          x[ , V2 := NULL]
          return(unique(x))
        } else if(is.numeric(i)) {
          N <- x[!is.na(i) , sum(V2)]
          Minimum <- min(x[ , wtd.table(x = i, weights = V2)][ , x])
          Maximum <- max(x[ , wtd.table(x = i, weights = V2)][ , x])
          Range <- Maximum - Minimum
          Mean <- x[ , wtd.mean(x = i, weights = V2)]
          Variance <- x[ , wtd.var(x = i, weights = V2)]
          SD <- sqrt(Variance)
          x <- data.table(N, Range, Minimum, Maximum, Mean, Variance, SD)
        }
      })
    } else if(!missing(split.vars)) {
      desc.tables <- lapply(X = data.object, FUN = function(i) {
        lapply(X = i, FUN = function(j) {
          x <- as.data.table(j)
          if(is.factor(j[ , i]) | is.character(j[ , i])) {
            x[ , N := .(N = sum(get(weight.var))), by = "i"]
            x[ , (weight.var) := NULL]
            return(unique(x))
          } else if(is.numeric(j[ , i])) {
            N <- x[!is.na(i), sum(get(weight.var)), by = split.vars]
            setnames(x = N, old = "V1", new = "N")
            Minimum <- x[ , lapply(.SD, function(j) {
              suppressWarnings(min(wtd.table(x = j, weights = get(weight.var))[["x"]]))
            }), by = split.vars, .SDcols = "i"]
            setnames(x = Minimum, old = "i", new = "Minimum")
            Maximum <- x[ , lapply(.SD, function(j) {
              suppressWarnings(max(wtd.table(x = j, weights = get(weight.var))[["x"]]))
            }), by = split.vars, .SDcols = "i"]
            setnames(x = Maximum, old = "i", new = "Maximum")
            Range <- merge(x = Minimum, y = Maximum, by = split.vars)
            Range[ , Range := Maximum - Minimum]
            Range[ , c("Minimum", "Maximum") := NULL]
            Mean <- x[ , wtd.mean(x = get("i"), weights = get(weight.var)), by = split.vars]
            setnames(x = Mean, old = "V1", new = "Mean")
            Variance <- suppressWarnings(x[ , wtd.var(x = get("i"), weights = get(weight.var)), by = split.vars])
            setnames(x = Variance, old = "V1", new = "Variance")
            SD <- copy(Variance)
            SD[ , SD := sqrt(Variance)]
            SD[ , Variance := NULL]
            x <- Reduce(function(...) merge(..., all = TRUE, by = split.vars), list(N, Range, Minimum, Maximum, Mean, Variance, SD))
          }
        })
      })
    }
    
    if(missing(split.vars)) {
      desc.tables <- sapply(X = names(desc.tables), function(z) {
        if("Mean" %in% colnames(desc.tables[[z]])) {
          return(desc.tables[[z]])
        } else {
          rbindlist(l = list(desc.tables[[z]], data.table(i = factor("Total"), colSums(desc.tables[[z]][!is.na(i) & !i %in% analysis.vars.missings.attr[z], 2]))), use.names = FALSE)
        }
      }, USE.NAMES = TRUE, simplify = FALSE)
    } else {
      desc.tables <- sapply(X = names(desc.tables), function(y) {
        sapply(X = desc.tables[[y]], FUN = function(z) {
          if("Mean" %in% colnames(z)) {
            return(z)
          } else {
            NA.data.table <- data.table(t(data.table(rep(x = NA, times = length(split.vars)))))
            additional.row <- cbind(NA.data.table, i = factor("Total"), colSums(z[!is.na(i) & !i %in% analysis.vars.missings.attr[y], ncol(z), with = FALSE]))
            rbindlist(l = list(z, additional.row), use.names = FALSE)
          }
        }, USE.NAMES = TRUE, simplify = FALSE)
      }, USE.NAMES = TRUE, simplify = FALSE)
    }
    
    if(missing(split.vars)) {
      desc.tables <- lapply(X = desc.tables, FUN = function(i) {
        if(any(is.na(i[ , 1])) == TRUE) {
          lapply(X = i, FUN = function(j) {
            if(is.factor(j) == TRUE) {
              j <- addNA(j)
              tmp.levels <- levels(unlist(j))
              tmp.levels <- c(tmp.levels[!is.na(tmp.levels)], "<NA>")
              j <- factor(j, levels = tmp.levels, labels = tmp.levels)
              j[is.na(j)] <- "<NA>"
              data.table(j)
            } else {
              data.table(j)
            }
          })
        } else {
          i
        }
      })
    } else {
      desc.tables <- lapply(X = desc.tables, FUN = function(i) {
        lapply(X = i, FUN = function(j) {
          if("i" %in% colnames(j) && any(is.na(j[ , i])) == TRUE) { # Note here we seek for column "i", not list element "i".
            if(is.factor(j[ , i]) == TRUE) {
              j[ , i := addNA(i)]
              tmp.levels <- levels(unlist(j[ , i]))
              tmp.levels <- c(tmp.levels[!is.na(tmp.levels)], "<NA>")
              j[ , i := factor(i, levels = tmp.levels, labels = tmp.levels)]
              j[is.na(i), i := "<NA>"]
            } else {
              data.table(j)
            }
          } else {
            j
          }
        })
      })
    }
    
    if(missing(split.vars)) {
      desc.tables <- lapply(X = desc.tables, FUN = as.data.table)
    }
    
    if(missing(split.vars)) {
      desc.tables <- lapply(X = desc.tables, FUN = function(i) {
        setkeyv(x = i, unlist(colnames(i)[[1]]))
      })
    } else {
      desc.tables <- lapply(X = desc.tables, FUN = function(i) {
        
        lapply(X = i, FUN = function(j) {
          if("i" %in% colnames(j)) {
            setkeyv(x = j, "i")
          } else {
            return(j)
          }
        })
        
      })
    }
    
    if(missing(split.vars)) {
      desc.tables <- sapply(X = names(desc.tables), FUN = function(k) {
        var.colname <- names(desc.tables[[k]])[length(colnames(desc.tables[[k]])) - 1]
        freq.colname <- names(desc.tables[[k]])[length(colnames(desc.tables[[k]]))]
        if(var.colname == "i") {
          desc.tables[[k]][!eval(parse(text = var.colname)) == "Total", Percent := (eval(parse(text = freq.colname))/sum(eval(parse(text = freq.colname))))*100]
          desc.tables[[k]][is.na(Percent), Percent := sum(desc.tables[[k]][!eval(parse(text = var.colname)) %in% c("<NA>", analysis.vars.missings.attr[k]), Percent], na.rm = TRUE)]
        } else {
          return(desc.tables[[k]])
        }
      }, USE.NAMES = TRUE, simplify = FALSE)
    } else {
      desc.tables <- sapply(X = names(desc.tables), FUN = function(j) {
        sapply(X = desc.tables[[j]], FUN = function(k) {
          var.colname <- names(k)[length(colnames(k)) - 1]
          freq.colname <- names(k)[length(colnames(k))]
          if(var.colname == "i") {
            k[!eval(parse(text = var.colname)) == "Total", Percent := (eval(parse(text = freq.colname))/sum(eval(parse(text = freq.colname))))*100]
            k[is.na(Percent), Percent := sum(k[!i %in% c("<NA>", analysis.vars.missings.attr[j]), Percent], na.rm = TRUE)]
          } else {
            return(k)
          }
        }, USE.NAMES = TRUE, simplify = FALSE)
      }, USE.NAMES = TRUE, simplify = FALSE)
    }
    
    if(missing(split.vars)) {
      desc.tables <- sapply(X = names(desc.tables), function(k) {
        var.colname <- names(desc.tables[[k]])[length(colnames(desc.tables[[k]])) - 2]
        freq.colname <- names(desc.tables[[k]])[length(colnames(desc.tables[[k]])) - 1]
        if(var.colname == "i") {
          if(any(levels(unlist(desc.tables[[k]][ , eval(parse(text = var.colname))])) %in% c("<NA>", analysis.vars.missings.attr[k]))) {
            desc.tables[[k]][!eval(parse(text = var.colname)) %in% c("Total", "<NA>", analysis.vars.missings.attr[k]), Valid_Percent := (eval(parse(text = freq.colname))/sum(eval(parse(text = freq.colname))))*100]
          } else {
            desc.tables[[k]][!eval(parse(text = var.colname)) == "Total", Valid_Percent := (eval(parse(text = freq.colname))/sum(eval(parse(text = freq.colname))))*100]
          }
          desc.tables[[k]][is.na(Valid_Percent) & !eval(parse(text = var.colname)) %in% c("<NA>", analysis.vars.missings.attr[k]), Valid_Percent := sum(desc.tables[[k]][ , Valid_Percent], na.rm = TRUE)]
        } else {
          return(desc.tables[[k]])
        }
      }, USE.NAMES = TRUE, simplify = FALSE)
    } else {
      desc.tables <- sapply(X = names(desc.tables), function(j) {
        sapply(X = desc.tables[[j]], FUN = function(k) {
          var.colname <- names(k)[length(colnames(k)) - 2]
          freq.colname <- names(k)[length(colnames(k)) - 1]
          if(var.colname == "i") {
            if(any(levels(unlist(k[ , eval(parse(text = var.colname))])) %in% c("<NA>", analysis.vars.missings.attr[j]))) {
              k[!eval(parse(text = var.colname)) %in% c("Total", "<NA>", analysis.vars.missings.attr[j]), Valid_Percent := (eval(parse(text = freq.colname))/sum(eval(parse(text = freq.colname))))*100]
            } else {
              k[!eval(parse(text = var.colname)) == "Total", Valid_Percent := (eval(parse(text = freq.colname))/sum(eval(parse(text = freq.colname))))*100]
            }
            k[is.na(Valid_Percent) & !eval(parse(text = var.colname)) %in% c("<NA>", analysis.vars.missings.attr[j]), Valid_Percent := sum(k[ , Valid_Percent], na.rm = TRUE)]
          } else {
            return(k)
          }
        }, USE.NAMES = TRUE, simplify = FALSE)
      }, USE.NAMES = TRUE, simplify = FALSE)
    }
    
    if(missing(split.vars)) {
      desc.tables <- lapply(X = desc.tables, function(k) {
        var.colname <- names(k)[length(colnames(k)) - 3]
        freq.colname <- names(k)[length(colnames(k)) - 2]
        if(var.colname == "i") {
          k[grep(pattern = "^Total$|^<NA>$", x = unlist(levels(k[ , eval(parse(text = var.colname))])), invert = TRUE), Cumulative_Percent := cumsum(Valid_Percent)]
        } else {
          return(k)
        }
      })
    } else {
      desc.tables <- lapply(X = desc.tables, function(j) {
        lapply(X = j, FUN = function(k) {
          var.colname <- names(k)[length(colnames(k)) - 3]
          freq.colname <- names(k)[length(colnames(k)) - 2]
          if(var.colname == "i") {
            k[!eval(parse(text = var.colname)) %in% c("Total", "<NA>"), Cumulative_Percent := cumsum( Valid_Percent)]
          } else {
            return(k)
          }
        })
      })
    }
    
    if(missing(split.vars)) {
      desc.tables <- sapply(X = names(desc.tables), function(j) {
        var.colname <- grep(pattern = "^i$|^i.j$", x = colnames(desc.tables[[j]]), ignore.case = TRUE, value = TRUE)
        freq.colname <- grep(pattern = "^N$|^N.j$", x = colnames(desc.tables[[j]]), ignore.case = TRUE, value = TRUE)
        if(length(var.colname) != 0 && any(unlist(levels(unlist(desc.tables[[j]][ , eval(parse(text = (var.colname)))]))) %in% c("<NA>", analysis.vars.missings.attr[[j]]))) {
          total.row <- desc.tables[[j]][eval(parse(text = var.colname)) %in% c("Total", "<NA>", analysis.vars.missings.attr[[j]]), lapply(.SD, sum), .SDcols = c(freq.colname, "Percent")]
          total.row[ , (var.colname) := factor("Total")]
          setcolorder(x = total.row, neworder = c(var.colname, freq.colname, "Percent"))
          desc.tables[[j]] <- rbindlist(l = list(desc.tables[[j]], total.row), fill = TRUE)
        } else {
          desc.tables[[j]]
        }
      }, USE.NAMES = TRUE, simplify = FALSE)
    } else {
      desc.tables <- sapply(X = names(desc.tables), function(j) {
        lapply(X = desc.tables[[j]], FUN = function(k) {
          var.colname <- grep(pattern = "^i$", x = colnames(k), ignore.case = TRUE, value = TRUE)
          freq.colname <- grep(pattern = "^N$", x = colnames(k), ignore.case = TRUE, value = TRUE)
          if(length(var.colname) != 0 && any(unlist(levels(unlist(k[ , eval(parse(text = (var.colname)))]))) %in% c("<NA>", analysis.vars.missings.attr[[j]]))) {
            total.row <- k[eval(parse(text = var.colname)) %in% c("Total", "<NA>", analysis.vars.missings.attr[[j]]), lapply(.SD, sum), .SDcols = c(freq.colname, "Percent")]
            total.row[ , (var.colname) := factor("Total")]
            setcolorder(x = total.row, neworder = c(var.colname, freq.colname, "Percent"))
            k <- rbindlist(l = list(k, total.row), fill = TRUE)
          } else {
            k
          }
        })
      }, USE.NAMES = TRUE, simplify = FALSE)
    }
    
    if(missing(split.vars)) {
      desc.tables <- lapply(X = names(desc.tables), FUN = function(j) {
        var.colname <- names(desc.tables[[j]])[length(colnames(desc.tables[[j]])) - 4]
        if(var.colname == "i") {
          if(any(levels(unlist(desc.tables[[j]][ , eval(parse(text = var.colname))])) %in% c("<NA>", analysis.vars.missings.attr[[j]]))) {
            desc.tables[[j]][ , Value_Type := c(ifelse(test = levels(unlist(desc.tables[[j]][ , eval(parse(text = var.colname))])) %in% c("<NA>", analysis.vars.missings.attr[[j]]) == TRUE, yes = "Missing", no = "Valid"), "Total")]
            desc.tables[[j]][ , Value_Type := factor(x = Value_Type, levels = c("Valid", "Missing", "Total"))]
            setkeyv(x = desc.tables[[j]], cols = c("Value_Type", "i"))
          } else {
            desc.tables[[j]][ , Value_Type := c(rep("Valid", (nrow(desc.tables[[j]]) - 1)), "Total")]
          }
          setcolorder(x = desc.tables[[j]], neworder = c("Value_Type", colnames(desc.tables[[j]])[1:length(colnames(desc.tables[[j]])) - 1]))
        } else {
          return(cbind(data.table(Value_Type = "Valid"), desc.tables[[j]]))
        }
      })
    } else {
      desc.tables <- lapply(X = names(desc.tables), FUN = function(i) {
        lapply(X = desc.tables[[i]], FUN = function(j) {
          var.colname <- names(j)[length(colnames(j)) - 4]
          if(var.colname == "i") {
            j[ , Value_Type := "Valid"]
            j[eval(parse(text = var.colname)) %in% c("<NA>", analysis.vars.missings.attr[i]), Value_Type := "Missing"]
            j[is.na(Valid_Percent) & eval(parse(text = var.colname)) == "Total", Value_Type := "Total"]
            setcolorder(x = j, neworder = c(split.vars, "Value_Type", "i", "N", "Percent", "Valid_Percent", "Cumulative_Percent"))
            
            j[ , Value_Type := factor(x = Value_Type, levels = c("Valid", "Missing", "Total"))]
            setkeyv(x = j, cols = c("Value_Type", "i"))
            
            if(length(split.vars) > 1) {
              j[ , (split.vars) := lapply(.SD, function(l) {
                unique(as.character(unlist(na.omit(l))))
              }), .SDcols = split.vars]
            } else if(length(split.vars) == 1) {
              j[ , (split.vars) := lapply(.SD, function(l) {
                if(is.numeric(l) | is.character(l)) {
                  l <- factor(l)
                  if(all(is.na(l))) {
                    l <- addNA(l)
                  }
                }
                unique.first.col.cat <- unique(l[!is.na(l)])
                c(rep(x = unique(as.character(unlist(na.omit(l)))), times = nrow(j) -1), paste0("Total - ", unique.first.col.cat))
              }), .SDcols = split.vars]
            }
          } else {
            return(cbind(j[ , mget(split.vars)], data.table(Value_Type = "Valid"), j[ , mget(colnames(j)[!colnames(j) %in% split.vars])]))
          }
        })
      })
    }
    
    if(!missing(split.vars)) {
      lapply(desc.tables, function(i) {
        lapply(i, function(j) {
          j[ , (split.vars) := lapply(.SD, function(k) {
            if(!is.factor(k)) {
              ifelse(test = is.na(k), yes = "<NA>", no = k)
            } else {
              k
            }
          }), .SDcols = split.vars]
        })
      })
    }
    
    if(!missing(split.vars)) {
      desc.tables <- lapply(desc.tables, function(i) {
        i[order(names(i))]
      })
    }
    
    if(!missing(split.vars)) {
      desc.tables <- lapply(X = desc.tables, FUN = function(i) {
        rbindlist(l = i)
      })
    }
    
    desc.tables <- lapply(X = seq(desc.tables), function(i) {
      tmp <- data.table(desc.tables[[i]])
      var.colname <- grep(pattern = "^i$|^i.j$", x = colnames(tmp), value = TRUE, ignore.case = TRUE)
      freq.colname <- grep(pattern = "^N$|^N.j$", x = colnames(tmp), value = TRUE, ignore.case = TRUE)
      if(length(var.colname) != 0) {
        setnames(x = tmp, old = var.colname, new = variables[i])
        setnames(x = tmp, old = freq.colname, new = "Frequency")
        return(tmp)
      } else {
        return(tmp)
      }
    })
    
    names(desc.tables) <- variables
    
    if(!missing(split.vars) && length(split.vars) > 1) {
      desc.tables <- lapply(X = desc.tables, FUN = function(i) {
        i <- split(x = i, by = colnames(i)[1])
        lapply(X = i, FUN = function(j) {
          if("Frequency" %in% colnames(j)) {
            first.col <- data.table(paste0("Total - ", unique(j[ , get(colnames(j)[1])])))
            setnames(x = first.col, colnames(j)[1])
            total.row.freq <- cbind(first.col, setDT(as.list(colSums(j[get(j[ , colnames(j)[grep(pattern = split.vars[length(split.vars)], x = colnames(j)) + 2]]) != "Total", mget(grep(pattern = "^Frequency$", x = colnames(j), value = TRUE))]))))
            analysis.column <- colnames(j)[length(split.vars) + 2]
            total.row.pct <- setDT(as.list(colSums(j[get(analysis.column) != "Total", mget(grep(pattern = "^Frequency$", x = colnames(j), value = TRUE))])))
            setnames(x = total.row.pct, "Percent")
            total.row <- cbind(total.row.freq, total.row.pct)
            total.row[ , Percent := (Percent/Frequency)*100]
            rbindlist(l = list(j, total.row), use.names = TRUE, fill = TRUE)
          } else {
            return(j)
          }
          
        })
      })
      
      desc.tables <- lapply(X = desc.tables, FUN = function(i) {
        rbindlist(l = i, use.names = TRUE)
      })
      
    }
    
    export.workbook <- createWorkbook()
    
    lapply(X = variables, FUN = function(i) {
      addWorksheet(wb = export.workbook, sheetName = i)
    })
    
    lapply(X = names(desc.tables), FUN = function(i) {
      writeData(wb = export.workbook, sheet = i, x = desc.tables[[i]], startRow = 6)
    })
    
    apply(X = names.and.labels, MARGIN = 1, FUN = function(i) {
      var.name <- i[["Names"]]
      var.label <- i[["Labels"]]
      
      writeData(wb = export.workbook, sheet = var.name, x = data.table(x = "Variable name", y = "Variable label"), startRow = 3, colNames = FALSE)
      writeData(wb = export.workbook, sheet = var.name, x = data.table(var.name, var.label), startRow = 4, colNames = FALSE)
    })
    
    lapply(X = names(desc.tables), FUN = function(i) {
      setColWidths(wb = export.workbook, sheet = i, widths = "auto", cols = 1:ncol(desc.tables[[i]]))
    })
    
    thousands.style <- createStyle(numFmt = "#,###0", valign = "center")
    
    vertical.alignment <- createStyle(valign = "center")
    
    percentages.style <- createStyle(numFmt = "0.0", valign = "center")
    
    descriptives.style <- createStyle(numFmt = "0.00", valign = "center")
    
    horizontal.alignment <- createStyle(halign = "center")
    
    border.style <- createStyle(border = c("top", "bottom", "left", "right"))
    
    grey.highlight.style <- createStyle(fgFill = "#eaeaea")
    
    table.header.style <- createStyle(fgFill = "#000000", textDecoration = "bold", fontColour = "#ffffff", border = c("top", "bottom", "left", "right"), borderColour = "#ffffff", borderStyle = "double")
    
    return.index.style <- createStyle(fgFill = "#ffff00", textDecoration = "bold")
    
    lapply(X = names(desc.tables), FUN = function(i) {
      addStyle(wb = export.workbook, sheet = i, style = vertical.alignment, cols = 1:ncol(desc.tables[[i]]), rows = c(3:4, 6:(nrow(desc.tables[[i]]) + 6)), gridExpand = TRUE, stack = TRUE)
    })
    
    lapply(X = names(desc.tables), FUN = function(i) {
      addStyle(wb = export.workbook, sheet = i, rows = 6:(nrow(desc.tables[[i]]) + 6), cols = grep(pattern = "Frequency|^N$", x = colnames(desc.tables[[i]])), style = thousands.style, stack = TRUE)
    })
    
    lapply(X = names(desc.tables), FUN = function(i) {
      addStyle(wb = export.workbook, sheet = i, rows = 6:(nrow(desc.tables[[i]]) + 6), cols = grep(pattern = "Percent|Valid_Percent|Cumulative_Percent", x = colnames(desc.tables[[i]])), style = percentages.style, gridExpand = TRUE, stack = TRUE)
    })
    
    lapply(X = names(desc.tables), FUN = function(i) {
      addStyle(wb = export.workbook, sheet = i, rows = 6:(nrow(desc.tables[[i]]) + 6), cols = grep(pattern = "Range|Minimum|Maximum|Mean|Variance|SD", x = colnames(desc.tables[[i]])), style = descriptives.style, gridExpand = TRUE, stack = TRUE)
    })
    
    lapply(X = names(desc.tables), FUN = function(i) {
      addStyle(wb = export.workbook, sheet = i, style = horizontal.alignment, cols = 1:ncol(desc.tables[[i]]), rows = 6, stack = TRUE)
    })
    
    lapply(X = names(desc.tables), FUN = function(i) {
      addStyle(wb = export.workbook, sheet = i, style = vertical.alignment, cols = 1:2, rows = 1, stack = TRUE)
    })
    
    lapply(X = names(desc.tables), FUN = function(i) {
      addStyle(wb = export.workbook, sheet = i, style = border.style, cols = 1:6, rows = 4, gridExpand = TRUE, stack = TRUE)
    })
    
    lapply(X = names(desc.tables), FUN = function(i) {
      addStyle(wb = export.workbook, sheet = i, style = table.header.style, cols = 1:6, rows = 3, gridExpand = TRUE, stack = TRUE)
    })
    
    lapply(X = names(desc.tables), FUN = function(i) {
      addStyle(wb = export.workbook, sheet = i, style = grey.highlight.style, cols = 1:6, rows = 4, gridExpand = TRUE, stack = TRUE)
    })
    
    lapply(X = names(desc.tables), FUN = function(i) {
      addStyle(wb = export.workbook, sheet = i, style = table.header.style, cols = 1:ncol(desc.tables[[i]]), rows = 6, gridExpand = TRUE, stack = TRUE)
    })
    
    lapply(X = names(desc.tables), FUN = function(i) {
      addStyle(wb = export.workbook, sheet = i, style = return.index.style, cols = 1:6, rows = 1, gridExpand = TRUE, stack = TRUE)
    })
    
    lapply(X = names(desc.tables), FUN = function(i) {
      addStyle(wb = export.workbook, sheet = i, style = border.style, cols = 1:ncol(desc.tables[[i]]), rows = 7:(nrow(desc.tables[[i]]) + 6), gridExpand = TRUE, stack = TRUE)
    })
    
    addWorksheet(wb = export.workbook, sheetName = "Index", tabColour = "#FE001A")
    
    setColWidths(wb = export.workbook, sheet = "Index", cols = 1, widths = (max(nchar(c("Variable names", variables))) + 5))
    setColWidths(wb = export.workbook, sheet = "Index", cols = 2, widths = (max(nchar(c("Variable labels", names.and.labels[ , Labels]))) + 10))
    setColWidths(wb = export.workbook, sheet = "Index", cols = 4:5, widths = "auto")
    addStyle(wb = export.workbook, sheet = "Index", style = vertical.alignment, rows = 1:(length(variables) +1), cols = 1:2, gridExpand = TRUE, stack = TRUE)
    addStyle(wb = export.workbook, sheet = "Index", style = vertical.alignment, rows = 1:2, cols = 4:5, gridExpand = TRUE, stack = TRUE)
    
    write.sheet.links <- function(table.names, names.seq.num) {
      
      writeFormula(wb = export.workbook, sheet = "Index", startCol = 1, startRow = names.seq.num, x = makeHyperlinkString(sheet = table.names, text = table.names))
      
    }
    
    mapply(write.sheet.links, table.names = names.and.labels[ , Names], names.seq.num = 2:(length(variables) + 1))
    
    writeData(
      wb = export.workbook,
      sheet = "Index",
      x = data.table(names.and.labels[ , Labels]), startCol = 2,
      startRow = 2,
      colNames = FALSE)
    
    writeData(wb = export.workbook, sheet = "Index", x = data.table("Variable names", "Variable labels"), startCol = 1, startRow = 1, colNames = FALSE)
    
    addStyle(wb = export.workbook, sheet = "Index", rows = 1, cols = 1:2, style = table.header.style, stack = TRUE)
    addStyle(wb = export.workbook, sheet = "Index", rows = 2:(length(variables) + 1), cols = 1:2, style = border.style, gridExpand = TRUE, stack = TRUE)
    
    if(!is.null(study.name)) {
      if(weight.var != "TMPWGT") {
        export.weight.var <- weight.var
      } else {
        export.weight.var <- "none"
      }
      writeData(wb = export.workbook, sheet = "Index", x = data.table(list("Study", "Cycle", "Respondent type", "Weight")), startCol = 4, startRow = 1, colNames = FALSE)
      writeData(wb = export.workbook, sheet = "Index", x = data.table(list(study.name, study.cycle, resp.type, export.weight.var)), startCol = 5, startRow = 1, colNames = FALSE)
      addStyle(wb = export.workbook, sheet = "Index", rows = 1:4, cols = 4, style = table.header.style, stack = TRUE)
      addStyle(wb = export.workbook, sheet = "Index", rows = 1:4, cols = 5, style = border.style, gridExpand = TRUE, stack = TRUE)
    } else if(is.null(study.name)) {
      if(weight.var != "TMPWGT") {
        export.weight.var <- weight.var
      } else {
        export.weight.var <- "none"
      }
      writeData(wb = export.workbook, sheet = "Index", x = data.table(list("Weight")), startCol = 4, startRow = 1, colNames = FALSE)
      writeData(wb = export.workbook, sheet = "Index", x = data.table(list(export.weight.var)), startCol = 5, startRow = 1, colNames = FALSE)
      addStyle(wb = export.workbook, sheet = "Index", rows = 1, cols = 4, style = table.header.style, stack = TRUE)
      addStyle(wb = export.workbook, sheet = "Index", rows = 1, cols = 5, style = border.style, gridExpand = TRUE, stack = TRUE)
    }
    
    worksheetOrder(wb = export.workbook) <- c(length(sheets(export.workbook)), 1:length(variables))
    
    lapply(X = variables, FUN = function(i) {
      writeFormula(wb = export.workbook, sheet = i, startCol = 1, startRow = 1, x = makeHyperlinkString(sheet = "Index", text = "<<Return to the 'Index' sheet"))
    })
    
    lapply(X = variables, FUN = function(i) {
      mergeCells(wb = export.workbook, sheet = i, cols = 1:6, rows = 1)
    })
    
    if(missing(split.vars)) {
      cells.with.valid <- lapply(X = desc.tables, FUN = function(i) {
        grep(pattern = "^Valid$", x = i[ , eval(parse(text = "Value_Type"))]) + 6
      })
      
      valid.cells.ranges <- lapply(X = cells.with.valid, FUN = function(i) {
        split(x = i, f = cumsum(c(TRUE, diff(i) != 1)))
      })
      
      merging.statements <- lapply(X = valid.cells.ranges, FUN = function(i) {
        sapply(X = i, function(j) {
          paste0("mergeCells(wb = export.workbook, cols = 1, rows = ", list(j))
        })
      })
      
      merging.statements <- lapply(X = merging.statements, FUN = function(i) {
        unlist(x = i[!sapply(X = i, FUN = is.null)])
      })
      
      merging.statements <- merging.statements[!sapply(X = merging.statements, FUN = is.null)]
      
      merging.statements <- Map(f = paste0, merging.statements, ", sheet = '", as.list(names(merging.statements)), "')")
      
      eval(parse(text = unlist(merging.statements)))
      
      cells.with.missings <- lapply(X = desc.tables, FUN = function(i) {
        grep(pattern = "^Missing$", x = i[ , eval(parse(text = "Value_Type"))]) + 6
      })
      
      cells.with.missings <- Filter(length, cells.with.missings)
      
      if(length(cells.with.missings)) {
        
        missings.cells.ranges <- lapply(X = cells.with.missings, FUN = function(i) {
          split(x = i, f = cumsum(c(TRUE, diff(i) != 1)))
        })
        
        merging.statements <- lapply(X = missings.cells.ranges, FUN = function(i) {
          sapply(X = i, function(j) {
            paste0("mergeCells(wb = export.workbook, cols = 1, rows = ", list(j))
          })
        })
        
        merging.statements <- lapply(X = merging.statements, FUN = function(i) {
          unlist(x = i[!sapply(X = i, FUN = is.null)])
        })
        
        merging.statements <- merging.statements[!sapply(X = merging.statements, FUN = is.null)]
        
        merging.statements <- Map(f = paste0, merging.statements, ", sheet = '", as.list(names(merging.statements)), "')")
        
        eval(parse(text = unlist(merging.statements)))
      }
    } else {
      unique.split.cats.cells <- lapply(X = desc.tables, FUN = function(i) {
        
        unique.ranges.split.vars <- lapply(X = i[ , mget(split.vars)], FUN = function(j) {
          
          tmp.vals <- unique(as.character(j))
          
          tmp.vals <- lapply(X = tmp.vals, FUN = function(k) {
            which(k == j)
          })
          
          tmp.vals <- lapply(X = tmp.vals, FUN = function(k) {
            tmp.ranges <- split(k, cumsum(c(1, diff(k) != 1)))
            
            lapply(X = tmp.ranges, FUN = function(l) {
              l <- l + 6
              paste(c(l[1], l[length(l)]), collapse = ":")
            })
          })
          
          unlist(tmp.vals)
          
        })
        
      })
      
      unique.split.cats.cells <- lapply(X = unique.split.cats.cells, FUN = function(i) {
        lapply(X = i, FUN = function(j) {
          j[!j == "NA"]
        })
      })
      
      
      merging.statements.split.vars <- lapply(X = names(unique.split.cats.cells), FUN = function(i) {
        lapply(X = names(unique.split.cats.cells[[i]]), FUN = function(j) {
          lapply(X = unique.split.cats.cells[[i]][[j]], FUN = function(k) {
            j <- paste0("mergeCells(wb = export.workbook, cols = ", grep(pattern = j, x = c(split.vars, "Value_Type")), ", rows = ", k, ", sheet = '", i, "')")
          })
        })
      })
      
      eval(parse(text = unlist(merging.statements.split.vars)))
      
      unique.split.cats.value.types <- lapply(X = unique.split.cats.cells, FUN = function(i) {
        i[length(i)]
      })
      
      unique.split.cats.value.types <- lapply(X = unique.split.cats.value.types, FUN = function(i) {
        lapply(X = i, FUN = function(j) {
          lapply(X = j, FUN = function(k) {
            eval(parse(text = k))
          })
        })
      })
      
      unique.split.cats.value.types <- lapply(X = unique.split.cats.value.types, FUN = function(i) {
        lapply(X = i, FUN = function(j) {
          Filter(length, lapply(X = j, FUN = function(k) {
            k[length(k) > 1]
          }))
        })
      })
      
      
      not.valid.positions <- sapply(X = names(unique.split.cats.value.types), FUN = function(i) {
        unlist(lapply(X = i, FUN = function(j) {
          grep(pattern = "^Valid$", x = desc.tables[[j]][ , Value_Type], invert = TRUE) + 6
        }))
      }, USE.NAMES = TRUE, simplify = FALSE)
      
      if(length(unlist(not.valid.positions)) != 0) {
        
        not.valid.positions <- Map(f = function(list1, list2) {
          lapply(X = list1, FUN = function(i) {
            lapply(X = i, FUN = function(j) {
              j[!j %in% list2]
            })
          })
        }, list1 = unique.split.cats.value.types, list2 = not.valid.positions)
        
        merging.statements.value.type <- lapply(X = names(not.valid.positions), FUN = function(i) {
          lapply(X = not.valid.positions[[i]], FUN = function(j) {
            lapply(X = j, FUN = function(k) {
              paste0("mergeCells(wb = export.workbook, cols = ", (length(split.vars) + 1), ", rows = ", paste(c(k[1], k[length(k)]), collapse = ":"), ", sheet = '", i, "')")
            })
          })
        })
        
        eval(parse(text = unlist(merging.statements.value.type)))
      }
      
      not.missing.positions <- sapply(X = names(unique.split.cats.value.types), FUN = function(i) {
        unlist(lapply(X = i, FUN = function(j) {
          grep(pattern = "^Missing$", x = desc.tables[[j]][ , Value_Type], invert = TRUE) + 6
        }))
      }, USE.NAMES = TRUE, simplify = FALSE)
      
      if(length(unlist(not.missing.positions)) != 0) {
        
        not.missing.positions <- Map(f = function(list1, list2) {
          lapply(X = list1, FUN = function(i) {
            lapply(X = i, FUN = function(j) {
              j[!j %in% list2]
            })
          })
        }, list1 = unique.split.cats.value.types, list2 = not.missing.positions)
        
        not.missing.positions <- Filter(length, not.missing.positions)
        
        not.missing.positions <- lapply(X = not.missing.positions, FUN = function(i) {
          lapply(X = i, FUN = function(j) {
            Filter(length, j)
          })
        })
        
        merging.statements.value.type <- lapply(X = names(not.missing.positions), FUN = function(i) {
          lapply(X = not.missing.positions[[i]], FUN = function(j) {
            lapply(X = j, FUN = function(k) {
              paste0("mergeCells(wb = export.workbook, cols = ", (length(split.vars) + 1), ", rows = ", paste(c(k[1], k[length(k)]), collapse = ":"), ", sheet = '", i, "')")
            })
          })
        })
        
        merging.statements.value.type <- unlist(merging.statements.value.type)
        
        if(!is.null(merging.statements.value.type)) {
          eval(parse(text = merging.statements.value.type))
        }
      }
      
    }
    
    if(missing(split.vars)) {
      
      rows.grand.total <- sapply(X = names(desc.tables), FUN = function(i) {
        if("Total" %in% desc.tables[[i]][ , Value_Type]) {
          which(desc.tables[[i]][ , Value_Type] == "Total" & desc.tables[[i]][ , get(i)] == "Total") + 6
        }
      }, USE.NAMES = TRUE, simplify = FALSE)
      
      if(!is.null(unlist(rows.grand.total))) {
        
        rows.grand.total <- Filter(Negate(is.null), rows.grand.total)
        
        merge.statements.grand.total <- lapply(X = names(rows.grand.total), FUN = function(i) {
          paste0("mergeCells(wb = export.workbook, cols = 1:2, rows = ", rows.grand.total[[i]], ", sheet = '", i, "')")
        })
        
        eval(parse(text = unlist(merge.statements.grand.total)))
        
      }
      
    } else if(!missing(split.vars)) {
      rows.grand.total <- sapply(X = names(desc.tables), FUN = function(i) {
        grep(pattern = "^Total", x = unlist(desc.tables[[i]][ , mget(colnames(desc.tables[[i]])[[1]])])) + 6
      }, USE.NAMES = TRUE, simplify = FALSE)
      
      if(length(unlist(rows.grand.total)) != 0) {
        
        rows.grand.total <- Filter(length, rows.grand.total)
        
        remove.merge.statements.grand.total <- lapply(X = names(rows.grand.total), FUN = function(i) {
          paste0("removeCellMerge(wb = export.workbook, cols = ", 1, ", rows = ", rows.grand.total[[i]], ", sheet = '", i, "')")
        })
        
        eval(parse(text = unlist(remove.merge.statements.grand.total)))
        
        merge.statements.grand.total <- lapply(X = names(rows.grand.total), FUN = function(i) {
          paste0("mergeCells(wb = export.workbook, cols = ", paste0("1:", (length(split.vars) + 2)), ", rows = ", rows.grand.total[[i]], ", sheet = '", i, "')")
        })
        
        eval(parse(text = unlist(merge.statements.grand.total)))
      }
    }
    
    lapply(X = variables, FUN = function(i) {
      mergeCells(wb = export.workbook, cols = 2:6, rows = 3, sheet = i)
      mergeCells(wb = export.workbook, cols = 2:6, rows = 4, sheet = i)
    })
    
    freezePane(wb = export.workbook, sheet = "Index", firstRow = TRUE)
    
    freeze.statements <- sapply(X = variables, FUN = function(i) {
      paste0("freezePane(wb = export.workbook, sheet = '", i, "', firstActiveRow = 7)")
    })
    
    eval(parse(text = freeze.statements))
    
    message('Statistics for all ', length(variables), ' variables computed in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.computations}[[3]], "%H:%M:%OS3"), "\n")
    
    if(missing(output.file)) {
      output.file <- file.path(getwd(), "Analysis.xlsx")
    }
    
    does.file.exist <- file.exists(output.file)
    
    withCallingHandlers(
      saveWorkbook(wb = export.workbook, file = output.file, overwrite = TRUE),
      warning = function(w){
        if(grepl("reason 'Permission denied'", w$message)){
          stop('The file in "output.file" (', output.file, ') exists and is open, it cannot be overwritten. Please close the file and try again.', call. = FALSE)
        } else {
          message(w$message)
        }
      })
    
    if(does.file.exist == TRUE) {
      warning('The destination file in "output.file" already existed. It was overwritten.', call. = FALSE)
    }
    
    if(open.output == TRUE && file.exists(output.file)) {
      openXL(file = output.file)
    }
    
  }, interrupt = function(f) {
    message("\nInterrupted by the user. Computations are not finished and output file is not produced.\n")
  })
  
}
