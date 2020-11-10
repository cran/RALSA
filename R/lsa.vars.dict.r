#' @title Produce dictionary for large-scale assessments data variables
#'
#' @description Utility function to display dictionaries of variables from data sets containing objects of class \code{lsa.data}.
#'
#' @param data.file     Full path to the \code{.RData} file containing \code{lsa.data} object.
#'                      Either this or \code{data.object} shall be specified, but not both.
#'                      See details.
#' @param data.object   The object in the memory containing \code{lsa.data} object. Either this
#'                      or \code{data.file} shall be specified, but not both. See details.
#' @param var.names     Vector of variable names whose dictionaries shall be produced.
#'                      See details.
#' @param out.file      Optional, full path to a \code{.txt} file where the dictionaries shall be
#'                      saved, if needed. See details.
#' @param open.out.file Optional, if file path is provided to \code{out.file} shall the produced
#'                      file be open after the file is written?
#'
#' @details
#' Either \code{data.file} or \code{data.object} shall be provided as source of data. If both of them are provided, the function will stop with an error message.
#'
#' If \code{var.names} are not provided, then the function will produce dictionaries for all variables in the file/object.
#'
#' The function will print the dictionaries on the screen. If these need to be saved to a file for further reference as well, a full path to the \code{.txt} file shall be provided. If the file exists, it will be overwritten. If the file name is provided to \code{out.file} and \code{open.out.file = TRUE}, it will be automatically open in the default text editor after being written.
#'
#' @return
#' The dictionaries for the variables in \code{var.names} will be printed as tables on the screen. For each variable the dictionaries contain the variable name, the variable class, the variable label, unique variable values (see below) and the user-defined missing values (if any).
#'
#' The unique values' representation will depend on the variable class. If the variable is a factor, the factor levels will be displayed. If the variable is numeric or character, the unique values will be printed up to the sixth one.
#'
#' The user-defined missing values for factor variables will be as text strings. For the numeric variables these will be integers, followed by their labels in brackets.
#'
#' If a full file path is provided to the \code{out.file}, the same output will be written to a \code{.txt} file with a text on top which data file/object was used.
#'
#' @examples
#' # Display and write to file the dictionaries for multiple factor and numeric variables using
#' # PIRLS 2016 file with teacher and student data from several countries and open the file after
#' # it has been written to the disk.
#' \dontrun{
#' lsa.vars.dict(data.file = "C:/temp/test.RData", var.names = c("ASBG10A", "ASBG10B", "ASBG05A",
#' "ASBG05B", "ASBG05C", "ASBG05D", "ASBG05E", "ASBG05F", "ASBG05G", "ASBG05H", "ASBG06",
#' "ASBG07A", "ASBG07B", "ASBG08", "ATBG05BA", "ATBG05BB", "ATBG05BC", "ATBG05BD"),
#' out.file = "C:/temp/dict.txt", open.out.file = TRUE)
#' }
#'
# Same as above, using lsa.data object in the memory instead of data file.
#' \dontrun{
#' lsa.vars.dict(data.object = test, var.names = c("ASBG10A", "ASBG10B", "ASBG05A", "ASBG05B",
#' "ASBG05C", "ASBG05D", "ASBG05E", "ASBG05F", "ASBG05G", "ASBG05H","ASBG06", "ASBG07A",
#' "ASBG07B", "ASBG08", "ATBG05BA", "ATBG05BB", "ATBG05BC", "ATBG05BD"),
#' out.file = "C:/temp/dict.txt", open.out.file = TRUE)
#' }
#'
#' @seealso \code{\link{lsa.convert.data}}, \code{\link{lsa.recode.vars}}
#' @export

lsa.vars.dict <- function(data.file, data.object, var.names, out.file, open.out.file = FALSE) {

  tmp.options <- options(scipen = 999, digits = 22)
  on.exit(expr = options(tmp.options), add = TRUE)


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

  if(missing(var.names)) {
    var.names <- colnames(data)
  } else {
    if(any(var.names %in% colnames(data) == FALSE)) {
      stop('\nOne or more variable names passed to the "var.names" argument do not exist in the data. All operations stop here. Check your input.\n\n', call. = FALSE)
    }
  }

  tryCatch({

  cols.to.delete <- grep(pattern = paste(var.names, collapse = "|"), x = colnames(data), value = TRUE, invert = TRUE)
  if(length(cols.to.delete) > 0) {
    data[ , (grep(pattern = paste(var.names, collapse = "|"), x = colnames(data), value = TRUE, invert = TRUE)) := NULL]
  }

  ptm.data.dictionary <- proc.time()

  var.names <- as.list(var.names)
  names(var.names) <- unlist(var.names)

  var.classes <- lapply(X = data, FUN = class)

  var.labels <- lapply(X = data, FUN = function(i) {
    if(length(attr(x = i, which = "variable.label")) > 0) {
      paste0("'", attr(x = i, which = "variable.label"), "'")
    } else {
      ""
    }
  })

  var.unique.values <- lapply(X = data, FUN = function(i) {
    if(is.factor(i)) {
      if(length(levels(i)) > 2) {
        c(paste0("'", levels(i)[1], "'\n"), paste0("                 '", levels(i)[2:(length(levels(i)) - 1)], "'\n"), paste0("                 '", levels(i)[length(levels(i))], "'"))
      } else {
        c(paste0("'", levels(i)[1], "'\n"), paste0("                 '", levels(i)[2]))
      }
    } else {
      if(length(unique(i)) > 6) {
        paste0(paste(head(x = unique(i)), collapse = ", "), "...\n                  (truncated, ", length(unique(i)), " omitted)")
      } else {
        paste(unique(i), collapse = ", ")
      }
    }
  })

  var.user.missings <- lapply(X = data, FUN = function(i) {
    miss.attr <- attr(x = i, which = "missings")
    if(is.factor(i)) {

      if(length(miss.attr) == 0) {
        ""
      } else if(length(miss.attr) == 1) {
        paste0("'", miss.attr, "'")
      } else if(length(miss.attr) == 2) {
        c(paste0("'", miss.attr[1], "'\n"), paste0("                 '", miss.attr[2], "'"))
      } else if (length(miss.attr) > 2) {
        c(paste0("'", miss.attr[1], "'\n"), paste0("                 '", miss.attr[2:(length(miss.attr) - 1)], "\n"), paste0("                 '", miss.attr[length(miss.attr)], "'"))
      }

    } else if(is.numeric(i)) {
      if(length(miss.attr) == 0) {
        ""
      } else if(length(miss.attr) == 1) {
        paste0(miss.attr, " ('", names(miss.attr), "')")
      } else if(length(miss.attr) == 2) {
        c(paste0(miss.attr[1], " ('", names(miss.attr)[1], "')", "\n"), paste0("                 ", miss.attr[2], " ('", names(miss.attr)[2], "')"))
      } else if(length(miss.attr) > 2) {
        c(paste0(miss.attr[1], " ('", names(miss.attr)[1], "')\n"), paste0("                 ", miss.attr[2:(length(miss.attr) - 1)], " ('", names(miss.attr)[2:(length(miss.attr) - 1)], "')\n"), paste0("                 ", miss.attr[length(miss.attr)], " ('", names(miss.attr)[length(miss.attr)], "')"))
      }
    }
  })

  vars.dict <- lapply(X = names(var.names), FUN = function(i) {
    list(var.names[[i]],
         var.classes[[i]],
         var.labels[[i]],
         var.unique.values[[i]],
         var.user.missings[[i]])
  })

  message("")
  message("The following tables contain the dictionaries for the variables of interest.\n")

  invisible(lapply(X = vars.dict, FUN = function(i) {
    message(paste(rep(x = "+", times = unlist(options("width")) - 10), collapse = ""))
    message("Variable name:   '", i[[1]], "'")
    message(paste(rep(x = "-", times = unlist(options("width")) - 10), collapse = ""))
    message("Variable class:  '", i[[2]], "'")
    message(paste(rep(x = "-", times = unlist(options("width")) - 10), collapse = ""))
    message("Variable label:  ", i[[3]])
    message(paste(rep(x = "-", times = unlist(options("width")) - 10), collapse = ""))
    if(i[[2]] == "factor") {
      message("Variable levels: ", i[[4]])
    } else {
      message("Unique values:    ", i[[4]])
    }
    message(paste(rep(x = "-", times = unlist(options("width")) - 10), collapse = ""))
    message("User missing:    ", i[[5]])
    message(paste(rep(x = "+", times = unlist(options("width")) - 10), collapse = ""))
    message("\n\n")
  }))

  var.word.count <- if(length(var.names) == 1) {
    list("Dictionary", " variable")
  } else {
    list("Dictionaries", " variables")
  }

  message('\n', var.word.count[[1]], ' for ',  length(var.names), var.word.count[[2]], ' produced in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.data.dictionary}[[3]], "%H:%M:%OS3"))

  if(!missing(out.file)) {
    ptm.write.dictionary <- proc.time()

    cat("", file = out.file)

    cat(paste(rep(x = "+", times = unlist(options("width")) - 10), collapse = ""), file = out.file, append = TRUE)
    if(!missing(data.file)) {
      cat("\nUsed data file:", data.file, "\n", file = out.file, append = TRUE)
    } else if(!missing(data.object)) {
      cat("\nUsed object in memory:", used.data, "\n", file = out.file, append = TRUE)
    }
    cat(paste(rep(x = "+", times = unlist(options("width")) - 10), collapse = ""), file = out.file, append = TRUE)
    cat("\n\n\n\n", file = out.file, append = TRUE)
    invisible(sapply(X = vars.dict, FUN = function(i) {
      cat(paste(rep(x = "+", times = unlist(options("width")) - 10), collapse = ""), file = out.file, append = TRUE)
      cat("\nVariable name:   '", i[[1]], "'\n", sep = "", file = out.file, append = TRUE)
      cat(paste(rep(x = "-", times = unlist(options("width")) - 10), collapse = ""), file = out.file, append = TRUE)
      cat("\nVariable class:  '", i[[2]], "'\n", sep = "", file = out.file, append = TRUE)
      cat(paste(rep(x = "-", times = unlist(options("width")) - 10), collapse = ""), file = out.file, append = TRUE)
      cat("\nVariable label:  ", i[[3]], "\n", sep = "", file = out.file, append = TRUE)
      cat(paste(rep(x = "-", times = unlist(options("width")) - 10), collapse = ""), file = out.file, append = TRUE)
      if(i[[2]] == "factor") {
        cat("\nVariable levels: ", i[[4]], "\n", sep = "", file = out.file, append = TRUE)
      } else {
        cat("\nUnique values:    ", i[[4]], "\n", sep = "", file = out.file, append = TRUE)
      }
      cat(paste(rep(x = "-", times = unlist(options("width")) - 10), collapse = ""), file = out.file, append = TRUE)
      cat("\nUser missing:    ", i[[5]], "\n", sep = "", file = out.file, append = TRUE)
      cat(paste(rep(x = "+", times = unlist(options("width")) - 10), collapse = ""), "\n", file = out.file, append = TRUE)
      cat("\n\n\n\n", file = out.file, append = TRUE)
    }))

    message('\nVariable dictionaries written to disk in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.write.dictionary}[[3]], "%H:%M:%OS3"), "\n\n")


    if(open.out.file == TRUE) {
      if(Sys.info()["sysname"] == "Windows") {
        shell.exec(out.file)
      } else if(Sys.info()["sysname"] == "Linux") {
        system(paste0("xdg-open ", out.file))
      } else if(Sys.info()["sysname"] == "Darwin") {
        system(paste0("open ", out.file))
      }
    }

  }

  }, interrupt = function(f) {
    message("\n\nInterrupted by the user. Not all requested dictionaries have been produced.")
  },
  error = function(e) {
    message("")
  })

}
