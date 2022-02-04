#' @title Merge study data from different countries and/or respondents
#'
#' @description \code{lsa.merge.data} combines data from different countries and/or different respondents (e.g. students and teachers, or students and schools).
#'
#' @param inp.folder Folder containing the data sets. The data sets must be \code{.RData}, produced
#'                   by \code{lsa.convert.data}. See details.
#' @param file.types What file types (i.e. respondents) shall be merged? See details.
#' @param ISO        Vector containing character ISO codes of the countries' data files to include
#'                   in the merged file. See details.
#' @param out.file   Full path to the file the data shall be stored in. The object stored in the
#'                   file will have the same name. See details.
#'
#' @details
#' The function merges files from studies where the files are per country and respondent type (e.g. student, school, teacher). That is, all studies except PISA.
#'
#' The \code{inp.folder} specifies the path to the folder containing the \code{.RData} files produced by \code{lsa.convert.data}. The folder must contain only files for a single study, single cycle and single population (e.g. TIMSS 2015, grade 4). All files in the input folder must be exported with the same option (\code{TRUE} or \code{FALSE}) of the \code{missing.to.NA} argument of the \code{lsa.convert.data} function. If input folder is not provided to the argument, the working folder (\code{getwd()}) will be used.
#'
#' The \code{file.types} is a list of the respondent types as component names and their variables as elements to be merged. The file type names are three-character codes, the first three characters of the corresponding file names. The elements are vectors of upper case variable names, \code{NULL} takes all variables in the corresponding file. For example, in TIMSS \code{asg} will merge only student-level data from grade 4, \code{c(asg, atg)} will merge the student-level and teacher-level data from grade 4, \code{c(bsg, btm)} will merge student-level and mathematics teacher-level data from grade 8. If a merge is not possible by the study design, the function will stop with an error. See the examples.
#'
#' The \code{ISO} is a character vector specifying the countries whose data shall be merged. The elements of the vector are the fourth, fifth and sixth characters in the file names. For example, \code{c("aus", "swe", "svn")} will merge the data from Australia, Sweden and Slovenia for the file types specified in \code{file.types}. If file for specific country does not exist in the \code{inp.folder}, a warning will be issued. If missing, the files for all countries in the folder will be merged for the specified \code{file.types}.
#'
#' The \code{out.file} must contain full path (including the \code{.RData} extension, if missing, it will be added) to the output file (i.e. the file containing merged data). The file contains object with the same name and has a class extension \code{lsa.data}. It has additional attribute \code{file.type} showing data from which respondents is available after the merging has been done. For example, merging the student-level data with teacher-level data in TIMSS grade 4 will assign "std.bckg.tch.bckg" to this attribute. The object has two additional attributes: study name (\code{study}) and study cycle (\code{cycle}). The object in the \code{.RData} file is keyed on the country ID variable. If output folder is not provided, the merged file will be saved in the working folder (\code{getwd()}) as \code{merged_data.RData}.
#'
#' @return
#' \code{.RData} data file containing an object with class \code{lsa.data}, an extension of the \code{data.table} class. The \code{data.table} object has the same name as the \code{.RData} file it is saved in. The object contains the data from different respondents and/or countries merged and has additional attributes: study name (\code{study}), study cycle (\code{cycle}), and respondent file type (\code{file.type}). Each variable has its own additional attributes: its own label attached to it, if it existed in the source SPSS file. If the \code{missing.to.NA} in the source file was set to \code{TRUE}, each variable has an attribute \code{missings}, containing the user-defined missing values.
#'
#' @examples
#'
#' # Merge TIMSS 2015 grade 4 student and teacher variables for Australia, Chinese Taipei and
#' # Slovenia taking all variables in both files
#' \dontrun{
#' lsa.merge.data(inp.folder = "C:/Data", file.types = list(asg = NULL, atg = NULL),
#' ISO = c("aus", "twn", "svnn"), out.file = "C:/Merged/Merged.RData")
#' }
#'
#' # Same as the above, taking just few variables from each file
#' \dontrun{
#' lsa.merge.data(inp.folder = "C:/Data",
#' file.types = list(asg = c("ASBG01", "ASBG02A", "ASBG02B"),
#' atg = c("ATBG01", "ATBG02", "ATBG03")), ISO = c("aus", "twn", "svnn"),
#' out.file = "C:/Merged/Merged.RData")
#' }
#'
#' @seealso \code{\link{lsa.convert.data}}
#' @export

lsa.merge.data <- function(inp.folder, file.types, ISO, out.file) {
  tmp.options <- options(scipen = 999, digits = 22)
  on.exit(expr = options(tmp.options), add = TRUE)
  if(missing(inp.folder)) {
    inp.folder <- getwd()
  }
  names(file.types) <- tolower(names(file.types))
  if(!is.list(file.types)) {
    stop('The "file.types" argument is missing or does not follow the required format. All operations stop here. Check your input.\n\n', call. = FALSE)
  } else if(is.list(file.types) & is.null(names(file.types))) {
    stop('The "file.types" argument does not follow the required format. Check the RALSA documentation for the required format for this argument. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  file.list <- list.files(path = inp.folder, pattern = ".RData$")
  if(length(file.list) == 0) {
    stop('The folder specified in the "inp.folder" argument does not contain any .RData files. Check your input.\n\n', call. = FALSE)
  } else {
    file.list <- gsub(pattern = ".RData", replacement = "", x = file.list)
  }
  population <- unique(substr(x = file.list, start = 1, stop = 1))
  if(length(population) > 1) {
    stop('The folder specified in the "inp.folder" argument contains .RData files for more than one population. All operations stop here.\n\n', call. = FALSE)
  }
  study.type <- unique(sapply(X = file.list, FUN = function(i) {
    substr(x = i, start = nchar(i)-1, stop = nchar(i)-1)
  }))
  if(length(study.type) > 1) {
    stop('The folder specified in the "inp.folder" argument contains .RData files for more than one study. All operations stop here.\n\n', call. = FALSE)
  }
  study.cycle <- unique(sapply(X = file.list, FUN = function(i) {
    substr(x = i, start = nchar(i), stop = nchar(i))
  }))
  if(length(study.cycle) > 1) {
    stop('The folder specified in the "inp.folder" argument contains .RData files for more than one cycle. All operations stop here.\n\n', call. = FALSE)
  }
  if(missing(file.types)) {
    stop('Argument "file.types" is missing with no default. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(length(setdiff(x = names(file.types), y = unique(substr(x = file.list, start = 1, stop = 3)))) > 0) {
    stop('Some of the file types provided in the "file.types" argument do not exist in the input folder. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(missing(ISO)) {
    ISO <- unique(substr(x = file.list, start = 4, stop = 6))
  }
  ISO <- tolower(ISO)
  if(all(ISO %in% unique(substr(x = file.list, start = 4, stop = 6)) == FALSE)) {
    stop('No files for the specified countries supplied in "ISO" (', paste(ISO, collapse = ", "), ') were found in the "input.folder". All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  warnings.collector <- list()
  ISOs.not.in.files <- setdiff(x = ISO, y = unique(substr(x = file.list, start = 4, stop = 6)))
  if(length(ISOs.not.in.files) > 0) {
    warnings.collector[["nonexisting.ISOs"]] <- ISOs.not.in.files
    ISO <- ISO[!ISO %in% ISOs.not.in.files]
  }
  if(!missing(ISO)) {
    file.list <- grep(pattern = paste(sort(do.call(paste0, expand.grid(names(file.types), ISO))), collapse = "|"), x = file.list, ignore.case = TRUE, value = TRUE)
  } else {
    file.list <- names(file.types)
  }
  file.types.to.merge <- lapply(X = names(file.types), FUN = function(i) {
    paste0(grep(pattern = i, x = file.list, ignore.case = TRUE, value = TRUE), ".RData")
  })
  names(file.types.to.merge) <- names(file.types)
  nonexiting.file.types.to.merge <- sapply(X = file.types.to.merge, FUN = function(i) {
    any(i == ".RData")
  })
  if(any(nonexiting.file.types.to.merge == TRUE)) {
    stop('One or more file types specified in "file.types" argument exists for the countries specified in the "ISO" argument. Check your input.', call. = FALSE)
  }
  study.name <- unique(sapply(X = file.types.to.merge, FUN = function(i) {
    sapply(X = i[1], FUN = function(j) {
      attr(x = import.data(path = file.path(inp.folder, j)), which = "study")
    })
  }))
  study.merge.combinations <- lapply(X = merge.combinations[[study.name]], FUN = function(i) {
    sort(i[1:length(i) - 1])
  })
  which.combination.exists <- sapply(X = study.merge.combinations, FUN = function(i) {
    identical(sort(names(file.types)), i[1:length(i)])
  })
  which.combination.exists <- unlist(merge.combinations[[study.name]][which.combination.exists])
  if(is.null(which.combination.exists)) {
    stop('Merging files for the respondent types specified in the "file.types" argument (', paste(names(file.types), collapse = ', '), ") is not possible. Check the RALSA documentation for the possible merging options in ", study.name, ". All operations stop here. Check your input.\n\n", call. = FALSE)
  }
  if(study.name %in% c("TIMSS", "TIMSS Advanced", "preTIMSS", "eTIMSS PSI", "PIRLS", "prePIRLS", "ePIRLS", "TiPi", "CivED") && grepl(pattern = "tch", x = which.combination.exists[length(which.combination.exists)]) == TRUE) {
    tch.stud.link.files <- list.files(path = inp.folder, pattern = paste("^.st|^bl_", ISO, sep = "", collapse = "|"))
    tch.stud.link.files <- grep(pattern = paste(ISO, collapse = "|"), x = tch.stud.link.files, ignore.case = TRUE, value = TRUE)
    if(length(tch.stud.link.files) == 0) {
      stop('The file types to merge include teacher data, but no teacher-student linkage files were found in the folder specified in "inp.folder". All operations stop here.\n\n', call. = FALSE)
    } else {
      file.types.to.merge[[substr(x = tch.stud.link.files[1], start = 1, stop = 3)]] <- tch.stud.link.files
    }
  }
  tryCatch({
    cnt.lengths.file.types.to.merge <- any(length(ISO) != sapply(X = file.types.to.merge, FUN = function(i) {
      length(i)
    }))
    if(cnt.lengths.file.types.to.merge == TRUE) {
      cnt <- lapply(X = file.types.to.merge, FUN = function(i) {
        substr(x = i, start = 4, stop = 6)
      })
      missing.files <- lapply(X = cnt, FUN = function(i) {
        ISO[which(!ISO %in% i)]
      })
      missing.files <- Filter(length, missing.files)
      missing.files <- unlist(unique(lapply(X = missing.files, FUN = function(i, j) {
        paste0(j, '" missing for ISO "', i, '".')
      }, j = names(missing.files))))
      warnings.collector[["missing.files"]] <- missing.files
    }
    message('\nData files from ', length(file.types), ' respondent types are to be merged.\n')
    message('Data files from ', length(ISO), ' countries are to be merged.\n')
    if(exists("tch.stud.link.files")) {
      message("Student-teacher link files are added.\n")
    }
    file.types.01 <- file.types
    if(any(grepl(pattern = "ast|bst|bl_|mst|pst", x = names(file.types.to.merge)) == TRUE)) {
      file.types.01[grep(pattern = "ast|bst|bl_|mst|pst", x = names(file.types.to.merge), value = TRUE)] <- list(NULL)
    }
    ptm <- proc.time()
    tmp.first.file <- import.data(path = file.path(inp.folder, file.types.to.merge[[1]][1]))
    study.cycle <- attributes(tmp.first.file)[["cycle"]]
    tmp.first.file <- NULL
    all.data.imported <- lapply(X = seq_along(file.types.to.merge), FUN = function(i, j, k) {
      ptm.import <- proc.time()
      imported <- lapply(X = j[[i]], FUN = function(l, m) {
        tmp <- import.data(path = file.path(inp.folder, l))
        if(is.null(m[[i]])) {
          return(tmp)
        } else {
          colnames.not.in.data <- m[[i]][!m[[i]] %in% colnames(tmp)]
          if(length(colnames.not.in.data) > 0) {
            stop(paste0('The following variables passed to "file.types" were not found in the data: ', paste(colnames.not.in.data, collapse = ", "), '. All operations stop here. Check your input.\n\n'), call. = FALSE)
          }
          design.and.ID.IT.cols <- c(grep(pattern = "^ID|^IT[[:alpha:]]+[[:alnum:]]+|FAC|ADJ|^IL|TCER", x = colnames(tmp), ignore.case = TRUE, value = TRUE), grep(pattern = paste(unique(unlist(x = studies.all.design.variables, recursive = TRUE, use.names = FALSE)), collapse = "|"), x = colnames(tmp), ignore.case = TRUE, value = TRUE))
          return(tmp[ , c(m[[i]], design.and.ID.IT.cols), with = FALSE])
        }
      }, m = k)
      import.time <- format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.import}[[3]], "%H:%M:%OS3")
      message('   "', substr(x = basename(j[[i]])[[1]], start = 1, stop = 3), '" files imported in ', import.time)
      return(imported)
    }, j = file.types.to.merge, k = file.types.01)
    names(all.data.imported) <- names(file.types.to.merge)
    variable.labels <- unique(x = rbindlist(lapply(X = all.data.imported, FUN = function(i) {
      all.labels <- unique(x = rbindlist(lapply(X = i, FUN = function(j) {
        tmp.variable.lables <- lapply(X = j, FUN = function(k) {
          attr(x = k, which = "variable.label", exact = TRUE)
        })
        tmp.variable.lables <- Filter(Negate(is.null), tmp.variable.lables)
        data.table(V1 = names(tmp.variable.lables), V2 = unlist(tmp.variable.lables))
      })), by = "V1")
    })), by = "V1")
    imported.data.sets.attributes <- lapply(X = all.data.imported, FUN = function(i) {
      lapply(X = i, FUN = function(j) {
        attributes(j)
      })
    })
    imported.data.sets.attributes <- lapply(X = imported.data.sets.attributes, FUN = function(i) {
      lapply(X = i, FUN = function(j) {
        j[!names(j) %in% c("names", "row.names", ".internal.selfref", "codepage", "class")]
      })
    })
    all.study.attributes <- unique(unlist(lapply(X = imported.data.sets.attributes, FUN = function(i) {
      sapply(X = i, FUN = function(j) {
        j[["study"]]
      })
    })))
    if(length(all.study.attributes) > 1) {
      stop('The folder specified in the "inp.folder" argument contains .RData files for more than one study. All operations stop here.\n\n', call. = FALSE)
    }
    all.cycle.attributes <- unique(unlist(lapply(X = imported.data.sets.attributes, FUN = function(i) {
      sapply(X = i, FUN = function(j) {
        j[["cycle"]]
      })
    })))
    if(length(all.cycle.attributes) > 1) {
      stop('The folder specified in the "inp.folder" argument contains .RData files for more than one cycle. All operations stop here.\n\n', call. = FALSE)
    }
    key.attribute <- unique(unlist(lapply(X = imported.data.sets.attributes, FUN = function(i) {
      sapply(X = i, FUN = function(j) {
        j[["sorted"]]
      })
    })))
    if(study.name == "TIMSS" && study.cycle %in% c("1995", "1999")) {
      all.have.missings.attr <- unlist(lapply(X = all.data.imported[!names(all.data.imported) %in% grep(pattern = "^.st", x = names(all.data.imported), value = TRUE)], FUN = function(i) {
        lapply(X = i, FUN = function(j) {
          all(sapply(X = j, FUN = function(k) {
            is.null(attr(k, "missings"))
          }) == TRUE)
        })
      }))
    } else {
      all.have.missings.attr <- unlist(lapply(X = all.data.imported, FUN = function(i) {
        lapply(X = i, FUN = function(j) {
          all(sapply(X = j, FUN = function(k) {
            is.null(attr(k, "missings"))
          }) == TRUE)
        })
      }))
    }
    if(length(unique(all.have.missings.attr)) > 1) {
      stop('Some of the files passed for merging have been converted with "missing.to.NA = TRUE", others have been converted with "missing.to.NA = FALSE" using the "lsa.convert.data" function. All files for merging have to be converted with the same option in "missing.to.NA", either "TRUE" or "FALSE". All operations stop here.\n\n', call. = FALSE)
    }
    data.have.missings.attr <- unlist(lapply(X = all.data.imported, FUN = function(i) {
      lapply(X = i, FUN = function(j) {
        any(sapply(X = j, FUN = function(k) {
          !is.null(attr(k, "missings"))
        }) == TRUE)
      })
    }))
    lapply(all.data.imported, function(i) {
      lapply(i, function(j) {
        j[ , colnames(j) := lapply(.SD, function(k) {
          if(all(k %in% attr(k, "missings")) == TRUE) {
            setattr(x = k, name = "missings", value = NULL)
            k <- NA
          } else {
            k
          }
        }), .SDcols = colnames(j)]
      })
    })
    if(all(data.have.missings.attr) == TRUE) {
      all.vars.miss <- lapply(X = all.data.imported, FUN = function(i) {
        lapply(X = i, FUN = function(j) {
          Filter(Negate(is.null), lapply(X = j, FUN = function(k) {
            attr(x = k, which = "missings")
          }))
        })
      })
      collapsed.all.vars.miss <- lapply(X = all.vars.miss, FUN = function(i) {
        Reduce(c, i)
      })
      collapsed.all.vars.miss <- lapply(X = collapsed.all.vars.miss, FUN = function(i) {
        lapply(X = i, FUN = function(j) {
          if(is.character(j)) {
            gsub(pattern = "'", replacement = "\\\\'", x = j)
          } else (
            j
          )
        })
      })
      collapsed.all.vars.miss <- lapply(X = collapsed.all.vars.miss, FUN = function(i) {
        sapply(X = unique(names(i)), FUN = function(j) {
          unlist(i[names(i) == j])
        }, simplify = FALSE)
      })
      collapsed.all.vars.miss <- lapply(X = collapsed.all.vars.miss, FUN = function(i) {
        lapply(X = i, FUN = function(j) {
          j[!duplicated(j)]
        })
      })
      collapsed.all.vars.miss <- lapply(X = collapsed.all.vars.miss, FUN = function(i) {
        lapply(X = i, FUN = function(j) {
          if(is.numeric(j)) {
            setNames(object = j, nm = gsub(pattern = "^[[:alnum:]]+\\.", replacement = "", x = names(j)))
          } else if(is.character(j)) {
            if(length(j) > 1) {
              setNames(object = j, nm = gsub(pattern = "[[:digit:]]$", replacement = "", x = names(j)))
            } else {
              j
            }
          }
        })
      })
      collapsed.all.vars.miss.char <- lapply(X = collapsed.all.vars.miss, FUN = function(i) {
        Filter(length, Filter(is.character, i))
      })
      collapsed.all.vars.miss.num <- lapply(X = collapsed.all.vars.miss, FUN = function(i) {
        Filter(length, Filter(is.numeric, i))
      })
      collapsed.all.vars.miss.num <- lapply(X = collapsed.all.vars.miss.num, FUN = function(i) {
        lapply(X = i, FUN = function(j) {
          setattr(x = j, name = "names", value = gsub(pattern = "^[[:alpha:]]\\_[[:alnum:]]+\\.", replacement = "", x = attr(j, "names")))
        })
      })
      char.miss.statements <- lapply(X = collapsed.all.vars.miss.char, FUN = function(i) {
        lapply(X = i, FUN = function(j) {
          if(length(j) == 1) {
            paste0("setattr(x = final.merged.data[['", names(j), "']], name = 'missings', value = '", j, "')")
          } else if(length(j) > 1) {
            paste0("setattr(x = final.merged.data[['", unique(names(j)), "']], name = 'missings', value = c('", paste(j, collapse = "', '"), "'))")
          }
        })
      })
      num.miss.statements <- lapply(X = collapsed.all.vars.miss.num, FUN = function(i) {
        right <- lapply(X = i, FUN = function(j) {
          if(length(j) == 1) {
            paste0("']], name = 'missings',", paste0(" value = c('", names(j), "' = ", j), "))")
          } else if(length(j) > 1) {
            paste0("']], name = 'missings', value = c('", paste(paste0(names(j), "' = ", j), collapse = ", '"), "))")
          }
        })
        paste0("setattr(x = final.merged.data[['", names(i), right)
      })
      all.miss.statements <- c(unlist(char.miss.statements, use.names = FALSE), unlist(num.miss.statements, use.names = FALSE))
    }
    names.all.data.imported <- names(all.data.imported)
    message("")
    all.data.imported <- lapply(X = seq_along(all.data.imported), FUN = function(i, j, k) {
      ptm.adding <- proc.time()
      tmp <- rbindlist(l = j[[i]], use.names = TRUE, fill = TRUE)
      message('   "', k[[i]], '" cases added together in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.adding}[[3]], "%H:%M:%OS3"))
      return(tmp)
    }, j = all.data.imported, k = names(all.data.imported))
    names(all.data.imported) <- names.all.data.imported
    lapply(X = all.data.imported, FUN = function(i) {
      setkeyv(x = i, cols = key.attribute)
    })
    merge.respondents <- function(data.obj, imported.files, design.obj, key.var, add.key.1, add.key.2, add.key.3, add.key.4, add.key.5) {
      all.file.types <- list(
        leader.file.types = c("alg", "blg"),
        school.file.types = c("acg", "bcg", "ccg", "bc_", "icg", "mcg", "pcg"),
        teacher.file.types = c("atg", "bt_", "btg", "ctg", "btm", "bts", "itg", "mtg", "ptg"),
        inst.program.file.type = "dig",
        educator.quest.file.type = "deg",
        future.prim.teacher.quest.file.type = "dpg",
        future.sec.teacher.quest.file.type = "dsg"
      )
      if(study.name != "TALIS 3S") {
        all.file.types[["student.file.types"]] <- c("ash", "asa", "asg", "asc", "bs_", "bsa", "bsg", "cs_", "isg", "isa", "ise", "iss", "isl", "jsa", "jsg", "jse", "msa", "msg", "psa", "psg")
      } else {
        all.file.types[["staff.file.types"]] <- c("asg", "bsg")
      }
      if(length(intersect(all.file.types[["student.file.types"]], imported.files)) == 1) {
        student.level.data <- data.obj[[intersect(all.file.types[["student.file.types"]], imported.files)]]
      } else if(length(intersect(all.file.types[["student.file.types"]], imported.files)) > 1) {
        student.level.data <- data.obj[intersect(all.file.types[["student.file.types"]], imported.files)]
        ach.file.names <- intersect(c("asa", "bsa", "isa", "jsa", "msa", "psa"), names(student.level.data))
        bckg.file.names <- intersect(c("asg", "asc", "bs_", "bsg", "cs_", "isg", "ise", "iss", "isl", "jsg", "jse", "msg", "psg"), names(student.level.data))
        home.file.name <- "ash"
        student.level.data <- Filter(Negate(is.null), student.level.data[c(bckg.file.names, home.file.name, ach.file.names)])
        all.common.columns <- lapply(X = student.level.data, FUN = colnames)
        all.common.columns <- setDT(stack(setNames(all.common.columns, seq_along(all.common.columns))))[ , if(uniqueN(ind) > 1) {
          values
        }, values]$values
        all.common.columns <- all.common.columns[!all.common.columns %in% c(key.var, add.key.1)]
        lapply(X = student.level.data[2:length(student.level.data)], FUN = function(i) {
          suppressWarnings(i[ , (all.common.columns) := NULL])
        })
        lapply(X = student.level.data, FUN = function(i) {
          setkeyv(x = i, cols = c(key.var, add.key.1))
        })
        student.level.data <- Reduce(function(...) merge(..., all = TRUE), student.level.data)
        data.obj[intersect(all.file.types[["student.file.types"]], imported.files)] <- NULL
        assign(x = "all.data.imported", value = data.obj, pos = parent.frame())
      }
      if(any(all.file.types[["school.file.types"]] %in% imported.files) == TRUE) {
        school.level.data <- data.obj[[intersect(all.file.types[["school.file.types"]], imported.files)]]
        data.obj[intersect(all.file.types[["school.file.types"]], imported.files)] <- NULL
        assign(x = "all.data.imported", value = data.obj, pos = parent.frame())
      }
      if(any(all.file.types[["teacher.file.types"]] %in% imported.files) == TRUE) {
        linkage.file <- grep(pattern = "ast|bst|bl_|mst|pst", x = imported.files, value = TRUE)
        if(length(linkage.file) == 0) {
          teacher.level.data <- data.obj[[intersect(all.file.types[["teacher.file.types"]], imported.files)]]
        } else if(length(linkage.file) > 0) {
          teacher.level.data <- data.obj[intersect(c(linkage.file, all.file.types[["teacher.file.types"]]), imported.files)]
          bckg.file.names <- intersect(all.file.types[["teacher.file.types"]], names(teacher.level.data))
          teacher.level.data <- teacher.level.data[c(bckg.file.names, linkage.file)]
          if(study.name %in% c("TIMSS", "eTIMSS PSI") && "btm" %in% names(teacher.level.data)) {
            if(is.factor(teacher.level.data[["bst"]][, MATSUBJ])) {
              teacher.level.data[["bst"]] <- teacher.level.data[["bst"]][MATSUBJ == "Yes", ]
            } else {
              teacher.level.data[["bst"]] <- teacher.level.data[["bst"]][MATSUBJ == 1, ]
            }
            teacher.level.data[["bst"]][ , SCIWGT := NULL]
          } else if(study.name %in% c("TIMSS", "eTIMSS PSI") && "bts" %in% names(teacher.level.data)) {
            if(is.factor(teacher.level.data[["bst"]][, SCISUBJ])) {
              teacher.level.data[["bst"]] <- teacher.level.data[["bst"]][SCISUBJ == "Yes", ]
            } else {
              teacher.level.data[["bst"]] <- teacher.level.data[["bst"]][SCISUBJ == 1, ]
            }
            teacher.level.data[["bst"]][ , MATWGT := NULL]
          }
          all.common.columns <- Reduce(intersect, lapply(X = teacher.level.data, FUN = colnames))
          all.common.columns <- all.common.columns[!all.common.columns %in% c(key.var, add.key.2)]
          lapply(X = teacher.level.data[2:length(teacher.level.data)], FUN = function(i) {
            i[ , (all.common.columns) := NULL]
          })
          if(study.name == "CivED") {
            cols.to.remove <- grep(pattern = "^ID|^IT[[:alpha:]]+[[:alnum:]]+|FAC|ADJ|^IL|TCER|WGT|JK", x = colnames(teacher.level.data[[linkage.file]]), invert = TRUE, value = TRUE)
            teacher.level.data[[linkage.file]][ , (cols.to.remove) := NULL]
          }
          lapply(X = teacher.level.data, FUN = function(i) {
            setkeyv(x = i, cols = c(key.var, add.key.2))
          })
          teacher.level.data <- Reduce(function(...) merge(..., all.y = TRUE), teacher.level.data)
          if(study.name != "CivED") {
            if(study.name %in% c("PIRLS", "ePIRLS", "prePIRLS")) {
              suppressWarnings(teacher.level.data[ , ("TOTWGT") := NULL])
            } else {
              suppressWarnings(teacher.level.data[ , (c("TOTWGT", "TCHWGT")) := NULL])
            }
          }
          data.obj[intersect(c(linkage.file, all.file.types[["teacher.file.types"]]), imported.files)] <- NULL
          assign(x = "all.data.imported", value = data.obj, pos = parent.frame())
        }
      }
      if(any(all.file.types[["inst.program.file.type"]] %in% imported.files) == TRUE) {
        inst.program.level.data <- data.obj[[intersect(all.file.types[["inst.program.file.type"]], imported.files)]]
        data.obj[intersect(all.file.types[["inst.program.file.type"]], imported.files)] <- NULL
        assign(x = "all.data.imported", value = data.obj, pos = parent.frame())
      }
      if(any(all.file.types[["educator.quest.file.type"]] %in% imported.files) == TRUE) {
        educator.level.data <- data.obj[[intersect(all.file.types[["educator.quest.file.type"]], imported.files)]]
        data.obj[intersect(all.file.types[["educator.quest.file.type"]], imported.files)] <- NULL
        assign(x = "all.data.imported", value = data.obj, pos = parent.frame())
      }
      if(any(all.file.types[["future.prim.teacher.quest.file.type"]] %in% imported.files) == TRUE) {
        future.prim.teacher.level.data <- data.obj[[intersect(all.file.types[["future.prim.teacher.quest.file.type"]], imported.files)]]
        data.obj[intersect(all.file.types[["future.prim.teacher.quest.file.type"]], imported.files)] <- NULL
        assign(x = "all.data.imported", value = data.obj, pos = parent.frame())
      }
      if(any(all.file.types[["future.sec.teacher.quest.file.type"]] %in% imported.files) == TRUE) {
        future.sec.teacher.level.data <- data.obj[[intersect(all.file.types[["future.sec.teacher.quest.file.type"]], imported.files)]]
        data.obj[intersect(all.file.types[["future.sec.teacher.quest.file.type"]], imported.files)] <- NULL
        assign(x = "all.data.imported", value = data.obj, pos = parent.frame())
      }
      if(any(all.file.types[["leader.file.types"]] %in% imported.files) == TRUE) {
        leader.level.data <- data.obj[[intersect(all.file.types[["leader.file.types"]], imported.files)]]
        data.obj[intersect(all.file.types[["leader.file.types"]], imported.files)] <- NULL
        assign(x = "all.data.imported", value = data.obj, pos = parent.frame())
      }
      if(any(all.file.types[["staff.file.types"]] %in% imported.files) == TRUE) {
        staff.level.data <- data.obj[[intersect(all.file.types[["staff.file.types"]], imported.files)]]
        data.obj[intersect(all.file.types[["staff.file.types"]], imported.files)] <- NULL
        assign(x = "all.data.imported", value = data.obj, pos = parent.frame())
      }
      existing.merge.types <- names(which(sapply(X = c("student.level.data",
                                                       "school.level.data",
                                                       "teacher.level.data",
                                                       "inst.program.level.data",
                                                       "educator.level.data",
                                                       "future.prim.teacher.level.data",
                                                       "future.sec.teacher.level.data",
                                                       "leader.level.data",
                                                       "staff.level.data"), FUN = function(i) {
                                                         exists(i)
                                                       }) == TRUE))
      if(identical("student.level.data", existing.merge.types) == TRUE) {
        final.merged.data <- student.level.data
        student.level.data <- NULL
      } else if(identical("school.level.data", existing.merge.types) == TRUE) {
        final.merged.data <- school.level.data
        school.level.data <- NULL
      } else if(identical(c("student.level.data", "school.level.data"), existing.merge.types) == TRUE) {
        all.common.columns <- intersect(x = colnames(student.level.data), y = colnames(school.level.data))
        all.common.columns <- all.common.columns[!all.common.columns %in% c(key.var, add.key.3)]
        student.level.data[ , (all.common.columns) := NULL]
        school.file.name <- intersect(all.file.types[["school.file.types"]], imported.files)
        if(identical(names(file.types.01), c("ash", "acg")) || identical(names(file.types.01), c("acg", "ash"))) {
          NULL
        } else {
          suppressWarnings(school.level.data[ , (unique(studies.all.design.variables[["sampling.vars"]][[school.file.name]])) := NULL])
        }
        setkeyv(x = student.level.data, cols = c(key.var, add.key.3))
        setkeyv(x = school.level.data, cols = c(key.var, add.key.3))
        if(study.name %in% c("CivED", "ICCS", "ICILS", "REDS") || study.name == "TIMSS" && study.cycle == "1999" || study.name == "TIMSS Advanced" && study.cycle == "2008") {
          final.merged.data <- merge(x = school.level.data, y = student.level.data, all.y = TRUE)
        } else {
          final.merged.data <- merge(x = school.level.data, y = student.level.data, all.x = TRUE)
        }
        student.level.data <- NULL
        school.level.data <- NULL
      } else if(identical("teacher.level.data", existing.merge.types) == TRUE) {
        final.merged.data <- teacher.level.data
        teacher.level.data <- NULL
      } else if(identical(c("school.level.data", "teacher.level.data"), existing.merge.types) == TRUE) {
        all.common.columns <- intersect(x = colnames(school.level.data), y = colnames(teacher.level.data))
        all.common.columns <- all.common.columns[!all.common.columns %in% c(key.var, add.key.3)]
        if(study.name == "TALIS") {
          all.common.columns <- all.common.columns[!all.common.columns == "SCHWGT"]
        }
        teacher.level.data[ , (all.common.columns) := NULL]
        setkeyv(x = school.level.data, cols = c(key.var, add.key.3))
        setkeyv(x = teacher.level.data, cols = c(key.var, add.key.3))
        school.file.name <- intersect(all.file.types[["school.file.types"]], imported.files)
        if(!study.name %in% c("CivED", "SITES")) {
          suppressWarnings(school.level.data[ , (unique(studies.all.design.variables[["sampling.vars"]][[school.file.name]])) := NULL])
        }
        if(study.name %in% c("CivED", "ICCS", "ICILS", "SITES", "REDS") || study.name == "TALIS" & study.cycle == "2018" || study.name == "TIMSS Advanced" && study.cycle == "2008") {
          final.merged.data <- merge(x = school.level.data, y = teacher.level.data, all.y = TRUE)
        } else {
          final.merged.data <- merge(x = school.level.data, y = teacher.level.data, all.x = TRUE)
        }
        school.level.data <- NULL
        teacher.level.data <- NULL
      } else if(identical(c("student.level.data", "teacher.level.data"), existing.merge.types) == TRUE) {
        all.common.columns <- intersect(x = colnames(student.level.data), y = colnames(teacher.level.data))
        all.common.columns <- all.common.columns[!all.common.columns %in% c(key.var, add.key.1)]
        student.level.data[ , (all.common.columns) := NULL]
        setkeyv(x = student.level.data, cols = c(key.var, add.key.1))
        setkeyv(x = teacher.level.data, cols = c(key.var, add.key.1))
        last.student.file <- length(intersect(all.file.types[["student.file.types"]], imported.files))
        student.file.name <- intersect(all.file.types[["student.file.types"]], imported.files)[last.student.file]
        if(!is.null(studies.all.design.variables[["sampling.vars"]][[student.file.name]])) {
          suppressWarnings(student.level.data[ , (unique(studies.all.design.variables[["sampling.vars"]][[student.file.name]])) := NULL])
        }
        final.merged.data <- merge(x = student.level.data, y = teacher.level.data, all.y = TRUE)
        student.level.data <- NULL
        teacher.level.data <- NULL
      } else if(identical(c("student.level.data", "school.level.data", "teacher.level.data"), existing.merge.types) == TRUE) {
        all.common.columns <- intersect(x = colnames(student.level.data), y = colnames(school.level.data))
        all.common.columns <- all.common.columns[!all.common.columns %in% c(key.var, add.key.3)]
        school.level.data[ , (all.common.columns) := NULL]
        school.file.name <- intersect(all.file.types[["school.file.types"]], imported.files)
        if(study.name != "CivED") {
          suppressWarnings(school.level.data[ , (unique(studies.all.design.variables[["sampling.vars"]][[school.file.name]])) := NULL])
        }
        last.student.file <- length(intersect(all.file.types[["student.file.types"]], imported.files))
        student.file.name <- intersect(all.file.types[["student.file.types"]], imported.files)[last.student.file]
        if(!is.null(studies.all.design.variables[["sampling.vars"]][[student.file.name]])) {
          suppressWarnings(student.level.data[ , (unique(studies.all.design.variables[["sampling.vars"]][[student.file.name]])) := NULL])
        }
        setkeyv(x = student.level.data, cols = c(key.var, add.key.3))
        setkeyv(x = school.level.data, cols = c(key.var, add.key.3))
        if(study.name == "TIMSS" && study.cycle == "1999") {
          tmp <- merge(x = school.level.data, y = student.level.data, all.y = TRUE)
        } else {
          tmp <- merge(x = school.level.data, y = student.level.data, all.x = TRUE)
        }
        student.level.data <- NULL
        school.level.data <- NULL
        all.common.columns <- intersect(x = colnames(tmp), y = colnames(teacher.level.data))
        all.common.columns <- all.common.columns[!all.common.columns %in% c(key.var, add.key.1)]
        teacher.level.data[ , (all.common.columns) := NULL]
        setkeyv(x = tmp, cols = c(key.var, add.key.1))
        setkeyv(x = teacher.level.data, cols = c(key.var, add.key.1))
        if(study.name == "CivED" || study.name == "TIMSS" & study.cycle %in% c("2007", "2011", "2019") & "bts" %in% names(file.types) || study.name == "TIMSS" & study.cycle %in% "2019" & "btm" %in% names(file.types) || study.name == "TIMSS Advanced" & study.cycle == "2008" & all(c("pcg", "ptg") %in% names(file.types) == TRUE)) {
          final.merged.data <- merge(x = tmp, y = teacher.level.data, all.y = TRUE)
        } else {
          final.merged.data <- merge(x = tmp, y = teacher.level.data, all.x = TRUE)
        }
        tmp <- NULL
        teacher.level.data <- NULL
      } else if(identical("inst.program.level.data", existing.merge.types) == TRUE) {
        final.merged.data <- inst.program.level.data
        inst.program.level.data <- NULL
      } else if(identical("educator.level.data", existing.merge.types) == TRUE) {
        final.merged.data <- educator.level.data
        educator.level.data <- NULL
      } else if(identical("future.prim.teacher.level.data", existing.merge.types) == TRUE) {
        final.merged.data <- future.prim.teacher.level.data
        future.prim.teacher.level.data <- NULL
      } else if(identical("future.sec.teacher.level.data", existing.merge.types) == TRUE) {
        final.merged.data <- future.sec.teacher.level.data
        future.sec.teacher.level.data <- NULL
      } else if(identical(c("inst.program.level.data", "future.prim.teacher.level.data"), existing.merge.types) == TRUE) {
        all.common.columns <- intersect(x = colnames(inst.program.level.data), y = colnames(future.prim.teacher.level.data))
        all.common.columns <- all.common.columns[!all.common.columns %in% c(key.var, add.key.4)]
        inst.program.level.data[ , (all.common.columns) := NULL]
        setkeyv(x = future.prim.teacher.level.data, cols = c(key.var, add.key.4))
        setkeyv(x = inst.program.level.data, cols = c(key.var, add.key.4))
        inst.program.file.name <- intersect(all.file.types[["inst.program.file.type"]], imported.files)
        suppressWarnings(inst.program.level.data[ , (studies.all.design.variables[["sampling.vars"]][[inst.program.file.name]]) := NULL])
        final.merged.data <- merge(x = inst.program.level.data, y = future.prim.teacher.level.data, all.y = TRUE)
        inst.program.level.data <- NULL
        future.prim.teacher.file.name <- NULL
      } else if(identical(c("inst.program.level.data", "future.sec.teacher.level.data"), existing.merge.types) == TRUE) {
        all.common.columns <- intersect(x = colnames(inst.program.level.data), y = colnames(future.sec.teacher.level.data))
        all.common.columns <- all.common.columns[!all.common.columns %in% c(key.var, add.key.4)]
        inst.program.level.data[ , (all.common.columns) := NULL]
        setkeyv(x = future.sec.teacher.level.data, cols = c(key.var, add.key.4))
        setkeyv(x = inst.program.level.data, cols = c(key.var, add.key.4))
        inst.program.file.name <- intersect(all.file.types[["inst.program.file.type"]], imported.files)
        suppressWarnings(inst.program.level.data[ , (studies.all.design.variables[["sampling.vars"]][[inst.program.file.name]]) := NULL])
        final.merged.data <- merge(x = inst.program.level.data, y = future.sec.teacher.level.data, all.y = TRUE)
        inst.program.level.data <- NULL
        future.sec.teacher.file.name <- NULL
      } else if(identical(c("staff.level.data"), existing.merge.types) == TRUE) {
        final.merged.data <- staff.level.data
        staff.level.data <- NULL
      } else if(identical(c("leader.level.data"), existing.merge.types) == TRUE) {
        final.merged.data <- leader.level.data
        leader.level.data <- NULL
      } else if(identical(c("leader.level.data", "staff.level.data"), existing.merge.types) == TRUE) {
        all.common.columns <- intersect(x = colnames(leader.level.data), y = colnames(staff.level.data))
        all.common.columns <- all.common.columns[!all.common.columns %in% c(key.var, add.key.5)]
        leader.level.data[ , (all.common.columns) := NULL]
        setkeyv(x = leader.level.data, cols = c(key.var, add.key.5))
        setkeyv(x = staff.level.data, cols = c(key.var, add.key.5))
        leader.file.name <- intersect(all.file.types[["leader.file.types"]], imported.files)
        suppressWarnings(leader.level.data[ , (studies.all.design.variables[["sampling.vars"]][[leader.file.name]]) := NULL])
        final.merged.data <- merge(x = leader.level.data, y = staff.level.data, all.y = TRUE)
        leader.level.data <- NULL
        staff.file.name <- NULL
      }
      final.merged.data[ , (key.var) := droplevels(get(key.var))]
      setkeyv(x = final.merged.data, cols = key.var)
      return(final.merged.data)
    }
    ptm.merge <- proc.time()
    final.merged.data <- merge.respondents(data.obj = all.data.imported, imported.files = names(all.data.imported), design.obj = studies.all.design.variables, key.var = key.attribute, add.key.1 = "IDSTUD", add.key.2 = c("IDTEACH", "IDLINK"), add.key.3 = "IDSCHOOL", add.key.4 = "IDTPU", add.key.5 = "IDCENTRE")
    message('\n   Files from all respondents merged in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.merge}[[3]], "%H:%M:%OS3"), "\n")
    if(exists("all.miss.statements") == TRUE) {
      all.miss.statements <- grep(pattern = paste0("\\<", paste(colnames(final.merged.data), collapse = "\\>|\\<"), "\\>"), x = all.miss.statements, value = TRUE)
      eval(parse(text = all.miss.statements))
      duplicated.num.miss.codes.names <- names(Filter(isTRUE, Filter(length, lapply(X = final.merged.data, FUN = function(i) {
        any(duplicated(names(attr(i, "missings"))) == TRUE)
      }))))
      if(length(duplicated.num.miss.codes.names) > 0) {
        duplicated.num.miss.codes <- lapply(final.merged.data[ , mget(duplicated.num.miss.codes.names)], function(i) {
          attr(i, "missings")
        })
        fixed.miss.duplicates <- lapply(X = names(duplicated.num.miss.codes), FUN = function(i) {
          duplicated.num.miss.codes[[i]][duplicated.num.miss.codes[[i]] %in% final.merged.data[ , get(i)]]
        })
        names(fixed.miss.duplicates) <- duplicated.num.miss.codes.names
        miss.attr.statements <- sapply(X = names(fixed.miss.duplicates), FUN = function(i) {
          if(length(i) == 1) {
            right <- paste0("']], name = 'missings',", paste0(" value = ", fixed.miss.duplicates[i], ")"))
          } else if(length(i) > 1) {
            right <- paste0("']], name = 'missings', value = c('", paste(paste0(names(i), "' = ", fixed.miss.duplicates[i]), collapse = ", '"), "))")
          }
          return(paste0("setattr(x = final.merged.data[['", i, right))
        })
        eval(parse(text = miss.attr.statements))
      }
    }
    existing.variable.labels.columns <- intersect(x = colnames(final.merged.data), y = variable.labels[ , V1])
    variable.labels <- variable.labels[V1 %in% existing.variable.labels.columns]
    variable.labels[ , V2 := gsub(pattern = "'", replacement = "\\\\'", x = V2)]
    variable.labels.statements <- paste0("setattr(x = final.merged.data[['", variable.labels[ , V1], "']], name = 'variable.label', value = '", variable.labels[ , V2], "')")
    sapply(X = variable.labels.statements, FUN = function(i) {
      eval(parse(text = i))
    })
    ptm.save <- proc.time()
    setattr(x = final.merged.data, name = "class", value = c("lsa.data", attr(x = final.merged.data, which = "class")))
    setattr(x = final.merged.data, name = "study", value = study.name)
    setattr(x = final.merged.data, name = "cycle", value = study.cycle)
    setattr(x = final.merged.data, name = "file.type", value = which.combination.exists[length(which.combination.exists)])
    if(missing(out.file)) {
      out.file <- file.path(getwd(), "merged_data.RData")
    }
    if(grepl(pattern = ".RData$", x = out.file) == FALSE) {
      out.file <- paste0(out.file, ".RData")
    }
    assign(x = gsub(pattern = "\\.RData", replacement = "", x = basename(out.file)), value = final.merged.data)
    final.merged.data <- NULL
    save(list = gsub(pattern = "\\.RData", replacement = "", x = basename(out.file)), file = out.file, compress = FALSE)
    message('   The merged data file "', basename(out.file), '" is saved under "', dirname(out.file), '" in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.save}[[3]], "%H:%M:%OS3"))
    message('\n All operations finished in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm}[[3]], "%H:%M:%OS3"), "\n")
  }, interrupt = function(f) {
    message("\nInterrupted by the user. The files for the specified countries and respondents are not merged and merged file is not produced.\n")
  })
  if(length(warnings.collector) > 0) {
    if(!is.null(warnings.collector[["nonexisting.ISOs"]])) {
      warning(paste0('Non-existing country codes passed to "ISO" (ignored, check your input): "', paste(warnings.collector[["nonexisting.ISOs"]], collapse = '", "'), '".'), call. = FALSE)
      message("\n\n")
    }
    if(!is.null(warnings.collector[["missing.files"]])) {
      warning("File types missing for ISO codes:\n", paste0('   "', paste(warnings.collector[["missing.files"]], collapse = '\n   "')), call. = FALSE)
    }
  }
}
