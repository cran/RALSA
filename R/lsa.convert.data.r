#' @name lsa.convert.data
#' @aliases lsa.convert.data
#' @aliases print.lsa.data
#' @aliases lsa.select.countries.PISA
#'
#' @title Convert Large-Scale Assessments' Datasets to .RData Format
#'
#' @description \code{lsa.convert.data} converts datasets from large-scale assessments from their original formats (SPSS or ASCII text) into \code{.RData} files. \code{print} prints the properties of an \code{lsa.data} objects on screen. \code{lsa.select.countries.PISA} lets selecting PISA data from specific countries for analysis.
#'
#' @param inp.folder    The folder containing the IEA-like SPSS data files or ASCII text files and
#'                      \code{.sps} import files for OECD PISA data from cycles prior to 2015.
#'                      See details.
#'                      If blank, the working directory (\code{getwd()}) is used.
#' @param PISApre15     When converting PISA files, set to \code{TRUE} if the input files are from
#'                      PISA cycles prior 2015 (ASCII text format with \code{.sps} control files)
#'                      or to \code{FALSE} (default) if they are in SPSS \code{.sav} format, as in
#'                      the case of IEA studies and the like and OECD PISA 2015 or later. Ignored
#'                      if the input folder contains IEA-like studies.
#' @param ISO           Vector containing character ISO codes of the countries' data files to
#'                      convert (e.g. \code{ISO = c("aus", "svn")}). If none of the files contain
#'                      the specified ISO codes in their names, the codes are ignored and a warning
#'                      is shown. Ignored when converting PISA files (both for cycles prior 2015
#'                      and 2015 and later). This argument is case-insensitive, i.e. the ISO codes
#'                      can be passed as lower- or upper-case.
#'                      (lower or upper) as the original SPSS \code{.sav} files.
#' @param missing.to.NA Should the user-defined missing values be recoded to \code{NA}? If
#'                      \code{TRUE}, all user-defined missing values from the SPSS files (or
#'                      specified in the OECD PISA import syntax files) are all imported as
#'                      \code{NA}. If \code{FALSE} (default), they are converted to valid values
#'                      and the missing codes are assigned to an attribute \code{missings} for
#'                      each variable. See details.
#' @param out.folder    Path to the folder where the converted files will be stored. If omitted,
#'                      same as the \code{inp.folder}, and if the \code{inp.folder} is missing as
#'                      well, this will be \code{getwd()}.
#' @param x             (\code{print} only) \code{lsa.data} object.
#' @param col.nums      (\code{print} only) Which columns to print, positions by number.
#' @param data.file     (\code{lsa.select.countries.PISA} only) Converted PISA data file to select
#'                      countries' data from. Either this one or \code{data.object} must be
#'                      provided, but not both. See details.
#' @param data.object   (\code{lsa.select.countries.PISA} only) PISA object in memory to filter.
#'                      Either this one or \code{data.file} must be provided, but not both.
#'                      See details.
#' @param cnt.names     (\code{lsa.select.countries.PISA} only) Character vector containing the
#'                      names of the countries, as they exist in the data, which should stay in the
#'                      PISA exported file or object in memory.
#' @param output.file   (\code{lsa.select.countries.PISA} only) Full path to the file with the filtered
#'                      countries' data to be written on disk. If not provided, the PISA object
#'                      will be written to memory.
#' @param ...           (\code{print} only) Further arguments passed to or from other methods.
#'
#' @details
#' The \code{lsa.convert.data} function converts the originally provided data files into \code{.RData} sets. RALSA adds its own method for printing \code{lsa.data} objects on screen. The \code{lsa.select.countries.PISA} is a utility function that allows the user to select countries of interest from a converted PISA data file (or PISA object residing in memory) and remove the rest of the countries' data. This is useful when the user does not want to analyze all countries data in a PISA file.
#'
#' \itemize{
#' \item \strong{\code{lsa.convert.data}}
#'
#' IEA studies, as well as OECD TALIS and some conducted by other organizations, provide their data in SPSS \code{.sav} format with same or very similar structure: one file per country and type of respondent (e.g. school principal, student, teacher, etc.) per population. For IEA studies and OECD TALIS use the \code{ISO} argument to specify the countries' three-letter ISO codes whose data is to be converted. The three-letter ISO codes for each country can be found in the user guide for the study in scope. For example, the ISO codes of the countries participating in PIRLS 2016 can be found in its user guide on pages 52-54. To convert the files from all countries in the downloaded data from IEA studies and OECD TALIS, simply omit the \code{ISO} argument. Cycles of OECD PISA prior to 2015, on the other hand, do not provide SPSS \code{.sav} or other binary files, but ASCII text files, accompanied with SPSS syntax (\code{.sps}) files that are used to import the text files into SPSS. These files are per each type of respondent containing all countries' data. The \code{lsa.convert.data} function converts the data from either source assuring that the structure of the output \code{.RData} files is the same, although the structure of the input files is different (SPSS binary files vs. ASCII text files plus import \code{.sps} files). The data from PISA 2015 and later, on the other hand, is provided in SPSS format (all countries in one file per type of respondent). Thus, the \code{PISApre15} argument needs to be specified as \code{TRUE} when converting data sets from PISA prior to its 2015 cycle. The default for the \code{PISApre15} argument is \code{FALSE} which means that the function expects to find IEA-like SPSS binary files per country and type of respondent in the directory in \code{inp.folder} or OECD PISA 2015 (or later) SPSS \code{.sav} files. If \code{PISApre15 = TRUE} and country codes are provided to \code{ISO}, they will be ignored because PISA files contain data from all countries together.
#'
#' The files to be converted must be in a folder on their own, from a single study, single cycle and single population. In addition, if there are more than one file types per study, cycle and population, these also must be in different folders. For example, in TIMSS 2019 the grade 8 data files are main (end with "m7", electronic version of the paper administered items), bridge (end with "b7", paper administration with trend items for countries participating in previous TIMSS cycles) and Problem Solving and Inquiry (PSI) tasks (end with "z7", electronic administration only, optional for countries). These different types must be in separate folders. In case of OECD PISA prior 2015, the folder must contain both the ASCII text files and the SPSS \code{.sps} import syntax files. If the folder contains data sets from more than one study or cycle, the operation will break with error messages.
#'
#' If the path for the \code{inp.folder} argument is not specified, the function will search for files in the working directory (i.e. as returned by \code{getwd()}). If folder path for the the \code{out.folder} is not specified, it will take the one from the \code{inp.folder} and the files will be stored there. If both the \code{inp.folder} and \code{out.folder} arguments are missing, the directory from \code{getwd()} will be used to search, convert and store files.
#'
#' If \code{missing.to.NA} is set to \code{TRUE}, all user-defined missing values from the SPSS will be imported as \code{NA} which is \code{R}'s only kind of missing value. This will be the most often case when analyzing these data since the reason why the response is missing will be irrelevant most of the times. However, if it is needed to know why the reasons for missing responses, as when analyzing achievement items (i.e. not administered vs. omitted or not reached), the argument shall be set to \code{FALSE} (default for this argument) which will convert all user-defined missing values as valid ones.
#'
#' \item \strong{\code{print}}
#'
#' RALSA uses its own method for printing objects of class \code{lsa.data} on screen. Passing just the object name to the console will print summarized information about the study's data and the first six columns of the dataset (see the Value section). If \code{col.nums} specifies which columns from the dataset shall be included in the output (see examples).
#'
#' \item \strong{\code{lsa.select.countries.PISA}}
#'
#' \code{lsa.select.countries.PISA} lets the user to take a PISA dataset, either a converted file or \code{lsa.data} object in the memory and reduce the number of countries in it by passing the names of the countries which need to be kept as a character vector to the \code{cnt.names} argument. If full path (including the file name) to the resulting file is specified in the \code{output.file} argument, it will be written on disk. If not, the data will be written to an \code{lsa.object} in memory with the same name as the input file. See the examples.
#' }
#'
#' @return
#'
#' \itemize{
#'
#' \item \strong{\code{lsa.convert.data}}
#'
#' \code{.RData} data files, containing an object with class \code{lsa.data}, an extension of the \code{data.table} class. The \code{data.table} object has the same name as the \code{.RData} file it is saved in. The object has additional attributes: study name (\code{study}), study cycle (\code{cycle}), and respondent file type (\code{file.type}). Each variable has its own additional attributes: its own label attached to it, if it existed in the source SPSS file. If the \code{missing.to.NA} was set to \code{TRUE}, each variable has an attribute \code{missings}, containing the user-defined missing values from the SPSS files.
#'
#' The object in the \code{.RData} file is keyed on the country ID variable.
#'
#' \item \strong{\code{print}}
#'
#' Prints the information of an \code{lsa.data} object (study, cycle, respondent type, number of countries, key -- country ID, and if the variables have user-defined missing values) and a preview of the data. The default preview (when no \code{col.nums}) are specified will include the first six columns.
#'
#' \item \strong{\code{lsa.select.countries.PISA}}
#'
#'  Writes a file containing an \code{lsa.object} with the data for the countries passed to the \code{cnt.names} argument, if the \code{output.file} argument is used. If the \code{output.file} argument is not used, the \code{lsa.object} will be written to the memory with the same name as the file name in \code{inp.file}.
#' }
#'
#' @note
#' When downloading the \code{.sps} files (ASCII text and control \code{.sps}) for OECD PISA files prior to the 2015 cycle (say http://www.oecd.org/pisa/pisaproducts/pisa2009database-downloadabledata.htm), save them **without changing their names and without modifying the file contents**. The function will look for the files as they were named originally.
#'
#' Different studies and cycles define the "I don't know" (or similar) category of discrete variables in different ways - either as a valid or missing value. The \code{lsa.convert.data} function sets all such or similar codes to missing value. If this has to be changed, the \code{lsa.recode.vars} can be used as well (also see \code{lsa.vars.dict}).
#'
#' @examples
#' # Convert all IEA-like SPSS files in the working directory, setting all user-defined missing
#' # values to \code{NA}
#' \dontrun{
#' lsa.convert.data(missing.to.NA = TRUE)
#' }
#'
#'
#' # Convert IEA TIMSS 2011 grade 8 data from Australia and Slovenia, keeping all user-defined
#' # missing values as valid ones specifying custom input and output directories
#' \dontrun{
#' lsa.convert.data(inp.folder = "C:/TIMSS_2011_G8", ISO = c("aus", "svn"), missing.to.NA = FALSE,
#' out.folder = "C:/Data")
#' }
#'
#' # Convert OECD PISA 2009 files converting all user-defined missing values to \code{NA}
#' # using custom input and output directories
#' \dontrun{
#' lsa.convert.data(inp.folder = "/media/PISA_2009", PISApre15 = TRUE, missing.to.NA = TRUE,
#' out.folder = "/tmp")
#' }
#'
#' # Print 20th to 25th column in PISA 2018 student questionnaire dataset loaded into memory
#' \dontrun{
#' print(x = cy07_msu_stu_qqq, col.nums = 20:25)
#' }
#'
#' # Select data from Albania and Slovenia from PISA 2018 student questionnaire dataset
#' # and save it under the same file name in a different folder
#' \dontrun{
#' lsa.select.countries.PISA(data.file = "C:/PISA/cy07_msu_stu_qqq.RData",
#' cnt.names = c("Albania", "Slovenia"),
#' output.file = "C:/PISA/Reduced/cy07_msu_stu_qqq.RData")
#' }
#'
#' @references
#' Foy, P. (Ed.). (2018). \emph{PIRLS 2016 User Guide for the International Database}. TIMSS & PIRLS International Study Center.
#'
#' @seealso \code{\link{lsa.merge.data}}, \code{\link{lsa.vars.dict}}, \code{\link{lsa.recode.vars}}
#' @export
#' @rdname lsa.convert.data
lsa.convert.data <- function(inp.folder, PISApre15 = FALSE, ISO, missing.to.NA = FALSE, out.folder) {
  tmp.options <- options(scipen = 999, digits = 22)
  on.exit(expr = options(tmp.options), add = TRUE)
  tryCatch({
    if(missing(x = inp.folder)) {
      inp.folder = getwd()
    } else {
      inp.folder <- paste(inp.folder)
    }
    if(missing(x = out.folder)) {
      out.folder <- paste(inp.folder)
    } else {
      out.folder <- paste(out.folder)
    }
    if(PISApre15 == FALSE) {
      if(missing(x = ISO)) {
        ISO <- paste(list.files(path = inp.folder, pattern = ".sav", ignore.case = TRUE), sep = "")
        if(length(ISO) == 0) {
          if(dir.exists(paths = inp.folder) == TRUE) {
            message("\nError:\nThe input folder contains no \".sav\" files to read. Check your input.\n\n")
          } else {
            message("\nError:\nThe input folder does not exist. Check your input.\n\n")
          }
        }
        if(any(sapply(X = ISO, FUN = nchar) == 12 )) {
          full.inp.file.path <- file.path(inp.folder, ISO)
          study.and.cycle <- unique(x = tolower(substr(x = ISO, start = 7, stop = 8)))
          if(length(x = study.and.cycle) > 1) {
            message("\nError:\nThe input folder contains data files from more than one study and/or cycle. The input folder must contain files from single study and cycle only.\n\n")
            stop()
          }
        } else if(any(sapply(X = ISO, FUN = nchar) > 12 )) {
          full.inp.file.path <- file.path(inp.folder, ISO)
          first.four.char <- unique(x = substr(x = tolower(ISO), start = 1, stop = 4))
          if(grepl(pattern = "0", x = first.four.char)[1]) {
            study.and.cycle <- unique(x = substr(x = tolower(ISO), start = 1, stop = 4))
          } else {
            study.and.cycle <- unique(x = substr(x = tolower(ISO), start = 1, stop = 3))
          }
          if(length(x = tolower(study.and.cycle)) > 1) {
            message("\nError:\nThe input folder contains data files from more than one study and/or cycle. The input folder must contain files from single study and cycle only.\n\n")
            stop()
          }
        }
      } else {
        files <- list.files(path = inp.folder, pattern = ".sav", ignore.case = TRUE)
        cnt.ISOs <- unique(x = substr(x = files, start = 4, stop = 6))
        if(length(files) > 0 & length(intersect(x = tolower(ISO), tolower(cnt.ISOs))) == 0) {
          message("\nError:\nNo country codes matching the data files in the input folder were entered in \"ISO = ...\". \nCheck your input and the files in the input folder.\n\n")
        }
        if(length(files) == 0) {
          if(dir.exists(paths = inp.folder) == TRUE) {
            message("\nError:\nThe input folder contains no \".sav\" files to read. Check your input.\n\n")
          } else {
            message("\nError:\nThe input folder does not exist. Check your input.\n\n")
          }
        }
        study.and.cycle <- unique(x = tolower(substr(x = files, start = 7, stop = 8)))
        if(length(x = tolower(study.and.cycle)) > 1) {
          message("\nError:\nThe input folder contains data files from more than one study and/or cycle. The input folder must contain files from single study and cycle only.\n\n")
          stop()
        }
        available.ISOs <- unique(substring(text = files, first = 4, last = 8))
        if(any(tolower(paste0(ISO, study.and.cycle)) %in% tolower(available.ISOs) == "FALSE")) {
          warning("Data files for one or more countries in \"ISO = ...\"", " do not exist in the specified folder.", "\nCheck your input.\n", call. = FALSE)
        }
        ISO <- files[grep(pattern = paste0(ISO, study.and.cycle, collapse = "|"), x = files, ignore.case = TRUE)]
        full.inp.file.path <- file.path(inp.folder, ISO)
      }
      root.file.names <- sapply(X = strsplit(x = ISO, split = "\\."), FUN = "[", 1)
      full.out.file.path <- paste(file.path(out.folder, tolower(x = root.file.names)), "RData", sep = ".")
      if(length(ISO) > 0) {
        if(dir.exists(paths = out.folder) == TRUE) {
          if(length(ISO) == 1) {
            message("\n", "One dataset found for conversion. Some datasets can be rather large. Please be patient.\n")
          } else {
            message("\n", paste(length(ISO), " datasets found for conversion. Some datasets can be rather large. Please be patient.\n"))
          }
        } else {
          message("\nError:\nThe output folder does not exist. Check your input.\n\n")
        }
      } else {
        if(dir.exists(paths = out.folder) == FALSE) {
          message("\nError:\nThe output folder does not exist. Check your input.\n\n")
        }
      }
      counter <- 0
      convert.IEA <- function(inp.file, out.file, new.file) {
        ptm <- proc.time()
        tmp <- tryCatch({
          read_sav(file = inp.file, user_na = TRUE)
        }, error = function(e) {
          read_sav(file = inp.file, user_na = TRUE, encoding = "latin1")
        })
        if(any(sapply(X = ISO, FUN = nchar) == 12 )) {
          inp.file.first.char <- substr(x = basename(inp.file), start = 1, stop = 1)
        } else if (any(sapply(X = ISO, FUN = nchar) > 12 )) {
          first.four.char <- unique(x = substr(x = tolower(ISO), start = 1, stop = 4))
          if(grepl(pattern = "0", x = first.four.char)[1]) {
            inp.file.first.char <- substr(x = basename(inp.file), start = 1, stop = 4)
          } else {
            inp.file.first.char <- substr(x = basename(inp.file), start = 1, stop = 3)
          }
        }
        inp.file.first.char <- tolower(inp.file.first.char)
        study.and.cycle <- tolower(study.and.cycle)
        if(inp.file.first.char %in% c("a", "b") && study.and.cycle == "m1") {
          study.attribute <- "TIMSS"
          cycle.attribute <- "1995"
        } else if(inp.file.first.char %in% c("a", "b") && study.and.cycle == "m2") {
          study.attribute <- "TIMSS"
          cycle.attribute <- "1999"
        } else if(inp.file.first.char %in% c("a", "b") && study.and.cycle == "m3") {
          study.attribute <- "TIMSS"
          cycle.attribute <- "2003"
        } else if(inp.file.first.char %in% c("a", "b") && study.and.cycle == "m4") {
          study.attribute <- "TIMSS"
          cycle.attribute <- "2007"
        } else if(inp.file.first.char %in% c("a", "b") && study.and.cycle == "b4") {
          study.attribute <- "TIMSS"
          cycle.attribute <- "2007"
        } else if(inp.file.first.char %in% c("a", "b") && study.and.cycle == "m5") {
          study.attribute <- "TIMSS"
          cycle.attribute <- "2011"
        } else if(inp.file.first.char %in% c("a", "b") && study.and.cycle == "m6") {
          study.attribute <- "TIMSS"
          cycle.attribute <- "2015"
        } else if(inp.file.first.char %in% c("a", "b") && study.and.cycle == "m7") {
          study.attribute <- "TIMSS"
          cycle.attribute <- "2019"
        } else if(inp.file.first.char %in% c("a", "b") && study.and.cycle == "b7") {
          study.attribute <- "TIMSS"
          cycle.attribute <- "2019"
        } else if(inp.file.first.char %in% c("a", "b") && study.and.cycle == "z7") {
          study.attribute <- "eTIMSS PSI"
          cycle.attribute <- "2019"
        } else if(inp.file.first.char %in% c("a", "b") && study.and.cycle == "m8") {
          study.attribute <- "TIMSS"
          cycle.attribute <- "2023"
        } else if(inp.file.first.char %in% c("a", "b") && study.and.cycle == "m9") {
          study.attribute <- "TIMSS"
          cycle.attribute <- "2027"
        } else if(inp.file.first.char %in% c("a", "b") && study.and.cycle == "n1") {
          study.attribute <- "preTIMSS"
          cycle.attribute <- "2015"
        } else if(inp.file.first.char %in% c("m", "p") && study.and.cycle == "m1") {
          study.attribute <- "TIMSS Advanced"
          cycle.attribute <- "1995"
        } else if(inp.file.first.char %in% c("m", "p") && study.and.cycle == "m2") {
          study.attribute <- "TIMSS Advanced"
          cycle.attribute <- "2008"
        } else if(inp.file.first.char %in% c("m", "p") && study.and.cycle == "m3") {
          study.attribute <- "TIMSS Advanced"
          cycle.attribute <- "2015"
        } else if(inp.file.first.char == "a" && study.and.cycle == "r1") {
          study.attribute <- "PIRLS"
          cycle.attribute <- "2001"
        } else if(inp.file.first.char == "a" && study.and.cycle == "r2") {
          study.attribute <- "PIRLS"
          cycle.attribute <- "2006"
        } else if(inp.file.first.char == "a" && study.and.cycle == "r3") {
          study.attribute <- "PIRLS"
          cycle.attribute <- "2011"
        } else if(inp.file.first.char == "a" && study.and.cycle == "r4") {
          study.attribute <- "PIRLS"
          cycle.attribute <- "2016"
        } else if(inp.file.first.char == "a" && study.and.cycle == "r5") {
          study.attribute <- "PIRLS"
          cycle.attribute <- "2021"
        } else if(inp.file.first.char == "a" && study.and.cycle == "a5") {
          study.attribute <- "PIRLS"
          cycle.attribute <- "2021"
        } else if(inp.file.first.char == "a" && study.and.cycle == "r6") {
          study.attribute <- "PIRLS"
          cycle.attribute <- "2026"
        } else if(inp.file.first.char == "a" && study.and.cycle == "l1") {
          study.attribute <- "prePIRLS"
          cycle.attribute <- "2011"
        } else if(inp.file.first.char == "a" && study.and.cycle == "l2") {
          study.attribute <- "prePIRLS"
          cycle.attribute <- "2016"
        } else if(inp.file.first.char == "a" && study.and.cycle == "e1") {
          study.attribute <- "ePIRLS"
          cycle.attribute <- "2016"
        } else if(inp.file.first.char == "a" && study.and.cycle == "b1") {
          study.attribute <- "TiPi"
          cycle.attribute <- "2011"
        } else if(inp.file.first.char == "a" && study.and.cycle == "t1") {
          study.attribute <- "RLII"
          cycle.attribute <- "1991"
        } else if(inp.file.first.char == "a" && study.and.cycle == "t2") {
          study.attribute <- "RLII"
          cycle.attribute <- "2001"
        } else if(inp.file.first.char %in% c("a", "b", "c") && study.and.cycle == "s0") {
          study.attribute <- "SITES"
          cycle.attribute <- "1998"
        } else if(inp.file.first.char == "b" && study.and.cycle == "s1" && "DPCDATE" %in% colnames(tmp) && unique(tmp[ , "DPCDATE"]) < 20191102) {
          study.attribute <- "SITES"
          cycle.attribute <- "2006"
        } else if(inp.file.first.char %in% c("b", "c") && study.and.cycle == "f2") {
          study.attribute <- "CivED"
          cycle.attribute <- "1999"
        } else if(inp.file.first.char %in% c("i", "j") && study.and.cycle == "c2") {
          study.attribute <- "ICCS"
          cycle.attribute <- "2009"
        } else if(inp.file.first.char == "i" && study.and.cycle == "c3") {
          study.attribute <- "ICCS"
          cycle.attribute <- "2016"
        } else if(inp.file.first.char == "i" && study.and.cycle == "c4") {
          study.attribute <- "ICCS"
          cycle.attribute <- "2022"
        } else if(inp.file.first.char == "i" && study.and.cycle == "b4") {
          study.attribute <- "ICCS"
          cycle.attribute <- "2022"
        } else if(inp.file.first.char == "i" && study.and.cycle == "c5") {
          study.attribute <- "ICCS"
          cycle.attribute <- "2027"
        } else if(inp.file.first.char == "b"  && study.and.cycle == "i1") {
          study.attribute <- "ICILS"
          cycle.attribute <- "2013"
        } else if(inp.file.first.char == "b"  && study.and.cycle == "i2") {
          study.attribute <- "ICILS"
          cycle.attribute <- "2018"
        } else if(inp.file.first.char == "b"  && study.and.cycle == "i3") {
          study.attribute <- "ICILS"
          cycle.attribute <- "2023"
        } else if(inp.file.first.char == "b"  && study.and.cycle == "t1") {
          study.attribute <- "TALIS"
          cycle.attribute <- "2008"
        } else if(inp.file.first.char %in% c("a", "b", "c", "p")  && study.and.cycle == "t2") {
          study.attribute <- "TALIS"
          cycle.attribute <- "2013"
        } else if(inp.file.first.char %in% c("a", "b", "c", "p")  && study.and.cycle == "t3") {
          study.attribute <- "TALIS"
          cycle.attribute <- "2018"
        } else if(inp.file.first.char %in% c("a", "b", "c", "p")  && study.and.cycle == "t4") {
          study.attribute <- "TALIS"
          cycle.attribute <- "2024"
        } else if(inp.file.first.char %in% c("a", "b")  && study.and.cycle == "s1" && "IEADATE" %in% colnames(tmp) && unique(tmp[ , "IEADATE"]) > 20191102) {
          study.attribute <- "TALIS 3S"
          cycle.attribute <- "2018"
        } else if(inp.file.first.char %in% c("a", "b")  && study.and.cycle == "s2" && "IEADATE" %in% colnames(tmp) && unique(tmp[ , "IEADATE"]) > 20191102) {
          study.attribute <- "TALIS 3S"
          cycle.attribute <- "2024"
        } else if(inp.file.first.char == "d"  && study.and.cycle == "t1") {
          study.attribute <- "TEDS-M"
          cycle.attribute <- "2008"
        } else if(inp.file.first.char == "b"  && study.and.cycle == "v1") {
          study.attribute <- "REDS"
          cycle.attribute <- "2021"
        } else if(inp.file.first.char == "cy6") {
          study.attribute <- "PISA"
          cycle.attribute <- "2015"
        } else if(inp.file.first.char == "cy07") {
          study.attribute <- "PISA"
          cycle.attribute <- "2018"
        } else if(inp.file.first.char == "cy08") {
          study.attribute <- "PISA"
          cycle.attribute <- "2022"
        } else if(inp.file.first.char == "cy1") {
          study.attribute <- "PISA for Development"
          cycle.attribute <- "2019"
        }
        base.ISO <- gsub(pattern = ".sav$", replacement = "", x = basename(inp.file), ignore.case = TRUE)
        if(any(sapply(X = ISO, FUN = nchar) == 12 )) {
          inp.file.abbrev <- substr(x = basename(inp.file), start = 1, stop = 3)
        } else if (any(sapply(X = ISO, FUN = nchar) > 12 )) {
          if(study.and.cycle %in% c("cy6", "cy07", "cy08")) {
            inp.file.abbrev <- substr(x = base.ISO, start = nchar(base.ISO) - 6, stop = nchar(base.ISO))
          } else if(study.and.cycle == "cy1") {
            if(nchar(basename(inp.file)) == 19) {
              inp.file.abbrev <- substr(x = base.ISO, start = nchar(base.ISO) - 6, stop = nchar(base.ISO))
            } else if(nchar(basename(inp.file)) == 15) {
              inp.file.abbrev <- substr(x = base.ISO, start = nchar(base.ISO) - 2, stop = nchar(base.ISO))
            }
          }
        }
        if(inp.file.abbrev %in% c("alg", "ALG", "blg", "BLG")) {
          file.type.attribute <- "leader.bckg"
        } else if(study.attribute != "TALIS 3S" && inp.file.abbrev %in% c("asg", "bsg", "ASG", "BSG", "asc", "bs_", "cs_", "isg", "ISG", "jsg", "JSG")) {
          file.type.attribute <- "std.bckg"
        } else if(study.attribute == "TALIS 3S" && inp.file.abbrev %in% c("asg", "bsg", "ASG", "BSG")) {
          file.type.attribute <- "staff.bckg"
        } else if(inp.file.abbrev %in% c("msg", "MSG")) {
          file.type.attribute <- "math.std.bckg"
        } else if(inp.file.abbrev %in% c("psg", "PSG")) {
          file.type.attribute <- "phys.std.bckg"
        } else if(inp.file.abbrev %in% c("asa", "bsa", "ASA", "BSA", "isa", "ISA", "jsa", "JSA")) {
          file.type.attribute <- "std.ach"
        } else if(inp.file.abbrev %in% c("msa", "MSA")) {
          file.type.attribute <- "math.std.ach"
        } else if(inp.file.abbrev %in% c("psa", "PSA")) {
          file.type.attribute <- "phys.std.ach"
        } else if(inp.file.abbrev %in% c("asr", "bsr", "ASR", "BSR", "isr", "ISR", "jsr", "JSR")) {
          file.type.attribute <- "std.rel"
        } else if(inp.file.abbrev %in% c("msr", "MSR")) {
          file.type.attribute <- "math.std.rel"
        } else if(inp.file.abbrev %in% c("psr", "PSR")) {
          file.type.attribute <- "phys.std.rel"
        } else if(inp.file.abbrev %in% c("ash", "ASH")) {
          file.type.attribute <- "std.home"
        } else if(inp.file.abbrev %in% c("ise", "ISE", "jse", "JSE")) {
          file.type.attribute <- "std.EUM"
        } else if(inp.file.abbrev %in% c("iss", "ISS")) {
          file.type.attribute <- "std.AM"
        } else if(inp.file.abbrev %in% c("isl", "ISL")) {
          file.type.attribute <- "std.LAM"
        } else if(inp.file.abbrev %in% c("ast", "bst", "AST", "BST", "bl_")) {
          file.type.attribute <- "std-tch.lnk"
        } else if(inp.file.abbrev %in% c("mst", "MST")) {
          file.type.attribute <- "math.std-tch.lnk"
        } else if(inp.file.abbrev %in% c("pst", "PST")) {
          file.type.attribute <- "phys.std-tch.lnk"
        } else if(inp.file.abbrev %in% c("atg", "bt_", "itg", "ITG", "btg", "ATG", "BTG", "CTG", "PTG")) {
          file.type.attribute <- "tch.bckg"
        } else if(inp.file.abbrev %in% c("btm", "BTM", "mtg", "MTG")) {
          file.type.attribute <- "math.tch.bckg"
        } else if(inp.file.abbrev %in% c("bts", "BTS")) {
          file.type.attribute <- "sci.tch.bckg"
        } else if(inp.file.abbrev %in% c("ptg", "PTG")) {
          file.type.attribute <- "phys.tch.bckg"
        } else if(inp.file.abbrev == "DPG") {
          file.type.attribute <- "prim.tch.bckg"
        } else if(inp.file.abbrev == "DSG") {
          file.type.attribute <- "low-sec.tch.bckg"
        } else if(inp.file.abbrev == "DPR") {
          file.type.attribute <- "prim.tch.rel"
        } else if(inp.file.abbrev == "DSR") {
          file.type.attribute <- "low-sec.tch.rel"
        } else if(inp.file.abbrev %in% c("acg", "bcg", "bc_", "icg", "ICG", "ACG", "BCG", "CCG", "PCG")) {
          file.type.attribute <- "sch.bckg"
        } else if(inp.file.abbrev %in% c("mcg", "MCG")) {
          file.type.attribute <- "math.sch.bckg"
        } else if(inp.file.abbrev %in% c("pcg", "PCG")) {
          file.type.attribute <- "phys.sch.bckg"
        } else if(inp.file.abbrev == "axg") {
          file.type.attribute <- "prim.sch.bckg"
        } else if(inp.file.abbrev == "bxg") {
          file.type.attribute <- "low-sec.sch.bckg"
        } else if(inp.file.abbrev == "cxg") {
          file.type.attribute <- "upp-sec.sch.bckg"
        } else if(inp.file.abbrev == "DIG") {
          file.type.attribute <- "inst.bckg"
        } else if(inp.file.abbrev == "DEG") {
          file.type.attribute <- "educ.bckg"
        } else if(inp.file.abbrev %in% c("isp", "ISP", "asp", "ASP", "bsp", "BSP")) {
          file.type.attribute <- "std.proc"
        } else if(inp.file.abbrev %in% c("SCH_QQQ", "scq_qqq")) {
          file.type.attribute <- "sch.bckg"
        } else if(inp.file.abbrev %in% c("STU_QTM", "stu_qtm")) {
          file.type.attribute <- "std.qtm"
        } else if(inp.file.abbrev %in% c("STU_TTM", "stu_ttm")) {
          file.type.attribute <- "std.ttm"
        } else if(inp.file.abbrev %in% c("STU_COG", "stu_cog")) {
          file.type.attribute <- "std.ach"
        } else if(inp.file.abbrev %in% c("STU_QQQ", "stu_qqq")) {
          file.type.attribute <- "std.bckg"
        } else if(inp.file.abbrev %in% c("STU_QQ2", "stu_qq2")) {
          file.type.attribute <- "std.bckg"
        } else if(inp.file.abbrev %in% c("STU_FLT", "stu_flt")) {
          file.type.attribute <- "std.fin.lit"
        } else if(inp.file.abbrev %in% c("FLT_QQQ", "flt_qqq")) {
          file.type.attribute <- "std.fin.lit.bckg"
        } else if(inp.file.abbrev %in% c("FLT_COG", "flt_cog")) {
          file.type.attribute <- "std.fin.lit.ach"
        } else if(inp.file.abbrev %in% c("FLT_TTM", "flt_ttm")) {
          file.type.attribute <- "std.fin.lit.ttm"
        } else if(inp.file.abbrev %in% c("FLT_TIM", "flt_tim")) {
          file.type.attribute <- "std.fin.lit.tim"
        } else if(inp.file.abbrev %in% c("STU_CPS", "stu_cps")) {
          file.type.attribute <- "std.col.prob.slv"
        } else if(inp.file.abbrev %in% c("TCH_QQQ", "tch_qqq")) {
          file.type.attribute <- "tch.bckg"
        } else if(inp.file.abbrev %in% c("STU_TIM", "stu_tim")) {
          file.type.attribute <- "std.qtm"
        } else if(inp.file.abbrev %in% c("STU_PVS", "stu_pvs")) {
          file.type.attribute <- "std.bckg"
        } else if(inp.file.abbrev %in% c("COG", "cog")) {
          file.type.attribute <- "out.of.school.ach"
        } else if(inp.file.abbrev %in% c("QQQ", "qqq")) {
          file.type.attribute <- "out.of.school.bckg"
        } else if(inp.file.abbrev %in% c("TIM", "tim")) {
          file.type.attribute <- "out.of.school.qtm"
        }
        tmp <- reshape.imported(object = tmp, to.NA = missing.to.NA, study = study.attribute, cycle = cycle.attribute, type = file.type.attribute)
        assign(x = tolower(new.file), value = tmp)
        tmp <- NULL
        save(list = tolower(new.file), file = out.file, compress = FALSE)
        counter <<- counter + 1
        message("     ",
                if(nchar(counter) == 1) {
                  paste0("(  ", counter, "/", length(ISO), ")   ")
                } else if(nchar(counter) == 2) {
                  paste0("( ", counter, "/", length(ISO), ")   ")
                } else if(nchar(counter) > 2) {
                  paste0("(", counter, "/", length(ISO), ")   ")
                },
                basename(inp.file), " converted in ", format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm}[[3]], "%H:%M:%OS3"))
      }
      ptm <- proc.time()
      suppressWarnings(
        invisible(mapply(FUN = convert.IEA, inp.file = full.inp.file.path, out.file = full.out.file.path, new.file = root.file.names))
      )
      if(length(ISO) == 1) {
        message("\n", " The selected file successfully converted in ", format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm}[[3]], "%H:%M:%OS3"), "\n\n")
      } else {
        message("\n All ", length(ISO), " found files successfully converted in ", format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm}[[3]] - 1, "%H:%M:%OS3"), "\n\n")
      }
    } else if(PISApre15 == TRUE) {
      sps.files <- list.files(path = inp.folder, pattern = "\\.sps")
      PISA.cycle <- unique(str_extract(string = sps.files, pattern = "^[[:alpha:]]+[[:digit:]]+"))
      if(length(PISA.cycle) > 1) {
        message("\nError:\nThe input folder contains data files from more than one PISA pre 2015 cycle. The input folder must contain files from single study and cycle only.\n\n")
        stop()
      }
      txt.files <- list.files(path = inp.folder, pattern = "\\.txt")
      if(dir.exists(paths = inp.folder) == FALSE) {
        message("\nError:\nThe input folder does not exist. Check your input.\n\n")
      } else {
        if(length(sps.files) == 0) {
          message("\nError:\nThe input folder contains no control (\".sps\") files to read. Check your input.\n\n")
        }
        if(length(txt.files) == 0) {
          message("\nError:\nThe input folder contains no data (\".txt\") files to read. Check your input.\n\n")
        }
      }
      full.inp.file.path <- file.path(inp.folder, sps.files)
      root.file.names <- sapply(X = strsplit(x = sps.files, split = "\\."), FUN = "[", 1)
      full.out.file.path <- paste(file.path(out.folder, tolower(x = root.file.names)), "RData", sep = ".")
      if(length(sps.files) > 0 & length(txt.files) > 0) {
        message("\n", paste(length(txt.files), " datasets selected for conversion. Some datasets can be rather large. Please be patient.\n"))
      }
      count.success <- 0
      convert.PISApre15 <- function(inp.file, out.file, new.file) {
        tryCatch({
          ptm <- proc.time()
          syntax.path <- inp.file
          raw.spss.syntax <- readLines(con = syntax.path)
          line.encodings <- sapply(X = stri_enc_detect(raw.spss.syntax), FUN = function(i) {
            i[[1]][[1]][1]
          })
          most.freq.encoding <- names(sort(table(line.encodings), decreasing = TRUE)[1])
          spss.syntax <- str_trim(string = gsub(pattern = "^\t+|\\\"|\'", replacement = "", x = readLines(con <- file(syntax.path, encoding = most.freq.encoding))))
          close(con)
          spss.syntax <- stri_enc_toutf8(spss.syntax)
          spss.syntax <- gsub(pattern = "\t+", replacement = " ", x = spss.syntax)
          spss.syntax <- gsub(pattern = "/\\s+", replacement = "/", x = spss.syntax)
          spss.syntax <- gsub(pattern = "[[:space:]]{2, }", x = spss.syntax, replacement = " ")
          spss.syntax <- spss.syntax[spss.syntax != ""]
          comments <- grep(pattern = "^.{3}\\*|^\\*", x = spss.syntax)
          if(length(x = comments) != 0) {
            spss.syntax <- spss.syntax[-comments]
          }
          variables <- grep(pattern = "^/*[[:alpha:]]+(\\_)*([[:alnum:]]+)*\\s\\d+\\s*-\\s*\\d+\\s*", x = spss.syntax, value = TRUE)
          variables <- gsub(pattern = "/", replacement = "", x = variables)
          variables <- gsub(pattern = "\\)\\s+\\.", replacement = ").", x = variables)
          var.names <- toupper(word(string = variables, start = 1, end = 1))
          ranges <- str_extract(string = variables, pattern = "[[:digit:]]+\\s*-\\s*[[:digit:]]+")
          ranges.start <- as.numeric(gsub(pattern = "\\s*-", replacement = "", x = str_extract(string = variables, pattern = "[[:digit:]]+\\s*-")))
          ranges.end <- as.numeric(gsub(pattern = "-\\s*", replacement = "", x = str_extract(string = variables, pattern = "-\\s*[[:digit:]]+")))
          no.formats <- grep(pattern = "\\)\\.?$", x = variables, invert = TRUE)
          if(length(x = no.formats) != 0) {
            variables[no.formats] <- paste(variables[no.formats], "(F,0)")
          }
          var.formats <- toupper(x = grep(pattern = "[[:alpha:]]", x = gsub(pattern = "[^[:alpha:]]|[[:space:]]", replacement = "", x = word(string = variables, start = str_count(variables, "\\S+"), end = str_count(variables, "\\S+"))), value = TRUE))
          var.dec <- grepl(pattern = "[1-9]+", x = word(string = variables, start = str_count(variables, "\\S+"), end = str_count(variables, "\\S+")))
          var.classes <- ifelse(var.formats == "F" & var.dec == FALSE, "factor", ifelse(var.formats == "F" & var.dec == TRUE, "numeric", ifelse(var.formats == "A", "character", "undefined")))
          file.name.string <- gsub(pattern = "\\\\", replacement = "/", x = grep(pattern = "data list file", x = spss.syntax, ignore.case = TRUE, value = TRUE))
          txt.file.name <- basename(gsub(pattern = "data list file *=*|\\.txt\\s*/*$", replacement = "", x = file.name.string, ignore.case = TRUE))
          txt.file.name <- gsub(pattern = "PAQ09", replacement = "PAR09", x = txt.file.name)
          txt.file.name <- gsub(pattern = "SCQ09_DEC11", replacement = "SCQ09_Dec11", x = txt.file.name)
          txt.file.name <- gsub(pattern = "cogn_2003", replacement = "cogn_2003_v2", x = txt.file.name)
          txt.file.path <- paste0(dirname(syntax.path), "/", txt.file.name, ".txt")
          if(file.exists(txt.file.path) == TRUE) {
            count.success <<- count.success + 1
          }
          tmp <- suppressMessages(data.frame(read_fwf(file = txt.file.path, fwf_positions(start = ranges.start, end = ranges.end, col_names = var.names), progress = FALSE)))
          assign.class <- function(data, classes) do.call(paste("as", classes, sep = "."), list(data))
          tmp <- replace(tmp,, Map(f = assign.class, data = tmp, classes = var.classes))
          character.vars <- names(Filter(is.character, tmp))
          tmp[character.vars] <- lapply(X = tmp[character.vars], FUN = function(i) {
            i <- sub(pattern = "^0+([1-9])", "\\1", x = i)
            i <- gsub(pattern = "^0+$", replacement = "0", x = i)
          })
          value.labels.first <- grep(pattern = "val[[:alpha:]]* lab[[:alpha:]]*", x = spss.syntax, ignore.case = TRUE)[1]
          value.labels.part <- spss.syntax[value.labels.first:length(spss.syntax)]
          clean.unwanted.statements <- function(text, expression) {
            statements <- grep(pattern = expression, x = text, ignore.case = TRUE)
            if(length(statements) != 0) {
              periods.after.statements <- grep(pattern = "\\.$", x = text)
              periods.after.first.statement <- periods.after.statements[which(periods.after.statements >= statements[1])]
              periods.terminating.statements <- head(periods.after.first.statement, n = length(statements))
              ranges.statements <- mapply(c, statements, periods.terminating.statements, SIMPLIFY = FALSE)
              indx.statements <- lapply(ranges.statements, function(i) {
                seq(from = i[1], to = i[2])
              })
              cleaned <- text[-unlist(indx.statements)]
            } else {
              cleaned <- text
            }
            exe.cache.recode.statements <- grep(pattern = "^exe[[:alpha:]]*\\.|^cache\\.|^recode", x = cleaned, ignore.case = TRUE)
            if(length(exe.cache.recode.statements) > 0) {
              cleaned <- cleaned[-exe.cache.recode.statements]
            }
            save.statements <- grep(pattern = "sav[[:alpha:]]*\\s+out[[:alpha:]]*", x = cleaned, ignore.case = TRUE)
            if(length(save.statements) > 0) {
              cleaned <- cleaned[-save.statements]
            }
            return(cleaned)
          }
          tmp.value.labels <- clean.unwanted.statements(text = value.labels.part, expression = "mis[[:alpha:]]*\\s+val[[:alpha:]]*")
          tmp.value.labels <- clean.unwanted.statements(text = tmp.value.labels, expression = "formats*\\s+[[:alnum:]]+")
          tmp.value.labels <- clean.unwanted.statements(text = tmp.value.labels, expression = "var[[:alpha:]]*\\s+lev[[:alpha:]]*")
          single.periods <- which(tmp.value.labels == ".")
          if(length(tmp.value.labels[single.periods]) != 0) {
            tmp.value.labels <- tmp.value.labels[-single.periods]
          }
          indx.ending.slashes <- grep(pattern = "/$", x = tmp.value.labels)
          if(length(indx.ending.slashes) > 0) {
            indx.move.end.slash <- indx.ending.slashes + 1
            tmp.value.labels[indx.move.end.slash] <- paste("/", tmp.value.labels[indx.move.end.slash], sep = "")
            tmp.value.labels[indx.ending.slashes] <- gsub(pattern = "/$", replacement = "", x = tmp.value.labels[indx.ending.slashes])
          }
          lab.statements.to.split <- grep(pattern = "Missing /", x = tmp.value.labels)
          if(length(lab.statements.to.split) > 0){
            add.split <- gsub(pattern = " /", replacement = " split /", x = tmp.value.labels[lab.statements.to.split])
            splitted.statements <- strsplit(x = add.split, split = " split ")
            tmp.value.labels[lab.statements.to.split] <- splitted.statements
            tmp.value.labels <- unlist(tmp.value.labels)
          }
          tmp.value.labels <- gsub(pattern = "xxxn", replacement = "", x = tmp.value.labels, ignore.case = TRUE)
          mixed.val.lab.var.names <- grep(pattern = "val[[:alpha:]]* lab[[:alpha:]]* [[:alnum:]]+", x = tmp.value.labels, ignore.case = TRUE)
          if(length(mixed.val.lab.var.names) != 0) {
            mixed.val.lab.var.names.pos <- lapply(tmp.value.labels[mixed.val.lab.var.names], function(i) {
              gregexpr(pattern = "\\s", text = i)[[1]][2]
            })
            max.char.mixed.lab.var.names <- lapply(X = tmp.value.labels[mixed.val.lab.var.names], nchar)
            val.lab.keywords <- str_trim(substring(text = tmp.value.labels[mixed.val.lab.var.names], first = 1, last = mixed.val.lab.var.names.pos), side = "both")
            var.names.and.labels <- str_trim(substring(text = tmp.value.labels[mixed.val.lab.var.names], first = mixed.val.lab.var.names.pos, last = max.char.mixed.lab.var.names), side = "both")
            split.statements <- Map(c, val.lab.keywords, var.names.and.labels)
            tmp.value.labels[mixed.val.lab.var.names] <- split.statements
            tmp.value.labels <- unlist(tmp.value.labels)
          }
          value.labels.next.pos <- grep(pattern = "val[[:alpha:]]* lab[[:alpha:]]*", x = tmp.value.labels, ignore.case = TRUE) + 1
          no.slash <- value.labels.next.pos[grep(pattern = "^/", x = tmp.value.labels[value.labels.next.pos], invert = TRUE)]
          tmp.value.labels[no.slash] <- paste("/", tmp.value.labels[no.slash], sep = "")
          tmp.value.labels <- tmp.value.labels[-grep(pattern = "val[[:alpha:]]* lab[[:alpha:]]*", x = tmp.value.labels, ignore.case = TRUE)]
          last.labels <- grep(pattern = "^/[[:alnum:]]+", x = tmp.value.labels) - 1
          tmp.value.labels[last.labels] <- gsub(pattern = "\\.$", replacement = "", x = tmp.value.labels[last.labels], ignore.case = TRUE)
          tmp.value.labels <- paste(tmp.value.labels, " split ", sep = "")
          tmp.value.labels[length(tmp.value.labels)] <- gsub(pattern = " split ", replacement = "", x = tmp.value.labels[length(tmp.value.labels)])
          indx.var.labels.lines <- grep(pattern = "^/|^[[:upper:]]{4, }|^[[:alpha:]]+[[:digit:]]+[[:alpha:]]", x = tmp.value.labels)
          indx.var.names <- split(indx.var.labels.lines, cumsum(c(TRUE, diff(indx.var.labels.lines) != 1)))
          indx.sequences <- indx.var.names[which(sapply(indx.var.names, length) > 1)]
          if(length(indx.sequences) == 0) {
            tmp.value.labels <- gsub(pattern = " split ", replacement = "", x = tmp.value.labels)
            tmp.value.labels <- str_trim(string = tmp.value.labels, side = "both")
          } else {
            indx.starting.slash <- grep(pattern = "^/", x = tmp.value.labels)
            tmp.value.labels[indx.starting.slash] <- paste("split1", tmp.value.labels[indx.starting.slash], sep = "")
            indx.remove.split <- unlist(lapply(X = indx.sequences, FUN = function(i) {
              head(i, n = length(i) - 1)
            }))
            tmp.value.labels[indx.remove.split] <- gsub(pattern = " split ", replacement = "", x = tmp.value.labels[indx.remove.split])
            pasted.value.labels <- paste(tmp.value.labels, sep = "", collapse = " ")
            tmp.value.labels <- unlist(str_split(string = pasted.value.labels, pattern = " split "))
            tmp.value.labels <- unlist(str_split(string = tmp.value.labels, pattern = "split1"))
            tmp.value.labels <- str_trim(string = tmp.value.labels, side = "both")
            tmp.value.labels <- tmp.value.labels[tmp.value.labels != ""]
          }
          mixed.statements <- grep(pattern = "^/(\\w+\\s)+([[:upper:]]\\s|[[:digit:]]+\\s)", x = tmp.value.labels)
          extract.and.split.labels <- function(vec, lst1, lst2) {
            substring(lapply(vec, function(i) i), first = lapply(lst1, function(i) i), last = lapply(lst2, function(i) i))
          }
          if(length(mixed.statements) != 0) {
            mixed.statements.pos.and.lengt <- gregexpr(pattern = "\\s[[:upper:]]\\s|\\s[[:digit:]]+\\s([^000]|[0-9]+\\%|[1-9]+[[:alpha:]]|[0-9]+\\-[0-9]+)", text = tmp.value.labels[mixed.statements])
            mixed.statements.pos <- lapply(mixed.statements.pos.and.lengt, function(i) {
              as.vector(i)
            })
            extract.var.names <- str_trim(substring(text = tmp.value.labels[mixed.statements], first = 1, last = sapply(mixed.statements.pos, function(i) i[1])), side = "both")
            split.end.pos <- lapply(X = mixed.statements.pos, FUN = function(i) {
              if(length(i) > 1) {
                i[2:length(i)] - 1
              } else if(length(i) == 1) {
                NULL
              }
            })
            split.last.pos <- lapply(X = tmp.value.labels[mixed.statements], FUN = function(i) {
              nchar(i)
            })
            split.end.pos <- mapply(c, split.end.pos, split.last.pos, SIMPLIFY = FALSE)
            extract.val.labels <- Map(extract.and.split.labels, tmp.value.labels[mixed.statements], mixed.statements.pos, split.end.pos)
            join.names.and.labels <- Map(c, as.list(extract.var.names), as.list(extract.val.labels))
            tmp.value.labels[mixed.statements] <- join.names.and.labels
          }
          tmp.value.labels <- str_trim(string = unlist(tmp.value.labels), side = "both")
          tmp.value.labels <- gsub(pattern = "\\s*/*\\.$", replacement = "", x = tmp.value.labels)
          mixed.labels <- grep(pattern = "^[[:digit:]]+\\s([[:alpha:]]+/*)+\\s[[:digit:]]+\\s[[:alpha:]]", x = tmp.value.labels)
          statements.to.escape <- c("1 pwk & 1 pmn",
                                    "[[:digit:]]+\\s*(hours|minutes)* and [[:digit:]]+\\s*(hours|minutes)",
                                    "times a year",
                                    "6342 QR 50000 or more - less than QR 750000",
                                    "6343 QR 75000 or more - less than QR 100000",
                                    "6344 QR 100000 or more - less than QR 125000",
                                    "6345 QR 125000 or more - less than QR 150000",
                                    "6346 QR 150000 or more",
                                    "2082 DKK 1 or more - less than DKK 500",
                                    "2083 DKK 501 or more - less than DKK 1.000",
                                    "6202 from 0 euros to 75 euros",
                                    "6203 from 75 euros to 3 999 euros",
                                    "6342 QR 500 or more - less than QR1000",
                                    "6346 QR 16000 or more",
                                    "QR 1000 or more - less than QR 6000",
                                    "QR 6000 or more - less than QR 11000",
                                    "QR 11000 or more - less than QR 16000",
                                    "QR 16000 or more",
                                    "YTL 600 or more - less than YTL 1200",
                                    "YTL 1200 or more - less than YTL 5000",
                                    "YTL 5000 or more - less than YTL 15000",
                                    "YTL 15000 or more",
                                    "QR 50000 or more - less than QR 75000",
                                    "YTL 6000 or more - less than YTL 12000",
                                    "YTL 12000 or more - less than YTL 24000",
                                    "YTL 24000 or more - less than YTL 48000",
                                    "YTL 48000 or more - less than YTL 72000",
                                    "YTL 72000 or more")
          unwanted.pattern <- grep(pattern = paste(statements.to.escape, collapse = "|"), x = tmp.value.labels)
          mixed.labels <- setdiff(mixed.labels, unwanted.pattern)
          if(length(mixed.labels) != 0) {
            mixed.labels.pos <- gregexpr(pattern = "\\s[[:digit:]]", text = tmp.value.labels[mixed.labels])
            mixed.labels.pos <- lapply(mixed.labels.pos, function(i) {
              as.vector(i)
            })
            length.mixed.labels <- lapply(tmp.value.labels[mixed.labels], nchar)
            all.split.points <- Map(c, 1, mixed.labels.pos, length.mixed.labels)
            mix.lab.start.positions <- lapply(X = all.split.points, FUN = function(i) {
              sort(c(i[seq(1, length(i), 2)], i[seq(2, length(i) - 1, 2)]))
            })
            mix.lab.end.positions <- lapply(X = all.split.points, FUN = function(i) {
              tail(i, n = length(i) - 1)
            })
            split.labels <- Map(extract.and.split.labels, tmp.value.labels[mixed.labels], mix.lab.start.positions, mix.lab.end.positions)
            tmp.value.labels[mixed.labels] <- split.labels
            tmp.value.labels <- str_trim(unlist(tmp.value.labels), side = "both")
          }
          value.labels.var.names.pos <- grep(pattern = "^/", x = tmp.value.labels)
          value.labels.start.pos <- value.labels.var.names.pos + 1
          value.labels.end.pos <- (value.labels.var.names.pos - 1)
          value.labels.end.pos[length(value.labels.end.pos) + 1] <- length(tmp.value.labels)
          value.labels.end.pos <- value.labels.end.pos[-1]
          form.statements <- function(string.object) {
            if(length(grep(pattern = "^/", x = string.object)) == 0) {
              obj.var.names <- string.object
            } else {
              obj.var.names <- toupper(sub(pattern = "^/", x = grep(pattern = "^/", x = string.object, value = TRUE), replacement = ""))
            }
            indx.elem.list <- lapply(gregexpr(" TO ", obj.var.names), function(component){
              attributes(component) <- NULL
              ifelse(component == -1, component <- 0, component)
              component
            })
            all.spaces <- gregexpr("\\s+", obj.var.names)
            matched.space.pos <- mapply(match, indx.elem.list, all.spaces)
            previous.string.pos <- lapply(matched.space.pos, function(x) x - 1)
            next.string.pos <- lapply(matched.space.pos, function(x) x + 1)
            previous.string <- lapply(seq_along(all.spaces), function(i) all.spaces[[i]][previous.string.pos[[i]]])
            next.string <- lapply(seq_along(all.spaces), function(i) all.spaces[[i]][next.string.pos[[i]]])
            ranges.var.list <- lapply(1:length(previous.string), function(i){
              xx <- strsplit(x = obj.var.names, split = "")[[i]]
              xx[next.string[[i]]] <- " which(names(tmp)=='"
              xx <- gsub(pattern = " TO ", replacement = ":", x = xx)
              xx <- gsub(pattern = " TO ", replacement = "'):", x = paste(xx, sep = " ", collapse = ""))
              xx <- gsub(pattern = " ", replacement = "' ", x = xx)
              xx <- lapply(X = xx, FUN = function(j) {
                if(str_count(string = j, pattern = "\\S+") == 1) {
                  paste("names(tmp['", j, "'])", sep = "")
                } else {
                  paste("names(tmp[ , c(which(names(tmp)=='", gsub(pattern = " ", replacement = "), which(names(tmp)=='", x = paste(xx, sep = " ", collapse = "")), "'))])", sep = "")
                }
              })
              xx <- unlist(xx)
            })
            number.of.single.quotes <- lapply(ranges.var.list, FUN = function(i) {
              x <- str_count(string = i, pattern = "'")
              return(x)
            })
            indx.four.quotes <- which(number.of.single.quotes == 4)
            indx.colons <- lapply(ranges.var.list, function(i) {
              grep(pattern = ":", i)
            })
            indx.single.colon <- which(indx.colons == 1)
            single.colon <- intersect(indx.four.quotes, indx.single.colon)
            if(length(single.colon) > 0) {
              ranges.var.list[single.colon] <- gsub(pattern = "names(tmp[", replacement = "names(tmp[which(names(tmp)==", x = ranges.var.list[single.colon], fixed = TRUE)
              ranges.var.list[single.colon] <- gsub(pattern = "'])", replacement = "')])", x = ranges.var.list[single.colon], fixed = TRUE)
              ranges.and.prev.string <- list(ranges.var.list = ranges.var.list, previous.string = previous.string, obj.var.names = obj.var.names)
            } else {
              ranges.and.prev.string <- list(ranges.var.list = ranges.var.list, previous.string = previous.string, obj.var.names = obj.var.names)
            }
            return(ranges.and.prev.string)
          }
          ranges.and.prev.pos <- form.statements(tmp.value.labels)
          value.labels.ranges.var.list <- ranges.and.prev.pos[["ranges.var.list"]]
          previous.string <- ranges.and.prev.pos[["previous.string"]]
          value.labels.var.names <- ranges.and.prev.pos[["obj.var.names"]]
          value.labels.full.var.list <- lapply(value.labels.ranges.var.list, function(i) {eval(parse(text = i))})
          value.labels.block.ranges <- paste(value.labels.start.pos, value.labels.end.pos, sep = ":", collapse = " ")
          value.labels.ranges.expr <- unlist(lapply(X = 1:str_count(string = value.labels.block.ranges, pattern = "\\S+"), FUN = function(i) {
            paste("value.labels.block", i, " <- tmp.value.labels[", word(string = value.labels.block.ranges, start = i, end = i), "]", sep = "")
          }))
          eval(parse(text = value.labels.ranges.expr))
          list.value.labels.blocks <- paste(ls(pattern = "value.labels.block[[:digit:]]+"), collapse = ", ")
          list.value.labels.blocks.names <- paste(ls(pattern = "value.labels.block[[:digit:]]+"))
          eval(parse(text = paste("value.labels.blocks <- list(", list.value.labels.blocks, ")", sep = "")))
          names(value.labels.blocks) <- list.value.labels.blocks.names
          value.labels.blocks <- value.labels.blocks[order(nchar(names(value.labels.blocks)), names(value.labels.blocks))]
          value.labels.blocks <- lapply(X = value.labels.blocks, gsub, pattern = paste("^00+\\s+", collapse = "|"), replacement = "0 ")
          value.labels.blocks <- lapply(X = value.labels.blocks, FUN = function(i) {
            sub("^0+([1-9])", "\\1", x = i)
          })
          concat.value.labels.blocks <- lapply(value.labels.blocks, function(i) {
            paste(i, collapse = " ")
          })
          blocks.with.just.miss <- c("^7 N/A 8 Invalid 9 Missing r Not reached$",
                                     "^7 N/A 8 Invalid r Not reached$",
                                     "^7 N/A 8 M/R 9 Mis",
                                     "^7 N/A 9 Missing r Not reached$",
                                     "^7 N/A r Not reached$",
                                     "^7+ N/A 8+ Invalid 9+ Missing$",
                                     "^7+ N/A 8+ Invalid$",
                                     "^8 Invalid r Not reached$",
                                     "^9 Missing r Not reached$",
                                     "^9*7 N/A 9*8 Invalid 9+ Miss.*$",
                                     "^9*7 N/A 9*8 Invalid$",
                                     "^9*7 N/A 9*8 M/R 9+ Missing$",
                                     "^9*7 N/A 9+ Mis$",
                                     "^9*7 N/A 9+ Missing 9*8 Invalid$",
                                     "^9*7 N/A 9+ Missing$",
                                     "^9*7 N/A$",
                                     "^9*8 Invalid 9*7 N/A 9+ Missing$",
                                     "^9*8 Invalid 9*7 N/A$",
                                     "^9*8 Invalid 9+ Missing$",
                                     "^9*8 Invalid$",
                                     "^9+ Missing 9*7 N/A$",
                                     "^9+ Missing 9*8 Invalid$",
                                     "^9+ Missing$",
                                     "^9+7 N/A 9+ Miss$",
                                     "^96 Ungraded 97 N/A 98 Invalid 99 Miss$",
                                     "^96 Ungraded 98 Invalid 99 Miss$",
                                     "^96 Ungraded 98 Invalid 99 Missing$",
                                     "^96 Ungraded 99 Missing$",
                                     "^996 N/A - <ISCED level> not available in this school 9997 N/A 9998 Invalid 9999 Miss$",
                                     "^996 N/A 9997 N/A 9998 Invalid 9999 Missing$",
                                     "^997 Missing$")
          indx.numeric <- unique(grep(paste(blocks.with.just.miss, collapse = "|"), concat.value.labels.blocks))
          if(length(indx.numeric) != 0) {
            tmp[ , unlist(unique(value.labels.full.var.list[indx.numeric]))] <- lapply(X = unlist(unique(value.labels.full.var.list[indx.numeric])), FUN = function(x) {
              as.numeric(as.character(tmp[ , x]))
            })
          }
          get.orig.data.values <- function(data, var.list) {
            component.names <- unlist(var.list)
            unique.values <- lapply(data[component.names], unique)
            unique.values <- lapply(unique.values, sort)
            unique.values <- lapply(unique.values, as.character)
            return(unique.values)
          }
          orig.data.values <- get.orig.data.values(data = tmp, var.list = value.labels.full.var.list)
          character.columns <- names(Filter(is.character, tmp))
          components.to.clean <- character.columns[is.element(character.columns, names(orig.data.values))]
          orig.data.values[components.to.clean] <- lapply(X = orig.data.values[components.to.clean], FUN = function(i) {
            sub("^0+([1-9])", "\\1", x = i)
          })
          orig.data.values.in.blocks <- lapply(X = value.labels.full.var.list, FUN = function(i) {
            values <- sort(unique(unlist(orig.data.values[i], use.names = FALSE)))
            duplicated.values <- paste(values, values)
            return(duplicated.values)
          })
          duplicated.data.values.in.blocks <- mapply(FUN = c, value.labels.blocks, orig.data.values.in.blocks, SIMPLIFY = FALSE)
          value.labels.blocks <- lapply(X = duplicated.data.values.in.blocks, FUN = function(i) {
            i[!duplicated(word(string = i, start = 1, end = 1))]
          })
          value.labels.blocks <- lapply(X = value.labels.blocks, FUN = sort)
          list.value.LEVELS <- lapply(X = value.labels.blocks, FUN = function(element) {
            word(string = element, start = 1, end = 1)
          })
          list.value.LABELS <- lapply(X = value.labels.blocks, FUN = function(element) {
            make.unique(gsub(pattern = "^\\w*[[:punct:]]*\\w*\\s*|^\\w*\\s*", replacement = "", x = element))
          })
          if(length(indx.numeric) != 0) {
            value.labels.blocks <- value.labels.blocks[-indx.numeric]
            value.labels.full.var.list <- value.labels.full.var.list[-indx.numeric]
            previous.string <- previous.string[-indx.numeric]
            value.labels.var.names <- value.labels.var.names[-indx.numeric]
            list.value.LEVELS <- list.value.LEVELS[-indx.numeric]
            list.value.LABELS <- list.value.LABELS[-indx.numeric]
          }
          value.lab.statement.left <- lapply(1:length(previous.string), function(i){
            if(str_count(string = value.labels.var.names[[i]], pattern = "\\S+") == 1) {
              xx <- strsplit(value.labels.var.names, "")[[i]]
              xx <- paste("tmp[ , '", paste(value.labels.full.var.list[[i]], sep = "", collapse = "', '"), "']", sep = "")
            } else {
              xx <- strsplit(value.labels.var.names, "")[[i]]
              xx <- paste("tmp[ , c('", paste(value.labels.full.var.list[[i]], sep = "", collapse = "', '"), "')]", sep = "")
            }
          })
          value.lab.statement.right <- lapply(1:length(previous.string), function(i){
            if(str_count(string = value.labels.var.names[[i]], pattern = "\\S+") == 1) {
              xx <- strsplit(value.labels.var.names, "")[[i]]
              xx <- paste("factor(tmp[ , '" , paste(value.labels.full.var.list[[i]], sep = "", collapse = "', '"), "'], levels = c('", paste(list.value.LEVELS[[i]], sep = "", collapse = "', '"), "')", ", labels = c('", paste(list.value.LABELS[[i]], sep = "'", collapse = "', '"), "'))", sep = "")
            } else {
              xx <- strsplit(value.labels.var.names, "")[[i]]
              xx <- paste("lapply(tmp[ , c('" , paste(value.labels.full.var.list[[i]], sep = "", collapse = "', '"), "')], factor, levels = c('", paste(list.value.LEVELS[[i]], sep = "", collapse = "', '"), "')", ", labels = c('", paste(list.value.LABELS[[i]], sep = "'", collapse = "', '"), "'))", sep = "")
            }
          })
          value.labels.statements <- paste(value.lab.statement.left, "<-", value.lab.statement.right)
          if(txt.file.name == "INT_stui_2003_v2") {
            val.lab.statements.to.remove <- c("\\'ST17Q14\\'], levels = c\\(\\'1\\', \\'20301\\', \\'27601\\',",
                                              "\\'ST17Q15\\'], levels = c\\(\\'1', \\'20301\\', \\'2\\', \\'30001\\',",
                                              "\\'ST17Q16\\'], levels = c\\(\\'1\\', \\'2\\', \\'30001\\', \\'30002\\',")
            indx.statements.to.remove <- grep(pattern = paste(val.lab.statements.to.remove, collapse = "|"), x = value.labels.statements)
            value.labels.statements <- value.labels.statements[-indx.statements.to.remove]
          }
          eval(parse(text = value.labels.statements))
          indx.char.cols.to.numeric <- function(data) {
            indx.character.columns <- sapply(X = data, FUN = function(x) {
              grepl(pattern = "character", x = class(x))
            })
            indx.uppercase.columns <- sapply(X = data, FUN = function(x) {
              all(grepl(pattern = "[[:upper:]]{3}", x = x))
            })
            indx.character.columns <- indx.character.columns[-which(indx.uppercase.columns == TRUE)]
            indx.mixed.columns <- sapply(X = data[indx.character.columns], FUN = function(x){
              any(grepl(pattern = "[a-zA-Z]+[0-9]+|[0-9]+[a-zA-Z]+", x = x) == TRUE)
            })
            indx.sentences.columns <- sapply(data[indx.character.columns], function(x) {
              any(str_count(string = x, pattern = "\\S+") > 5)
            })
            indx.cols.with.levels <- sapply(data, function(i) {
              indx.not.NULL <- length(attr(i, "levels"))
            })
            indx.cols.with.levels <- names(indx.cols.with.levels[indx.cols.with.levels > 0])
            indx.all.levels.num.values <- sapply(data[indx.cols.with.levels], function(i) {
              any(grep(pattern = "[[:alpha:]]", x = attr(x = i, which = "levels")), na.rm = TRUE)
            })
            character.columns <- names(indx.character.columns[indx.character.columns == TRUE])
            mixed.columns <- names(indx.mixed.columns[indx.mixed.columns == TRUE])
            sentences.columns <- names(indx.sentences.columns[indx.sentences.columns == TRUE])
            numeric.columns <- names(indx.all.levels.num.values[indx.all.levels.num.values == FALSE])
            preserved.columns <- c(mixed.columns, sentences.columns)
            c(setdiff(character.columns, preserved.columns), numeric.columns)
          }
          indx.convert.to.num <- indx.char.cols.to.numeric(tmp)
          tmp[ , indx.convert.to.num] <- lapply(X = indx.convert.to.num, FUN = function(x) {
            as.numeric(as.character(tmp[ , x]))
          })
          character.columns <- names(which(sapply(tmp, function(i) {class(i) == "character"})))
          tmp[character.columns] <- lapply(tmp[character.columns], function(i) {
            if(all(grepl(pattern = "^[[:upper:]]{3}$", x = i)) == TRUE) {
              as.factor(i)
            } else {
              as.character(i)
            }
          })
          filter.factor.vars.with.missing <- function(data) {
            var.levels <- lapply(X = data, FUN = function(i) {
              var.levels <- levels(i)
            })
            length.of.0 <- sapply(X = var.levels, FUN = length)
            vars.with.missing.levels <- var.levels[length.of.0 != 0]
            return(vars.with.missing.levels)
          }
          vars.with.missings <- names(filter.factor.vars.with.missing(tmp))
          missing.patterns <- c("N/A",
                                "Invalid",
                                "Missing",
                                "Miss",
                                "Mis",
                                "Not reached",
                                "N/A, Yes: Not administered",
                                "N/A, N/A: Not administered",
                                "Not administered",
                                "Unreached",
                                "Ungraded",
                                "M/R",
                                "99999970",
                                "99999990",
                                "9999997",
                                "9999998",
                                "9999999")
          fac.missing.levels.extract <- function(data, patterns) {
            missing.values.indx <- lapply(X = data, FUN = function(i) {
              grep(pattern = paste(patterns, collapse = "|"), x = levels(i))
            })
            length.of.0 <- sapply(X = missing.values.indx, FUN = length)
            vars.with.missing.levels <- missing.values.indx[length.of.0 != 0]
            var.names.with.missing.levels <- names(vars.with.missing.levels)
            var.levels <- lapply(data[var.names.with.missing.levels], function(i) {
              number.of.levels <- nlevels(i)
              factor.levels <- levels(i)
              match.factor.levels <- vector(mode = "character", length = number.of.levels)
              names(match.factor.levels) <- factor.levels
              match.factor.levels[1:number.of.levels] <- 1:number.of.levels
              return(match.factor.levels)
            })
            extract <- lapply(var.levels, function(i) {
              indx.differences <- names(i) %in% patterns
              difference <- i[indx.differences == TRUE]
            })
            flatten <- lapply(X = extract, FUN = function(i) {names(unlist(i))})
            return(flatten)
          }
          missing.levels.list.factors <- fac.missing.levels.extract(data = tmp, patterns = missing.patterns)
          missings.part <- spss.syntax[grep(pattern = "mis[[:alpha:]]*\\sval[[:alpha:]]*\\s*", x = spss.syntax, ignore.case = TRUE)[1]:length(spss.syntax)]
          missing.keywords <- grep(pattern = "mis[[:alpha:]]*\\sval[[:alpha:]]*\\s*", x = missings.part, ignore.case = TRUE)
          ending.periods <- grep(pattern = "([[:digit:]]+|[[:alpha:]]+)\\s*\\)\\s*\\.$", x = missings.part)
          if(length(missing.keywords) == 1 && length(ending.periods) == 0) {
            ending.period <- grep(pattern = "\\.", x = missings.part)
            missings.part <- missings.part[missing.keywords:ending.period]
            missings.part <- missings.part[-grep(pattern = "save outfile", x = missings.part)]
            missings.part <- gsub(pattern = "^/", replacement = "missing values ", x = missings.part)
          } else {
            first.period.larger.than.missing <- sapply(missing.keywords, function(i) head(ending.periods[ending.periods >= i], 1))
            missings.part <- missings.part[unlist(Map(`:`, missing.keywords, first.period.larger.than.missing))]
            missings.part <- gsub(pattern = "^/", replacement = "missing values ", x = missings.part)
            missings.part <- gsub(pattern = "\\s+\\)", replacement = ")", x = missings.part)
            missings.part <- gsub(pattern = "\\(\\s+", replacement = "(", x = missings.part)
            missings.part <- gsub(pattern = "\\s+\\.", replacement = ".", x = missings.part)
          }
          closing.brackets.no.period <- grep(pattern = "\\)$", x = missings.part)
          if(length(closing.brackets.no.period) > 0) {
            missings.part[closing.brackets.no.period] <- paste0(missings.part[closing.brackets.no.period], ".")
          }
          if(all(substring(text = missings.part, first = nchar(x = missings.part), last = nchar(x = missings.part)) == ".") == FALSE) {
            periods <- grep(pattern = "\\.", x = missings.part)
            missing.statements.collapsed <- paste(missings.part, collapse = " ")
            missing.values.statements <- unlist(strsplit(missing.statements.collapsed, split = ".", fixed = TRUE))
            missing.values.statements <- toupper(gsub(pattern = "\\s*mis[[:alpha:]]* val[[:alpha:]]* ", replacement = "", x = grep(pattern = "mis[[:alpha:]]* val[[:alpha:]]*", x = missing.values.statements, ignore.case = TRUE, value = TRUE), ignore.case = TRUE))
          } else {
            missing.values.statements <- gsub(pattern = "\\.", replacement = "", x = missings.part)
            missing.values.statements <- toupper(gsub(pattern = "\\s*mis[[:alpha:]]* val[[:alpha:]]* ", replacement = "", x = grep(pattern = "mis[[:alpha:]]* val[[:alpha:]]*", x = missing.values.statements, ignore.case = TRUE, value = TRUE), ignore.case = TRUE))
          }
          if(length(grep(pattern = " TO ", x = missing.values.statements)) > 0) {
            statements.with.var.ranges <- grep(pattern = " TO ", missing.values.statements)
            var.ranges <- sapply(X = str_split(string = missing.values.statements[statements.with.var.ranges], pattern = "\\s\\("), function(i) {
              i[1]
            })
            missing.values <- sapply(X = str_split(string = missing.values.statements[statements.with.var.ranges], pattern = "\\s\\("), function(i) {
              paste("(", i[2], sep = "")
            })
            list.of.variables <- lapply(form.statements(var.ranges)[["ranges.var.list"]], function(i) {eval(parse(text = i))})
            formed.var.list.w.miss.values <- lapply(X = Map(c, list.of.variables, missing.values), FUN = function(i) {
              paste(i, collapse = " ")
            })
            missing.values.statements[statements.with.var.ranges] <- formed.var.list.w.miss.values
            missing.values.statements <- unlist(missing.values.statements)
          }
          missing.values.statements <- gsub(pattern = ", ", replacement = ",", x = missing.values.statements)
          missing.values.statements <- gsub(pattern = " THRU ", replacement = "THRU", x = missing.values.statements)
          miss.var.names <- sapply(as.list(missing.values.statements), function(i) {
            word(string = i, start = 1, end = str_count(string = i, pattern = "\\S+") - 1)
          })
          miss.val.lists <- sapply(as.list(missing.values.statements), function(i) {
            list.miss <- word(string = i, start = str_count(string = i, pattern = "\\S+"), end = str_count(string = i, pattern = "\\S+"))
            gsub(pattern = ",", replacement = ", ", x = list.miss)
            gsub(pattern = "THRU", replacement = " THRU ", x = list.miss)
          })
          split.var.names <- str_split(string = miss.var.names, pattern = " ")
          indx.TO.statements <- which(lapply(X = split.var.names, FUN = function(i) {
            "TO" %in% i
          }) == TRUE)
          split.var.names[indx.TO.statements] <- lapply(X = split.var.names[indx.TO.statements], FUN = paste, collapse = " ")
          missing.values.statements <- unlist(mapply(FUN = paste, split.var.names, as.list(miss.val.lists)))
          missing.values.statements <- gsub(pattern = "9997 THRU 9999", replacement = "9997, 9998, 9999", x = missing.values.statements)
          missing.values.statements <- gsub(pattern = "9997 THRU 9999, 996", replacement = "996, 9997, 9998, 9999", x = missing.values.statements)
          missing.values.statements <- gsub(pattern = "96 THRU 99", replacement = "96, 97, 98, 99", x = missing.values.statements)
          missing.values.statements <- gsub(pattern = "\\s+\\)", replacement = ")", x = missing.values.statements)
          missing.values.statements <- gsub(pattern = "\\(\\s+", replacement = "(", x = missing.values.statements)
          indx.end.var.names <- unlist(gregexpr(pattern = " \\(", text = missing.values.statements))
          var.names.missing.statements <- word(string = missing.values.statements, start = 1, end = indx.end.var.names, sep = "")
          var.names.missing.statements <- unlist(strsplit(x = var.names.missing.statements, split = " "))
          num.vars.with.missings.names <- var.names.missing.statements[is.na(match(var.names.missing.statements,vars.with.missings))]
          if(length(num.vars.with.missings.names) == 0) {
            missing.levels.list <- missing.levels.list.factors
          } else {
            num.vars.with.missings.indx <- is.element(el = word(string = missing.values.statements, start = 1, end = indx.end.var.names, sep = ""), set = num.vars.with.missings.names)
            num.vars.with.missings.raw <- missing.values.statements[num.vars.with.missings.indx == TRUE]
            indx.end.var.names.raw <- unlist(gregexpr(pattern = "\\(", text = num.vars.with.missings.raw)) + 1
            indx.end.var.label.statements.raw <- unlist(gregexpr(pattern = "\\)", text = num.vars.with.missings.raw))
            missing.levels.list.numeric <- lapply(X = strsplit(x = word(string = num.vars.with.missings.raw, start = indx.end.var.names.raw, end = indx.end.var.label.statements.raw, sep = ""), split = ",", fixed = TRUE), FUN = function(i) {
              x <- sort(i)
              y <- as.numeric(x)
              return(y)
            })
            names(missing.levels.list.numeric) <- num.vars.with.missings.names
            missing.levels.list <- c(missing.levels.list.factors, missing.levels.list.numeric)
          }
          assign.attributes <- function(data, attr.name, attr.list, to.na = c("yes", "no", "ignore")) {
            if(attr.name == "missings" & to.na == "yes") {
              common.vars <- intersect(names(data), names(attr.list))
              factor.vars.logical <- lapply(data[common.vars], function(i) {class(i) == "factor"})
              numeric.vars.logical <- lapply(data[common.vars], function(i) {class(i) == "numeric"})
              names.factor.vars <- names(factor.vars.logical[factor.vars.logical == TRUE])
              names.numeric.vars <- names(numeric.vars.logical[numeric.vars.logical == TRUE])
              missing.levels.factor <- attr.list[names.factor.vars]
              missing.values.numeric <- attr.list[names.numeric.vars]
              statements.factor <- sapply(X = names(missing.levels.factor), FUN = function(k) {
                if(length(missing.levels.factor[[k]]) > 1) {
                  paste0("tmp[['", k, "']][tmp[['", k, "']] %in% ", missing.levels.factor[k], "] <- NA")
                } else if(length(missing.levels.factor[[k]]) == 1) {
                  paste0("tmp[['", k, "']][tmp[['", k, "']] == '", missing.levels.factor[k], "'] <- NA")
                }
              })
              statements.numeric <- sapply(X = names(missing.values.numeric), FUN = function(k) {
                if(length(missing.values.numeric[[k]]) > 1) {
                  paste0("tmp[['", k, "']][tmp[['", k, "']] %in% ", missing.values.numeric[k], "] <- NA")
                } else if(length(missing.values.numeric[[k]]) == 1) {
                  paste0("tmp[['", k, "']][tmp[['", k, "']] == ", missing.values.numeric[k], "] <- NA")
                }
              })
              statements <- c(statements.factor, statements.numeric)
              statements <- Filter(Negate(is.null), statements)
            } else if(attr.name == "missings" & to.na == "no") {
              var.names <- names(attr.list)
              statements.left <- paste("attr(", deparse(substitute(data)), "[['", var.names, "']], '", attr.name, "') <- ", sep = "")
              statements.right <- lapply(X = attr.list, FUN = function(i) {
                if(is.character(i) == TRUE) {
                  paste("c('", paste(i, collapse = "' ,'"), "')", sep = "")
                } else if(is.numeric(i) == TRUE) {
                  paste("c(", paste(i, collapse = ", "), ")", sep = "")
                }
              })
              statements <- paste(statements.left, statements.right, sep = "")
            } else if(attr.name == "var.labels" & to.na == "ignore") {
              statements <- paste("attr(tmp[['", names(var.labels), "']], 'variable.label') <- '", var.labels, "'", sep = "")
            }
            return(statements)
          }
          if(missing.to.NA == TRUE) {
            eval(parse(text = assign.attributes(data = tmp, attr.name = "missings", attr.list = missing.levels.list, to.na = "yes")))
            tmp <- droplevels(tmp)
          } else if(missing.to.NA == FALSE) {
            tmp <- droplevels(tmp)
            classes.missing.levels.list <- lapply(missing.levels.list, class)
            fac.vars.missing.levels.list <- names(classes.missing.levels.list[classes.missing.levels.list == "character"])
            missing.strings.fac <- lapply(tmp[fac.vars.missing.levels.list], function(i) {
              grep(pattern = paste(missing.patterns, collapse = "|"), x = levels(i), value = TRUE)
            })
            missing.strings.fac <- missing.strings.fac[lapply(missing.strings.fac, length) != 0]
            num.vars.missing.levels.list <- names(classes.missing.levels.list[classes.missing.levels.list == "numeric"])
            if(length(num.vars.missing.levels.list) > 0) {
              missing.strings.num <- missing.levels.list[num.vars.missing.levels.list]
              unique.values <- lapply(X = tmp[num.vars.missing.levels.list], FUN = function(i) (
                unique(i)
              ))
              which.missing.are.present <- function(list.of.missings, unique.data.values) {
                list.of.missings[which(list.of.missings %in% unique.data.values)]
              }
              presence.missing.strings.num <- mapply(FUN = which.missing.are.present, list.of.missings = missing.strings.num, unique.data.values = unique.values, SIMPLIFY = FALSE)
              indx.presence <- sapply(presence.missing.strings.num, function(i) {
                length(i) > 0
              })
              missing.strings.num <- presence.missing.strings.num[indx.presence]
              missing.levels.list.new <- c(missing.strings.fac, missing.strings.num)
            } else {
              missing.levels.list.new <- missing.strings.fac
            }
            eval(parse(text = assign.attributes(data = tmp, attr.name = "missings", attr.list = missing.levels.list.new, to.na = "no")))
            no.missing.attr <- lapply(tmp, function(i) {
              is.null(attr(i, "missings"))
            })
            no.missing.attr <- names(which(no.missing.attr == TRUE))
            missing.attr.as.NA <- as.list(rep(NA, times = length(no.missing.attr)))
            names(missing.attr.as.NA) <- no.missing.attr
            NA.miss.assign.statements <- assign.attributes(data = tmp, attr.name = "missings", attr.list = missing.attr.as.NA, to.na = "no")
            NA.miss.assign.statements <- gsub(pattern = "NULL", replacement = "NA", x = NA.miss.assign.statements)
            eval(parse(text = NA.miss.assign.statements))
          }
          var.labels.start <- grep(pattern = "var[[:alpha:]]* lab[[:alpha:]]*", x = spss.syntax, ignore.case = TRUE) + 1
          var.labels.end <- var.labels.start + (length(var.names) - 1)
          tmp.var.labels <- spss.syntax[var.labels.start:var.labels.end]
          periods.objects <- which(tmp.var.labels == ".")
          if(length(periods.objects) > 0) {
            tmp.var.labels <- tmp.var.labels[-which(tmp.var.labels == ".")]
          }
          tmp.var.labels.names <- toupper(word(tmp.var.labels, start = 1, end = 1))
          tmp.var.labels.content <- gsub(pattern = "^\\w*\\s*", replacement = "", x = tmp.var.labels)
          var.labels <- as.list(structure(.Data = tmp.var.labels.content, names = tmp.var.labels.names))
          var.labels <- lapply(X = var.labels, function(i) {
            gsub(pattern = "[[:space:]]\\.$|\\.$", replacement = "", x = i)
          })
          eval(parse(text = assign.attributes(data = tmp, attr.name = "var.labels", attr.list = var.labels, to.na = "ignore")))
          attr(x = tmp, which = "study") <- "PISA"
          if(txt.file.name %in% c("intcogn_v4", "intscho", "intstud_math_v3", "intstud_read_v3", "intstud_scie_v3")) {
            attr(x = tmp, which = "cycle") <- 2000
          } else if(txt.file.name %in% c("INT_cogn_2003_v2", "INT_schi_2003", "INT_stui_2003_v2")) {
            attr(x = tmp, which = "cycle") <- 2003
          } else if(txt.file.name %in% c("INT_Cogn06_S_Dec07", "INT_Cogn06_T_Dec07", "INT_Par06_Dec07", "INT_Sch06_Dec07", "INT_Stu06_Dec07")) {
            attr(x = tmp, which = "cycle") <- 2006
          } else if(txt.file.name %in% c("INT_COG09_S_DEC11", "INT_COG09_TD_DEC11", "INT_PAR09_DEC11", "INT_SCQ09_Dec11", "INT_STQ09_DEC11")) {
            attr(x = tmp, which = "cycle") <- 2009
          } else if(txt.file.name %in% c("INT_COG12_DEC03", "INT_COG12_S_DEC03", "INT_PAQ12_DEC03", "INT_SCQ12_DEC03", "INT_STU12_DEC03")) {
            attr(x = tmp, which = "cycle") <- 2012
          }
          if(txt.file.name %in% c("INT_STU12_DEC03", "INT_STQ09_DEC11", "INT_Stu06_Dec07", "INT_Par06_Dec07", "INT_stui_2003_v2", "intstud_math_v3", "intstud_read_v3", "intstud_scie_v3")) {
            attr(x = tmp, which = "file.type") <- "std.bckg"
          } else if(txt.file.name %in% c("INT_COG12_DEC03", "INT_COG09_DEC10", "INT_Cogn06_T_Dec07", "intcogn_v4")){
            attr(x = tmp, which = "file.type") <- "std.cogn"
          } else if(txt.file.name %in% c("INT_COG12_S_DEC03", "INT_COG09_Scored_DEC10", "INT_Cogn06_S_Dec07", "INT_cogn_2003_v2")) {
            attr(x = tmp, which = "file.type") <- "std.cogn.scr"
          } else if(txt.file.name %in% c("INT_PAQ12_DEC03", "INT_PAQ09_DEC10")) {
            attr(x = tmp, which = "file.type") <- "std.par.bckg"
          } else if(txt.file.name %in% c("INT_SCQ12_DEC03", "INT_SCQ09_Dec10", "INT_Sch06_Dec07", "INT_schi_2003", "intscho")) {
            attr(x = tmp, which = "file.type") <- "sch.bckg"
          }
          setDT(x = tmp, key = "CNT")
          attr(x = tmp, which = "class") <- c("lsa.data", attr(x = tmp, which = "class"))
          assign(x = tolower(new.file), value = tmp)
          tmp <- NULL
          full.out.file.path <- file.path(out.folder, paste(txt.file.name, "RData", sep = "."))
          save(list = tolower(new.file), file = out.file, compress = FALSE)
          message("       (", count.success, "/", length(txt.files), ")  ", str_pad(string = sub(pattern = "\\.sps", replacement = ".txt", x = basename(inp.file), ignore.case = TRUE), width = 40, side = "right"), " converted in ", format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm}[[3]] - 1, "%H:%M:%OS3"))
        },
        error = function(e) {
          message("")
        })
      }
      ptm <- proc.time()
      suppressWarnings(
        invisible(mapply(FUN = convert.PISApre15, inp.file = full.inp.file.path, out.file = full.out.file.path, new.file = root.file.names))
      )
      if(length(sps.files) > 0 & length(txt.files) > 0) {
        message("\n All ", count.success, " found files successfully converted in ", format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm}[[3]] - 1, "%H:%M:%OS3"), "\n\n")
      }
      if(length(sps.files) > length(txt.files)) {
        message("\nWarning:\nSome of the control (\".sps\") files have no matching data (\".txt\") file to convert. These files are skipped.\nCheck the content of the input folder.\n\n")
      }
      if(length(txt.files) > length(sps.files)) {
        message("\nWarning:\nSome of the data (\".txt\") files to convert have no matching control (\".sps\") files. These files were skipped.\nCheck the content of the input folder.\n\n")
      }
    }
  }, interrupt = function(f) {
    message("\n\nInterrupted by the user. Not all files have been converted.")
  },
  error = function(e) {
    message("")
  })
}
lsa.data <- function(x, ...) {
  NextMethod(x, ...)
}
#' @rdname lsa.convert.data
#' @export
print.lsa.data <- function(x, col.nums, ...) {
  if(missing(col.nums)) {
    col.nums <- colnames(x)[1:6]
  } else {
    if(is.numeric(col.nums)) {
      col.nums <- colnames(x)[col.nums]
    } else {
      col.nums <- col.nums
    }
  }
  col.nums <- na.omit(col.nums)
  attr(x = col.nums, which = "na.action") <- NULL
  print.obj <- names(which(sapply(X = ls(envir = parent.frame(2)), FUN = function(v) {
    identical(x, get(v, envir = parent.frame(2)))
  })))
  print.call <- match.call(expand.dots=FALSE)
  if(length(print.obj) != 0 & print.call != "print.lsa.data(x = x)" || length(print.obj) != 0 & print.call == "print.lsa.data(x = x)") {
    user.def.miss <- any(sapply(X = x, FUN = function(i) {
      length(attr(x = i, "missings")) > 0
    }))
    tmp.data <- setDT(copy(x[ , mget(col.nums)]))
    if(length(col.nums) < length(colnames(x))) {
      tmp.data[ , ".../..." := ".../..."]
    }
    list.of.components <- list(
      attr(x = x, which = "study"),
      attr(x = x, which = "cycle"),
      attr(x = x, which = "file.type"),
      length(unique(x[ , mget(key(x))])),
      attr(x = x, which = "sorted"),
      user.def.miss
    )
    message("\nLarge-scale assessment/survey data")
    message("==================================")
    message("Study:                 ", list.of.components[[1]])
    message("Study cycle:           ", list.of.components[[2]])
    message("Respondent type:       ", list.of.components[[3]])
    message("Number of countries:   ", list.of.components[[4]])
    message("Key:                   ", list.of.components[[5]])
    message("User defined missings: ", list.of.components[[6]])
    message("")
    if(length(col.nums) < length(colnames(x))) {
      message("\nData (omitted ", length(colnames(x)) - length(col.nums), " columns):\n")
    } else {
      message("\nData:\n")
    }
    message(paste0(capture.output(tmp.data), collapse = "\n"))
  } else if(length(print.obj) == 0 && print.call == "print.lsa.data(x = x)") {
    message(paste0(capture.output(data.table(x)), collapse = "\n"))
  }
}
#' @rdname lsa.convert.data
#' @export
lsa.select.countries.PISA <- function(data.file, data.object, cnt.names, output.file) {
  if(missing(cnt.names)) {
    stop('Vector of country names must be provided for the "cnt.names" argument. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(!missing(data.file) == TRUE && !missing(data.object) == TRUE) {
    stop('Either "data.file" or "data.object" has to be provided, but not both. All operations stop here. Check your input.\n\n', call. = FALSE)
  } else if(!missing(data.file)) {
    if(file.exists(data.file) == FALSE) {
      stop('The file specified in the "data.file" argument does not exist. All operations stop here. Check your input.\n\n', call. = FALSE)
    }
    ptm.data.import <- proc.time()
    data <- copy(import.data(path = data.file))
    used.data <- deparse(substitute(data.file))
    message('\nData file ', used.data, ' imported in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.data.import}[[3]], "%H:%M:%OS3"), "\n")
  } else if(!missing(data.object)) {
    if(length(all.vars(match.call())) == 0) {
      stop('The object specified in the "data.object" argument is quoted, is this an object or a path to a file? All operations stop here. Check your input.\n\n', call. = FALSE)
    }
    if(!exists(all.vars(match.call()))) {
      stop('The object specified in the "data.object" argument does not exist. All operations stop here. Check your input.\n\n', call. = FALSE)
    }
    data <- copy(data.object)
    used.data <- deparse(substitute(data.object))
    message('\nUsing data from object "', used.data, '".')
  }
  if(!"lsa.data" %in% class(data)) {
    stop('The data is not of class "lsa.data". All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(attributes(data)[["study"]] != "PISA" ) {
    stop('The data is not PISA data. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  ptm.data.reduction <- proc.time()
  warnings.collector <- list()
  cnt.names.in.data <- names(Filter(isFALSE, sapply(X = cnt.names, FUN = function(i) {
    i %in% data[ , CNT]
  })))
  if(length(cnt.names.in.data) > 0) {
    warnings.collector[["nonexisting.cnt"]] <- paste0('One or more country names passed to "cnt.names" do not exist in the data. These were ignored: ', paste(cnt.names.in.data, collapse = ", "), ".")
    cnt.names <- cnt.names[!cnt.names %in% cnt.names.in.data]
  }
  message("   A total of ", length(cnt.names), " valid country names found in the data.")
  tmp.key <- key(data)
  data <- data[CNT %in% cnt.names, ]
  data[ , CNT := droplevels(CNT)]
  setkeyv(x = data, cols = tmp.key)
  message('\n   Data from ', length(unique(data[ , CNT])), ' valid country names selected in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.data.reduction}[[3]], "%H:%M:%OS3"))
  if(!missing(output.file)) {
    ptm.save <- proc.time()
    assign(x = gsub(pattern = "\\.RData$", replacement = "", x = basename(output.file), ignore.case = TRUE), value = data)
    if(length(grep(pattern = "\\.RData$", x = output.file, value = TRUE)) == 0) {
      output.file <- paste0(output.file, ".RData")
    }
    save(list = gsub(pattern = "\\.RData$", replacement = "", x = basename(output.file), ignore.case = TRUE), file = output.file, compress = FALSE)
    message('\nThe file with the selected countries  "', basename(output.file), '" is saved under "', dirname(output.file), '" in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.save}[[3]], "%H:%M:%OS3"), "\n")
  } else {
    assign(x = gsub(pattern = "\\.RData$", replacement = "", x = basename(data.file), ignore.case = TRUE), value = data, envir = parent.frame())
    message('\nThe data object "', gsub(pattern = "\\.RData$", replacement = "", x = basename(data.file), ignore.case = TRUE), '" with selected countries was written to memory.\n')
  }
  if(length(warnings.collector) > 0) {
    if(!is.null(warnings.collector[["nonexisting.cnt"]])) {
      warning(warnings.collector[["nonexisting.cnt"]], call. = FALSE)
    }
  }
}
