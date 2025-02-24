#' @title Download large-scale assessment and survey data
#'
#' @description Downloads data files from large-scale assessments' and surveys' data repositories on the web and, if desired, converts them to \code{lsa.data} format and stores them in .RData files.
#'
#' @param study         String, large-scale assessment or study name. See details.
#' @param cycle         Numeric, study year of administration (cycle). See details.
#' @param POP           String, population of interest. If none is provide, default is taken.
#'                      See details.
#' @param ISO           Vector containing character ISO codes of the countries' data files to
#'                      include in the merged file. See details.
#' @param append        If some files for the study, cycle, populations and countries have
#'                      already been downloaded, download only the new ones.
#'                      (default is \code{TRUE}). See details.
#' @param convert       Logical, shall the data be converted to \code{lsa.data} and stored in
#'                      \code{.RData} files (default) after being downloaded. See details.
#' @param missing.to.NA Logical, should the user-defined missing values be converted to \code{NA}
#'                      when converting the downloaded data  (default is \code{FALSE})?
#'                      See \code{lsa.convert.data}.
#' @param out.folder    Path to the folder where the downloaded (and optionally converted) files
#'                      will be stored. If the final folder in the path does not exist, it will
#'                      be created.
#'
#' @details
#' The \code{lsa.download.data} function downloads large-scale assessments' and surveys' data files from data repositories on the web. This is a convenience function that saves time and efforts for the user. IEA studies, as well as OECD TALIS and TALIS 3S, provide their data in SPSS \code{.sav} format with same or very similar structure: one file per country and type of respondent (e.g. school principal, student, teacher, etc.) per population. For IEA studies and OECD TALIS and TALIS 3S use the \code{ISO} argument to specify the countries' three-letter ISO codes whose data is to be downloaded. The three-letter ISO codes for each country can be found in the user guide for the study in scope. For example, the ISO codes of the countries participating in PIRLS 2016 can be found in its user guide on pages 52-54. To download the files from all countries for an IEA study and OECD TALIS and TALIS 3S, simply omit the \code{ISO} argument, this will download files for all countries for the population in \code{POP} in the \code{study} and \code{cycle}. The \code{ISO} argument will not work for PISA files, as all data for all countries is provided within a single file per respondent type. If \code{ISO} is provided anyway, it will be ignored. Note that as of now, the function downloads PISA databases only from its latest cycles - 2015, 2018 and 2022.
#'
#' When all desired SPSS data files are downloaded, the function converts them to \code{lsa.data} objects and stores them as .RData files on the disk, removing the data files downloaded in their original (SPSS) format. This is the default behavior which can be overridden by setting \code{convert = FALSE}.
#'
#' The \code{study} argument defines the study for which data shall be downloaded. The acceptable strings are as follows:
#'
#' \itemize{
#'          \item \code{CivED} - IEA Civic and Citizenship Education study (CivED)
#'          \item \code{ICCS} - IEA International Civic and Citizenship Education Study (ICCS)
#'          \item \code{ICILS} - IEA International Computer and Information Literacy Study (ICILS)
#'          \item \code{PIRLS} - IEA Progress in International Reading Literacy Study (PIRLS)
#'          \item \code{prePIRLS} - IEA PIRLS Literacy (prePIRLS)
#'          \item \code{REDS} - IEA Responses to Educational Disruption Survey (REDS)
#'          \item \code{RLII} - IEA Reading Literacy Study (RL), second round
#'          \item \code{SITES} - IEA Second Information Technology in Education Study (SITES)
#'          \item \code{TIMSS} - IEA Trends in International Mathematics and Science Study (TIMSS)
#'          \item \code{preTIMSS} - IEA TIMSS Numeracy (TIMSS)
#'          \item \code{eTIMSS PSI} - IEA TIMSS with PSI items (TIMSS)
#'          \item \code{TIMSS Advanced Mathematics}/\code{TIMSS Advanced Physics} - IEA Trends in International Mathematics and Science Study in Mathematics and Physics (TIMSS Advanced)
#'          \item \code{TiPi} - IEA joint TIMSS and PIRLS 2011 study
#'          \item \code{PISA} - OECD Programme for International Student Assessment (PISA)
#'          \item \code{PISA D} - OECD Programme for International Student Assessment for low- and middle-income countries (PISA for Development)
#'          \item \code{TALIS} - OECD Teaching and Learning International Survey (TALIS) and
#'          \item \code{TALIS 3S} - OECD Starting Strong Teaching and Learning International Survey (TALIS Starting Strong Survey)
#' }
#'
#' The \code{cycle} argument provides information about the year of administration of a particular study for which SPSS data files can be downloaded. A numeric value for a specific year of administration needs to be provided. Here is the list from all released cycles for all studies RALSA supports till now:
#'
#' \itemize{
#'          \item For \code{CivED} - \code{1999}
#'          \item For \code{ICCS} - \code{2009}, \code{2016}, or \code{2022}
#'          \item For \code{ICILS} - \code{2013}, \code{2018}, or \code{2023}
#'          \item For \code{PIRLS} - \code{2001}, \code{2006}, \code{2011}, \code{2016}, or \code{2021}
#'          \item For \code{prePIRLS} - \code{2016}
#'          \item For \code{ePIRLS} - \code{2016}
#'          \item For \code{REDS} - \code{2021}
#'          \item For \code{RLII} - \code{1991} or \code{2001}
#'          \item For \code{SITES} - \code{1998} or \code{2006}
#'          \item For \code{TIMSS} - \code{1995}, \code{1998}, \code{2003}, \code{2007}, \code{2011}, \code{2015}, \code{2019} or \code{2023}
#'          \item For \code{preTIMSS} - \code{2015}
#'          \item For \code{eTIMSS PSI} - \code{2019}
#'          \item For \code{TIMSS Advanced Mathematics}/\code{TIMSS Advanced Physics} - \code{1995}, \code{2008}, or \code{2015}
#'          \item For \code{TiPi} - \code{2011}
#'          \item For \code{PISA} - \code{2015}, \code{2018} or \code{2022}
#'          \item For \code{PISA D} - \code{2019}
#'          \item For \code{TALIS} - \code{2008}, \code{2013}, or \code{2018}
#'          \item For \code{TALIS 3S} - \code{2018}
#' }
#'
#' Note that the data from the IEA Teacher Education and Development Study in Mathematics (TEDS-M) is not freely available from the IEA website due to data confidentiality issues and is available only on [request from the IEA](https://www.iea.nl/data-tools/repository/tedsm).
#'
#' Some studies (e.g. TIMSS and TALIS) have more than one population (i.e. students in grades 4 and 8 in TIMSS and teachers in different ISCED levels in TALIS). The \code{POP} argument is required for these studies, as the \code{lsa.download.data} needs to know data from which population is needed. The population strings for the \code{POP} argument for the pertinent studies are as follows:
#'
#' \itemize{
#' \item \strong{CivED}:
#'       \itemize{
#'                \item \code{G8} (grade 8)
#'                \item \code{G12} (grade 12)
#'     }
#' \item \strong{ICCS}:
#'       \itemize{
#'                \item \code{G8} (grade 8)
#'                \item \code{G9} (grade 9, ICCS 2009 only)
#'     }
#' \item \strong{ICILS}:
#'       \itemize{
#'                \item \code{G8} (grade 8)
#'       }
#' \item \strong{PIRLS}:
#'       \itemize{
#'                \item \code{G4} (grade 4)
#'       }
#' \item \strong{prePIRLS}:
#'       \itemize{
#'                \item \code{G4} (grade 4)
#'       }
#' \item \strong{ePIRLS}:
#'       \itemize{
#'                \item \code{G4} (grade 4)
#'       }
#' \item \strong{REDS}:
#'       \itemize{
#'                \item \code{G8} (grade 8)
#'       }
#' \item \strong{RLII}:
#'       \itemize{
#'                \item \code{G4} (grade 4)
#'       }
#' \item \strong{SITES}:
#'       \itemize{
#'                \item \code{M1 POP A} (Module 1, 1998, population A)
#'                \item \code{M1 POP B} (Module 1, 1998, population B)
#'                \item \code{M1 POP C} (Module 1, 1998, population C)
#'                \item \code{M2} (Module 2, 2006)
#'       }
#' \item \strong{TIMSS}:
#'       \itemize{
#'                \item \code{G4} (grade 4)
#'                \item \code{G8} (grade 8)
#'     }
#' \item \strong{preTIMSS}:
#'       \itemize{
#'                \item \code{G4} (grade 4)
#'     }
#' \item \strong{eTIMSS PSI}:
#'       \itemize{
#'                \item \code{G4} (grade 4)
#'                \item \code{G8} (grade 8)
#'     }
#' \item \strong{TIMSS Advanced Mathematics} / \strong{TIMSS Advanced Physics}:
#'       \itemize{
#'                \item \code{G12} (grade 12)
#'     }
#' \item \strong{TiPi}:
#'       \itemize{
#'                \item \code{G4} (grade 4)
#'     }
#' \item \strong{PISA}:
#'       \itemize{
#'                \item \code{Y15} (15-year-old)
#'     }
#' \item \strong{PISA for Development}
#'       \itemize{
#'              \item \code{IS} (in school)
#'              \item \code{OS} (out of school)
#'     }
#' \item \strong{TALIS}:
#'       \itemize{
#'               \item \code{I1} (ISCED 1)
#'               \item \code{I2} (ISCED 2)
#'               \item \code{I3} (ISCED 3)
#'               \item \code{P} (PISA schools)
#'      }
#' \item \strong{TALIS 3S}:
#'       \itemize{
#'               \item \code{I0.2} (ISCED 0.2)
#'               \item \code{IU3} (ISCED U3)
#'      }
#' }
#'
#' For the exact meaning of the population names, see the respective study documentation. Note that if \code{POP} is not provided, a default (first population for a study and/or a cycle) is applied.
#'
#' The \code{out.folder} argument controls where the files shall be stored. Note that the files will \strong{not} download the files directly in the folder path provided to the argument. Instead, it will create a folder named as the study name, cycle and population and place the downloaded files there. Note that for IEA studies, OECD TALIS and TALIS 3S, if the download folder already exists and it contains data files for a given study, cycle and population for some of the countries, the function will only append the new files in it, keeping the ones that already exist there, if the \code{append} argument equals \code{TRUE} (default). This can save a lot of time if the user needs to download just the additional files instead of download everything again. If \code{append} argument equals \code{TRUE}, the existing files will be overwritten. For OECD PISA and PISA for Development the \code{append} argument will be ignored and, if the study folder in \code{out.folder} exists and contains any SPSS \code{.sav} or \code{.RData} files, the function will stop its execution and ask for moving the existing files.
#'
#' @return
#'
#' If \code{convert = FALSE}, the function will return the originally downloaded data files (SPSS or ASCII textt with \code{.sps} control files) for the study, cycle, countries and population defined in the respective arguments, stored in the directory specified in \code{out.folder}. If \code{convert = TRUE} (default), converted \code{.RData} data files, containing an object with class \code{lsa.data}, an extension of the \code{data.table} class, will be saved in the directory defined by \code{out.folder} and the original downloaded SPSS data files will be removed.
#'
#' @note
#' \strong{It is not recommended to work further in the folder where the downloaded files reside, it is meant to be only for the downloaded (an possibly converted) files.}
#'
#' In some study cycles (e.g. TIMSS 2019 and PIRLS 2021), there are the so-called "bridge studies". These aim to test the differences between electronic and paper testing modes. When a study cycle contains data files from a bridge study, these will be downloaded too for the countries that conducted the study electronically.
#'
#' As of now, PISA data can be downloaded only for the 2015, 2018 and 2022 cycles.
#'
#' @examples
#' # Download and convert PIRLS 2016 data for Australia and Slovenia only and convert them
#' \dontrun{
#' lsa.download.data(study = "PIRLS", cycle = 2016, ISO = c("aus", "svn"), out.folder = "C:/Data")
#' }
#'
#' # Same as the above, but download files for all countries
#' \dontrun{
#' lsa.download.data(study = "PIRLS", cycle = 2016, out.folder = "C:/Data")
#' }
#'
#' # Download TIMSS 2019 data for grade 8 for South Africa and convert them
#' \dontrun{
#' lsa.download.data(study = "TIMSS", cycle = 2019, ISO = c("aus", "zaf"), POP = "Grade 8",
#' out.folder = "C:/Data")
#' }
#'
#' # Download PISA 2012 data and covert them
#' \dontrun{
#' lsa.download.data(study = "PISA", cycle = 2012, out.folder = "C:/Data")
#' }
#'
#'
#' @references
#' Foy, P. (Ed.). (2018). \emph{PIRLS 2016 User Guide for the International Database}. TIMSS & PIRLS International Study Center.
#'
#' @seealso \code{\link{lsa.convert.data}}
#' @export
lsa.download.data <- function(study, cycle, POP, ISO, out.folder, append = TRUE, convert = TRUE, missing.to.NA = FALSE) {
  tmp.options <- options(timeout = 18000)
  on.exit(expr = options(tmp.options), add = TRUE)
  warnings.collector <- list()
  if(missing(out.folder)) {
    stop('No path is provided to the "out.folder" argument. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(!missing(out.folder) & isFALSE(dir.exists(out.folder))) {
    dir.create(path = out.folder, recursive = TRUE)
  }
  cycle <- as.character(cycle)
  study.cycle.pop.strings <- as.list(grep(pattern = paste0("^", gsub(pattern = " ", replacement = "_", x = study)), x = names(study.dataset.files[["ZIP.data.folders.and.files"]]), ignore.case = TRUE, value = TRUE))
  study.cycle.pop.strings <- sapply(X = study.cycle.pop.strings, FUN = function(i) {
    i <- gsub(pattern = "eTIMSS_PSI", replacement = "eTIMSS PSI", x = i)
    i <- gsub(pattern = "TIMSS_Advanced_Mathematics", replacement = "TIMSS Advanced Mathematics", x = i)
    i <- gsub(pattern = "TIMSS_Advanced_Physics", replacement = "TIMSS Advanced Physics", x = i)
    i <- gsub(pattern = "TALIS_3S", replacement = "TALIS 3S", x = i)
    i <- gsub(pattern = "PISA_D", replacement = "PISA D", x = i)
    i <- strsplit(x = i, split = "_")
  })
  study.cycle.pop.strings <- Filter(f = length, x = lapply(X = study.cycle.pop.strings, FUN = function(i) {
    i[study %in% i]
  }))
  study.cycle.pop.strings <- unlist(Filter(f = length, x = sapply(X = study.cycle.pop.strings, FUN = function(i) {
    i[cycle %in% i]
  })))
  study.cycle.pop.strings <- paste(gsub(pattern = " ", replacement = "_", x = study.cycle.pop.strings), collapse = "_")
  if(missing(POP) & !study %in% c("PISA", "PISA D")) {
    POP <- str_extract(string = names(study.dataset.files[["ZIP.data.folders.and.files"]][[study.cycle.pop.strings]]), pattern = "G[[:digit:]]+$|I[[:digit:]]+$|M[[:digit:]]+$|Y[[:digit:]]+$|IS$|I0.2$")[1]
  } else if(missing(POP) & study == "PISA") {
    POP <- "Y15"
  } else if(missing(POP) & study == "PISA D") {
    POP <- "IS"
  }
  study.cycle.pop.strings.full <- paste(study.cycle.pop.strings, POP, sep = "_")
  if(!study %in% c("PISA", "PISA D")) {
    if(!study.cycle.pop.strings.full %in% names(study.dataset.files[["ZIP.data.folders.and.files"]][[study.cycle.pop.strings]])) {
      stop('A value passed to "study", "cycle" or "POP" is incorrect. Please refer to the ducumentation for acceptable values of these arguments. All operations stop here. Check your input.\n\n', call. = FALSE)
    } else {
      study.and.cycle.files.full <- grep(pattern = study.cycle.pop.strings.full, x = names(study.dataset.files[["ZIP.data.folders.and.files"]][[study.cycle.pop.strings]]), value = TRUE)
      study.and.cycle.files.full <- study.dataset.files[["ZIP.data.folders.and.files"]][[study.cycle.pop.strings]][study.and.cycle.files.full]
      study.and.cycle.files.to.download <- lapply(X = study.and.cycle.files.full, FUN = function(i) {
        i[[2:length(i)]]
      })
      ZIP.folder.paths <- lapply(X = study.and.cycle.files.full, FUN = function(i) {
        i[[1]]
      })
    }
    if(!missing(ISO)) {
      study.and.cycle.files.to.download <- lapply(X = study.and.cycle.files.to.download, FUN = function(i) {
        grep(pattern = paste0("^.{3}", ISO, collapse = "|"), x = i, ignore.case = TRUE, value = TRUE)
      })
    } else {
      study.and.cycle.files.to.download <- study.and.cycle.files.to.download
    }
  } else if(study %in% c("PISA", "PISA D")) {
    study.and.cycle.files.to.download <- study.dataset.files[["ZIP.data.folders.and.files"]][[study.cycle.pop.strings]][[study.cycle.pop.strings.full]][2]
    names(study.and.cycle.files.to.download) <- study.cycle.pop.strings.full
  }
  if(study %in% c("CivED", "ICCS", "ICILS", "PIRLS", "ePIRLS", "prePIRLS", "REDS", "RLII", "SITES", "TIMSS", "eTIMSS PSI", "preTIMSS", "TIMSS Advanced Mathematics", "TIMSS Advanced Physics", "TiPi")) {
    link.root <- "https://www.iea.nl/sites/default/files/data-repository/"
  } else if(study %in% c("TALIS", "TALIS 3S")) {
    link.root <- "https://webfs.oecd.org/talis/"
  } else if(study == "PISA" & cycle == "2015") {
    link.root <- "https://webfs.oecd.org/pisa/"
  } else if(study == "PISA" & cycle == "2018") {
    link.root <- "https://webfs.oecd.org/pisa2018/"
  } else if(study == "PISA" & cycle == "2022") {
    link.root <- "https://webfs.oecd.org/pisa2022/"
  } else if(study == "PISA D" & cycle == "2019" & POP == "IS") {
    link.root <- "https://web-archive.oecd.org/site/pisa-for-development/database/"
  } else if(study == "PISA D" & cycle == "2019" & POP == "OS") {
    link.root <- "https://webfs.oecd.org/pisad/"
  }
  download.URL <- paste0(link.root, study.dataset.files[["ZIP.roots"]][[study]][[cycle]][[POP]])
  message(paste0("\nConnecting to the ", study, " ", cycle, " database...\nDepending on the selection of files, the proces can take some time.\n"))
  tryCatch({
    if(!study %in% c("PISA", "PISA D")) {
      if(!missing(ISO)) {
        countries.found <- unique(gsub(pattern = "^[[:alpha:]]{2}\\_|^[[:alpha:]]{3}|[[:alnum:]]{2}\\.sav$", replacement = "", x = unlist(study.and.cycle.files.to.download), ignore.case = TRUE))
        countries.NOT.found <- setdiff(x = tolower(ISO), y = tolower(countries.found))
        message(paste0("     Files for ", length(countries.found), " of the requested ", length(ISO), " countries have been found."))
        if(length(countries.NOT.found) > 0) {
          warnings.collector[["ISOs.not.in.files"]] <- paste0("Files for the following requested countries haven't been found in the ", paste(study, cycle, POP, collapse = " "), ": ", paste(countries.NOT.found, collapse = ", "), ".\nCheck your input.")
        }
      } else {
        countries.NOT.found <- NULL
      }
      if(length(grep(pattern = "Bridge$", x = names(study.and.cycle.files.to.download), ignore.case = TRUE)) > 0) {
        message("     Main study files for ", length(unique(tolower(gsub(pattern = "^[[:alnum:]]{3}|\\.sav$", replacement = "", x = study.and.cycle.files.to.download[[grep(pattern = "Bridge$", x = names(study.and.cycle.files.to.download), ignore.case = TRUE, invert = TRUE)]], ignore.case = TRUE)))), " countries are found.")
        message("     Bridge study files for ", length(unique(tolower(gsub(pattern = "^[[:alnum:]]{3}|\\.sav$", replacement = "", x = study.and.cycle.files.to.download[[grep(pattern = "Bridge$", x = names(study.and.cycle.files.to.download), ignore.case = TRUE)]], ignore.case = TRUE)))), " countries are found.")
      }
      message("     A total of ", length(unlist(study.and.cycle.files.to.download)), " requested files found.\n\n     Please be patient.\n")
      files.to.download.zero.length <- names(Filter(f = length, study.and.cycle.files.to.download))
      study.and.cycle.files.to.download <- study.and.cycle.files.to.download[files.to.download.zero.length]
      ZIP.folder.paths <- ZIP.folder.paths[files.to.download.zero.length]
      ptm.download <- proc.time()
      lapply(X = names(study.and.cycle.files.to.download), FUN = function(i) {
        full.file.paths <- paste0(ZIP.folder.paths[[i]], "/", study.and.cycle.files.to.download[[i]])
        if(isFALSE(dir.exists(paths = file.path(out.folder, i)))) {
          dir.create(path = file.path(out.folder, i))
        } else if(isTRUE(dir.exists(paths = file.path(out.folder, i))) & isTRUE(append)) {
          files.existing.in.out.dir <- gsub(pattern = "\\.sav$|\\.RData$", replacement = "", x = list.files(path = file.path(out.folder, i), pattern = "\\.sav$|\\.RData$", ignore.case = TRUE), ignore.case = TRUE)
          files.passed.for.download <- gsub(pattern = "\\.sav$", replacement = "", x = study.and.cycle.files.to.download[[i]], ignore.case = TRUE)
          full.file.paths <- grep(pattern = paste(files.existing.in.out.dir, collapse = "|"), x = full.file.paths, ignore.case = TRUE, value = TRUE, invert = TRUE)
        }
        if(study == "TALIS") {
          full.file.paths <- gsub(pattern = "^\\.\\/", replacement = "", x = full.file.paths)
        }
        if(length(full.file.paths) > 0) {
          if(grepl(pattern = "Bridge$", x = i, ignore.case = TRUE)) {
            message("Downloading bridge study data files.\n")
          } else {
            message("Downloading study data files.\n")
          }
        }
        if(length(full.file.paths) > 0) {
          archive_extract(archive = download.URL, dir = file.path(out.folder, i), files = full.file.paths)
          number.of.countries <- length(unique(gsub(pattern = "^[[:alnum:]]{3}|.{6}$", replacement = "", x = study.and.cycle.files.to.download[[i]])))
          if(number.of.countries > 1) {
            countries.num.string <- " countries "
          } else {
            countries.num.string <- " country "
          }
          if(length(grep(pattern = "Bridge", x = i)) == 0) {
            message("     All ", length(study.and.cycle.files.to.download[[i]]), " main study data files for ", number.of.countries, countries.num.string, "have been donwloaded in ", format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.download}[[3]] - 1, "%H:%M:%OS3"), ".\n")
          } else {
            message("     All ", length(study.and.cycle.files.to.download[[i]]), " bridge data files for ", number.of.countries, countries.num.string, "have been donwloaded in ", format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.download}[[3]] - 1, "%H:%M:%OS3"), ".\n")
          }
          if(study != "TALIS") {
            file.copy(from = file.path(out.folder, i, full.file.paths), to = file.path(out.folder, i))
            folder.to.remove <- unlist(strsplit(x = ZIP.folder.paths[[i]], split = "/")[1])
            unlink(x = file.path(out.folder, i, folder.to.remove), recursive = TRUE, force = TRUE)
          }
        } else {
          message('No new files to download for "', i, '", all requested files already exist in the download folder. Skipping.\n')
        }
      })
    } else {
      ptm.download <- proc.time()
      if(!dir.exists(file.path(out.folder, study.cycle.pop.strings.full))) {
        dir.create(file.path(out.folder, study.cycle.pop.strings.full))
      }
      if(length(list.files(path = file.path(out.folder, study.cycle.pop.strings.full), pattern = "\\.sav$|\\.RData$", ignore.case = TRUE)) > 0) {
        stop('The study download folder ("', study.cycle.pop.strings.full, '") in "out.folder" already contains data files. To prevent overwriting them, inspect their content and remove them manually or choose a different "out.folder". All operations stop here.\n', call. = FALSE)
      }
      ZIP.files.to.remove <- file.path(out.folder, study.cycle.pop.strings.full, basename(path = download.URL))
      if(study == "PISA") {
        message(paste0("     A total of ", length(download.URL), " requested files will be downloaded.\n\n     PISA files are rather large. Please be patient.\n"))
      } else if(study == "PISA D") {
        message(paste0("     A total of ", length(download.URL), " requested files will be downloaded.\n\n     Please be patient.\n"))
      }
      sapply(X = 1:length(download.URL), FUN = function(i) {
        ptm.download.per.file <- proc.time()
        file.extension <- substring(text = download.URL[i], first = nchar(download.URL[i]) - 3, last = nchar(download.URL[i]))
        if(file.extension %in% c(".zip", ".ZIP")) {
          download.file(url = download.URL[i], destfile = file.path(out.folder, study.cycle.pop.strings.full, basename(download.URL[i])), quiet = TRUE)
        } else if(file.extension %in% c(".sav", ".SAV")) {
          download.file(url = download.URL[i], destfile = file.path(out.folder, study.cycle.pop.strings.full, basename(download.URL[i])), quiet = FALSE, method = "curl")
        }
        spaces.to.append <- max(nchar(download.URL))
        spaces.to.append <- rep(x = " ", times = spaces.to.append - nchar(download.URL[i]))
        message("     (", i, "/", length(download.URL), ") ", basename(download.URL[i]), " has been downloaded in ", spaces.to.append, format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.download.per.file}[[3]] - 1, "%H:%M:%OS3"))
      })
      message("\nAll requested ", length(download.URL), " files have been downloaded in ", format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.download}[[3]] - 1, "%H:%M:%OS3"), ".\n")
      if(study %in% c("PISA", "PISA D") & POP %in% c("Y15", "IS")) {
        sapply(X = 1:length(download.URL), FUN = function(i) {
          unzip(zipfile = file.path(out.folder, study.cycle.pop.strings.full, basename(download.URL[i])), exdir = file.path(out.folder, study.cycle.pop.strings.full))
          message("     (", i, "/", length(download.URL), ") SPSS data from ", basename(download.URL[i]), " has been extracted.")
        })
        message("\nAll requested ", length(download.URL), " files have been extracted.\n")
        unlink(x = ZIP.files.to.remove, force = TRUE)
      }
    }
    if(isTRUE(convert)) {
      message("Converting files as requested.")
      if(!study %in% c("PISA", "PISA D")) {
        convert.statement <- lapply(X = names(study.and.cycle.files.to.download), FUN = function(i) {
          if(length(list.files(path = file.path(out.folder, i), pattern = "\\.sav$", ignore.case = TRUE)) > 0) {
            paste0('lsa.convert.data(inp.folder = "', file.path(out.folder, i), '", missing.to.NA = ', missing.to.NA, ', out.folder = "', file.path(out.folder, i), '")')
          }
        })
      } else {
        if(study == "PISA") {
          convert.statement <- paste0('lsa.convert.data(inp.folder = "', file.path(out.folder, study.cycle.pop.strings.full), '", PISApre15 = FALSE, missing.to.NA = ', missing.to.NA, ', out.folder = "', file.path(out.folder, study.cycle.pop.strings.full), '")')
        } else if(study == "PISA D") {
          convert.statement <- paste0('lsa.convert.data(inp.folder = "', file.path(out.folder, study.cycle.pop.strings.full), '", missing.to.NA = ', missing.to.NA, ', out.folder = "', file.path(out.folder, study.cycle.pop.strings.full), '")')
        }
      }
      src.files.to.remove <- file.path(unlist(lapply(X = names(study.and.cycle.files.to.download), FUN = function(i) {
        if(length(list.files(path = file.path(out.folder, i), pattern = "\\.sav$", ignore.case = TRUE)) > 0) {
          intersect(x = file.path(out.folder, i, study.and.cycle.files.to.download[[i]]), y = list.files(file.path(out.folder, i), full.names = TRUE))
        } else {
          NULL
        }
      })))
      if(length(src.files.to.remove) > 0) {
        file.remove.statement <- paste0('file.remove("', paste(src.files.to.remove, collapse = '", "'), '")')
      }
    } else if(isFALSE(convert)) {
      sav.files.exist <- file.path(unlist(lapply(X = names(study.and.cycle.files.to.download), FUN = function(i) {
        list.files(path = file.path(out.folder, i), pattern = "\\.sav$", ignore.case = TRUE)
      })))
      if(length(sav.files.exist) > 0) {
        message('Use the "lsa.convert.data" function later to convert the existing SPSS data files in .RData format.\n')
      }
    }
  }, interrupt = function(f) {
    message("\nInterrupted by the user. No files were downloaded.\n")
  })
  if(length(warnings.collector) > 0) {
    if(!is.null(warnings.collector[["ISOs.not.in.files"]])) {
      warning(warnings.collector[["ISOs.not.in.files"]], call. = FALSE)
    }
  }
  if(isTRUE(convert)) {
    if(exists("convert.statement")) {
      on.exit(expr = eval(parse(text = convert.statement)))
    }
    if(exists("file.remove.statement")) {
      on.exit(expr = eval(parse(text = file.remove.statement)), add = TRUE)
    }
  }
}
