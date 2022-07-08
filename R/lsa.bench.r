#' @title Compute percentages of respondents reaching or surpassing certain ability cut-off scores
#'
#' @description \code{lsa.bench} computes percentages of respondents reaching or surpassing certain ability cut-off scores (benchmarks/performance levels). The cut-off scores are points in the distributions of PVs defined differently in different studies and, sometimes, in different study cycles. The percentages can also be computed as cumulative percentages. There is an option to compute an average of continuous contextual/background variable.
#'
#' @param data.file       The file containing \code{lsa.data} object. Either this or \code{data.object}
#'                        shall be specified, but not both. See details.
#' @param data.object     The object in the memory containing \code{lsa.data} object. Either this
#'                        or \code{data.file} shall be specified, but not both. See details.
#' @param split.vars      Categorical variable(s) to split the results by. If no split variables are
#'                        provided, the results will be for the overall countries' populations. If
#'                        one or more variables are provided, the results will be split by all but
#'                        the last variable and the percentages of respondents will be computed by
#'                        the unique values of the last splitting variable.
#' @param PV.root.bench   The root name(s) for the set(s) of plausible values which will be used
#'                        to compute the percentages of respondents reaching or surpassing certain
#'                        cut-off score. See details.
#' @param bench.vals      A vector of integers representing the cut-off scores. See details.
#' @param bench.type      A character string representing how the percentages of respondents shall
#'                        be computed. See details.
#' @param pcts.within     Logical value specifying if the percentages shall be computed within the
#'                        groups defined by the \code{split.vars} (\code{TRUE}) or not (\code{FALSE},
#'                        default). See details.
#' @param bckg.var        Name of continuous background or contextual variable to compute the mean
#'                        for. The results will be computed by all groups specified by the
#'                        splitting variables and per performance group. See details.
#' @param weight.var      The name of the variable containing the weights. If no name of a weight
#'                        variable is provide, the function will automatically select the default
#'                        weight variable for the provided data, depending on the respondent type.
#' @param include.missing Logical, shall the missing values of the splitting variables be included
#'                        as categories to split by and all statistics produced for them? The
#'                        default (\code{FALSE}) takes all cases on the splitting variables
#'                        without missing values before computing any statistics. See details.
#' @param shortcut        Logical, shall the "shortcut" method for IEA TIMSS, TIMSS Advanced,
#'                        TIMSS Numeracy, eTIMSS PSI, PIRLS, ePIRLS, PIRLS Literacy and RLII be
#'                        applied? The default (\code{FALSE}) applies the "full" design when
#'                        computing the variance components and the standard errors of the
#'                        estimates.
#' @param graphs          Logical, shall graphs be produced? Default is \code{FALSE}. See details.
#' @param save.output     Logical, shall the output be saved in MS Excel file (default) or not
#'                        (printed to the console or assigned to an object).
#' @param output.file     If \code{save.output = TRUE} (default), full path to the output file
#'                        including the file name. If omitted, a file with a default file name
#'                        "Analysis.xlsx" will be written to the working directory (\code{getwd()}).
#'                        Ignored if \code{save.output = FALSE}.
#' @param open.output     Logical, shall the output be open after it has been written? The default
#'                        (\code{TRUE}) opens the output in the default spreadsheet program installed
#'                        on the computer. Ignored if \code{save.output = FALSE}.
#'
#' @details
#' Either \code{data.file} or \code{data.object} shall be provided as source of data. If both of them are provided, the function will stop with an error message.
#'
#' The function computes percentages of respondents which reach or surpass certain cut-off scores (benchmarks/performance levels). These percentages are computed using a set of PVs, specified in the \code{PV.root.bench}. Only one set of PVs can be added to \code{PV.root.bench} at a time. All studies (except CivED, TEDS-M, SITES, TALIS and TALIS Starting Strong Survey) have a set of PVs per content domain (e.g. in TIMSS five for overall mathematics, five for algebra, five for geometry, etc.) and cognitive domain (i.e. knowing, applying and reasoning). In some studies (say TIMSS and PIRLS) the names of the PVs in a set always start with character string and end with sequential number of the PV. For example, the names of the set of PVs for overall mathematics in TIMSS are BSMMAT01, BSMMAT02, BSMMAT03, BSMMAT04 and BSMMAT05. The root of the PVs for this set to be added to \code{PV.root.avg} will be "BSMMAT". The function will automatically find all the variables in this set of PVs and include them in the analysis. In other studies like OECD PISA and IEA ICCS and ICILS the sequential number of each PV is included in the middle of the name. For example, in ICCS the names of the set of PVs are PV1CIV, PV2CIV, PV3CIV, PV4CIV and PV5CIV. The root PV name has to be specified in \code{PV.root.bench} as "PV#CIV".
#'
#' Multiple splitting variables can be added to the \code{split.vars}, the function will compute the percentages of respondents reaching or surpassing the cut-off scores for all formed groups and their means on the continuous variables. If no splitting variables are added, the results will be only by country.
#'
#' If a continuous contextual/background variable is provided to the \code{bckg.var}, the average for that variable will be computed for each group formed by the splitting variables and the performance groups. Only one contextual/background variable can be added in the analysis. This argument is ignored when \code{bench.type = "cumulative"}.
#'
#' The cut-off scores are provided as vector of integers (e.g. \code{c(475, 500)}) to the \code{bench.vals}. If no cut-off scores are provided, the function will automatically choose all benchmark values for the corresponding study and, in some cases for the data from the specific cycle. The latter applies to ICCS and PISA where the proficiency levels differ from one cycle to another.
#'
#' The \code{bench.type} argument has two different options: \code{"discrete"} (default) and \code{"cumulative"}. Using the former will compute the percentages of respondents within the boundaries specified by the cut-off scores in \code{bench.vals}. Using the latter, the function will compute the percentages of respondents at or above the cut-off points in the \code{bench.vals}.
#'
#' If the \code{pcts.within} if \code{FALSE} (default), the function will compute the percentages of respondents reaching or surpassing each of the cut-off scores defined by \code{bench.vals}. In this case the percentages of all respondents across the performance levels will add to 100 in each group defined by the splitting variables. On the contrary, if \code{pcts.within = TRUE}, the function will compute the percentages of respondents at each of the performance levels across groups defined by the splitting variables. Then the sum of percentages within a specific performance level will sum up to 100 across the groups defined by the splitting variables. For example, we can compute what is the ratio (i.e. percentages) of female and male students performing between 475 and 550 points in PIRLS -- say 55 of all students performing at this level are female and 45 are male. If no split variables are provided, the percentages will be 100 for each performance level within a country. The argument is ignored if \code{bench.type = "cumulative"}.
#'
#' If no variables are specified for \code{bckg.var}, the output will contain only percentages of cases in groups specified by the splitting variables and the cut-off scores.
#'
#' If \code{include.missing = FALSE} (default), all cases with missing values on the splitting variables will be removed and only cases with valid values will be retained in the statistics. Note that the data from the studies can be exported in two different ways: (1) setting all user-defined missing values to \code{NA}; and (2) importing all user-defined missing values as valid ones and adding their codes in an additional attribute to each variable. If the \code{include.missing} is set to \code{FALSE} (default) and the data used is exported using option (2), the output will remove all values from the variable matching the values in its \code{missings} attribute. Otherwise, it will include them as valid values and compute statistics for them.
#'
#' The \code{shortcut} argument is valid only for TIMSS, eTIMSS PSI, TIMSS Numeracy, TIMSS Advanced, PIRLS, ePIRLS, PIRLS Literacy and RLII. Previously, in computing the standard errors, these studies were using 75 replicates because one of the schools in the 75 JK zones had its weights doubled and the other one has been taken out. Since TIMSS 2015 and PIRLS 2016 the studies use 150 replicates and in each JK zone once a school has its weights doubled and once taken out, i.e. the computations are done twice for each zone. For more details see Foy & LaRoche (2016) and Foy & LaRoche (2017). If replication of the tables and figures is needed, the \code{shortcut} argument has to be changed to \code{TRUE}.
#'
#' If \code{graphs = TRUE}, the function will produce graphs. If \code{split.vars} are specified, bar plots of percentages of respondents (population estimates) reaching or surpassing each benchmark level specified in \code{bench.vals} per group specified by \code{split.vars} will be produced with error bars (95% confidence) for these percentages. If \code{bckg.var} is specified, plots with 95% confidence intervals of the average for this variable will be produced. All plots are produced per country.
#'
#' @return
#' If \code{save.output = FALSE}, a list containing the estimates and analysis information. If \code{graphs = TRUE}, the plots will be added to the list of estimates.
#'
#' If \code{save.output = TRUE} (default), an MS Excel (\code{.xlsx}) file (which can be opened in any spreadsheet program), as specified with the full path in the \code{output.file}. If the argument is missing, an Excel file with the generic file name "Analysis.xlsx" will be saved in the working directory (\code{getwd()}). The workbook contains three spreadsheets. The first one ("Estimates") contains a table with the results by country and the final part of the table contains averaged results from all countries' statistics. The following columns can be found in the table, depending on the specification of the analysis:
#'
#' \itemize{
#'   \item \verb{<}Country ID\verb{>} - a column containing the names of the countries in the file for which statistics are computed. The exact column header will depend on the country identifier used in the particular study.
#'   \item \verb{<}Split variable 1\verb{>}, \verb{<}Split variable 2\verb{>}... - columns containing the categories by which the statistics were split by. The exact names will depend on the variables in \code{split.vars}.
#'   \item n_Cases - the number of cases reaching or surpassing each of the benchmarks using a set of PVs. Please note that these may not be whole numbers because they are computed using each PV and then averaged.
#'   \item Sum_\verb{<}Weight variable\verb{>} - the estimated population number of elements per group after applying the weights. The actual name of the weight variable will depend on the weight variable used in the analysis.
#'   \item Sum_\verb{<}Weight variable\verb{>}\verb{_}SE - the standard error of the the estimated population number of elements per group. The actual name of the weight variable will depend on the weight variable used in the analysis.
#'   \item Performance_Group - the labels for the performance groups defined by the \code{bench.vals}.
#'   \item Percentages_\verb{<}PVs' root name\verb{>} - the percentages of respondents (population estimates) reaching or surpassing each cut-off score (in case of \code{bench.type = "discrete"}) or the the percentages of respondents (population estimates) at or above each cut-off value (in case of \code{bench.type = "cumulative"}) per groups defined by the splitting variables in \code{split.vars}.
#'   \item Percentages_\verb{<}PVs' root name\verb{>}\verb{_}SE - the standard errors of the percentages from above.
#'   \item Mean_\verb{<}Background variable\verb{>} - the average of the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.var}.
#'   \item Mean_\verb{<}Background variable\verb{>}\verb{_}SE - the standard error of the average of the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.var}.
#'   \item Variance_\verb{<}Background variable\verb{>} - the variance for the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.var}.
#'   \item Variance_\verb{<}Background variable\verb{>}\verb{_}SE - the error of the variance for the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.var}.
#'   \item SD_\verb{<}Background variable\verb{>} - the standard deviation for the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.var}.
#'   \item SD_\verb{<}Background variable\verb{>}\verb{_}SE - the error of the standard deviation for the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.avg.var}.
#'   \item Percent_Missings_\verb{<}Background variable\verb{>} - the percentage of missing values for the \verb{<}Background variable\verb{>} specified in \code{bckg.var}.
#' }
#'
#' The second sheet contains some additional information related to the analysis per country in columns:
#' \itemize{
#'   \item DATA - used \code{data.file} or \code{data.object}.
#'   \item STUDY - which study the data comes from.
#'   \item CYCLE - which cycle of the study the data comes from.
#'   \item WEIGHT - which weight variable was used.
#'   \item DESIGN - which resampling technique was used (JRR or BRR).
#'   \item SHORTCUT - logical, whether the shortcut method was used.
#'   \item NREPS - how many replication weights were used.
#'   \item ANALYSIS_DATE - on which date the analysis was performed.
#'   \item START_TIME - at what time the analysis started.
#'   \item END_TIME - at what time the analysis finished.
#'   \item DURATION - how long the analysis took in hours, minutes, seconds and milliseconds.
#' }
#'
#' The third sheet contains the call to the function with values for all parameters as it was executed. This is useful if the analysis needs to be replicated later.
#'
#' If \code{graphs = TRUE} there will be an additional "Graphs" sheet containing all plots.
#'
#' If any warnings resulting from the computations are issued, these will be included in an additional "Warnings" sheet in the workbook as well.
#'
#' @examples
#' # Compute percentages of female and male students reaching or surpassing the "Intermediate"
#' # and "High" benchamrks in TIMSS 2015 grade 8 mathematics using data file, omit missing from
#' # the splitting variable (female and male as answered by the students), without shortcut, and
#' # open the output after the computations are done
#' \dontrun{
#' lsa.bench(data.file = "C:/Data/TIMSS_2015_G8_Student_Miss_to_NA.RData", split.vars = "BSBG01",
#' include.missing = FALSE, PV.root.bench = "BSMMAT", bench.vals = c(475, 550),
#' output.file = "C:/temp/test.xlsx", open.output = TRUE)
#' }
#'
#' # Repeat the analysis from above, using an object loaded in the memory, the student senate
#' # weight and compute the cumulative percentage, adding student feeling safe at school as a
#' # second splitting variable, using the shortcut method and including the missing values of
#' # the splitting variables
#' \dontrun{
#' lsa.bench(data.object = T15_G8_student_data, split.vars = c("BSBG01", "BSBG15B"),
#' PV.root.bench = "BSMMAT", bench.vals = c(475, 550), weight.var = "SENWGT",
#' include.missing = TRUE, shortcut = TRUE, output.file = "C:/temp/test.xlsx",
#' open.output = TRUE)
#' }
#'
#' # Compute the percentage of students reaching or surpassing the "Level 2" and "Level 3"
#' # in computer and information lteracy and the average of the complex background scale
#' # "Use of specialist applications for activities" by student sex and expected further
#' # level of education using ICILS 2018 data loaded in memory, include the missing values
#' # in the splitting variables
#' \dontrun{
#' lsa.bench(data.object = ICILS_2018_student_data, split.vars = c("S_SEX", "IS2G03"),
#' PV.root.bench = "PV#CIL", bckg.var = "S_SPECACT", include.missing = TRUE,
#' output.file = "C:/temp/test.xlsx", open.output = TRUE)
#' }
#'
#' # Compute the cumulative percentage of students at or above each of the (default) benchmarks
#' # of student overall reading achievement scores using PIRLS 2016 student data file, split the
#' # output by student sex, use the full design, include the missing values of the splitting
#' # variable (i.e. student sex), and do not open the output after the computations are finished
#' \dontrun{
#' lsa.bench(data.file = "C:/Data/PIRLS_2016_Student_Miss_to_NA.RData", split.vars = "ASBG01",
#' PV.root.bench = "ASRREA", bench.type = "cumulative", include.missing = TRUE,
#' output.file = "C:/temp/test.xlsx", open.output = FALSE)
#' }
#'
#' @references
#' LaRoche, S., Joncas, M., & Foy, P. (2016). Sample Design in TIMSS 2015. In M. O. Martin, I. V. S. Mullis, & M. Hooper (Eds.), \emph{Methods and Procedures in TIMSS 2015} (pp. 3.1-3.37). Chestnut Hill, MA: TIMSS & PIRLS International Study Center.
#' LaRoche, S., Joncas, M., & Foy, P. (2017). Sample Design in PIRLS 2016. In M. O. Martin, I. V. S. Mullis, & M. Hooper (Eds.), \emph{Methods and Procedures in PIRLS 2016} (pp. 3.1-3.34). Chestnut Hill, MA: Lynch School of Education, Boston College.
#'
#' @seealso \code{\link{lsa.convert.data}}
#' @export


lsa.bench <- function(data.file, data.object, split.vars, PV.root.bench, bench.vals, bench.type, pcts.within = FALSE, bckg.var, weight.var, include.missing = FALSE, shortcut = FALSE, graphs = FALSE, save.output = TRUE, output.file, open.output = TRUE) {
  tmp.options <- options(scipen = 999, digits = 22)
  on.exit(expr = options(tmp.options), add = TRUE)
  warnings.collector <- list()
  if(missing(PV.root.bench)) {
    stop('No PV root has been provided for the "PV.root.bench" argument. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(!missing(PV.root.bench) & length(PV.root.bench) > 1) {
    stop('Only one PV root can been provided for the "PV.root.bench" argument. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(!missing(bckg.var) && length(bckg.var) > 1) {
    stop('Only one background variable can been provided for the "bckg.var" argument. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(missing(bench.type)) {
    bench.type <- "discrete"
  } else {
    bench.type <- bench.type
  }
  if(bench.type == "cumulative" && pcts.within == TRUE) {
    warnings.collector[["cumulative.pcts.within"]] <- 'The argument "bench.type" was set to "cumulative" and the argument "pcts.within" was set to "TRUE". Statistics with these two arguments is not possible to compute. The value of "pcts.within" was ignored.'
    pcts.within <- FALSE
  }
  if(bench.type == "cumulative" && !missing(bckg.var)) {
    warnings.collector[["cumulative.bckg.var"]] <- 'The argument "bench.type" was set to "cumulative" and a background variable name was passed to "bckg.var". Statistics with these two arguments is not possible to compute. The value of "bckg.var" was ignored.'
  }
  if(!missing(data.file) == TRUE && !missing(data.object) == TRUE) {
    stop('Either "data.file" or "data.object" has to be provided, but not both. All operations stop here. Check your input.\n\n', call. = FALSE)
  } else if(!missing(data.file)) {
    if(file.exists(data.file) == FALSE) {
      stop('The file specified in the "data.file" argument does not exist. All operations stop here. Check your input.\n\n', call. = FALSE)
    }
    if(!is.logical(save.output) || !save.output %in% c(TRUE, FALSE)) {
      stop('\nThe "save.output" argument can take only logical values (TRUE or FALSE). All operations stop here. Check your input.', call. = FALSE)
    }
    ptm.data.import <- proc.time()
    data <- copy(import.data(path = data.file))
    used.data <- deparse(substitute(data.file))
    message('\nData file ', used.data, ' imported in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.data.import}[[3]], "%H:%M:%OS3"))
  } else if(!missing(data.object)) {
    if(length(all.vars(match.call())) == 0) {
      stop('The object specified in the "data.object" argument is quoted, is this an object or a path to a file? All operations stop here. Check your input.\n\n', call. = FALSE)
    }
    if(!exists(all.vars(as.list(match.call())[["data.object"]]))) {
      stop('The object specified in the "data.object" argument does not exist. All operations stop here. Check your input.\n\n', call. = FALSE)
    }
    data <- copy(data.object)
    used.data <- deparse(substitute(data.object))
    message('\nUsing data from object "', used.data, '".')
  }
  if(!"lsa.data" %in% class(data)) {
    stop('\nThe data is not of class "lsa.data". All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  vars.list <- get.analysis.and.design.vars(data)
  if(!missing(split.vars)) {
    vars.list[["split.vars"]] <- vars.list[["split.vars"]][!split.vars == key(data)]
    if(length(vars.list[["split.vars"]]) == 0) {
      vars.list[["split.vars"]] <- NULL
    }
  }
  action.args.list <- get.action.arguments()
  file.attributes <- get.file.attributes(imported.object = data)
  vars.list.analysis.vars <- grep(pattern = "split.vars|bckg.var", x = names(vars.list), value = TRUE)
  vars.list.analysis.vars <- unlist(vars.list[vars.list.analysis.vars])
  vars.list.analysis.vars <- grep(pattern = paste(unique(unlist(studies.all.design.variables)), collapse = "|"), x = vars.list.analysis.vars, value = TRUE)
  if(length(vars.list.analysis.vars) > 0) {
    warnings.collector[["vars.list.analysis.vars"]] <- 'Some of the variables specified as analysis variables (in "split.vars" and/or "bckg.avg.vars") are design variables (sampling variables or PVs). This kind of variables shall not be used for analysis. Check your input.'
  }
  if(missing(bench.vals)) {
    if(intersect(file.attributes[["lsa.study"]], names(default.benchmarks)) == "ICCS") {
      tmp.benchmarks <- default.benchmarks[["ICCS"]]
      bench.vals <- tmp.benchmarks[[intersect(file.attributes[["lsa.cycle"]], names(tmp.benchmarks))]]
    } else if(intersect(file.attributes[["lsa.study"]], names(default.benchmarks)) == "ICILS") {
      bench.vals <- default.benchmarks[["ICILS"]]
    } else if(intersect(file.attributes[["lsa.study"]], names(default.benchmarks)) == "PIRLS") {
      bench.vals <- default.benchmarks[["PIRLS"]]
    } else if(intersect(file.attributes[["lsa.study"]], names(default.benchmarks)) == "prePIRLS") {
      bench.vals <- default.benchmarks[["prePIRLS"]]
    } else if(intersect(file.attributes[["lsa.study"]], names(default.benchmarks)) == "ePIRLS") {
      bench.vals <- default.benchmarks[["ePIRLS"]]
    } else if(intersect(file.attributes[["lsa.study"]], names(default.benchmarks)) == "TIMSS") {
      bench.vals <- default.benchmarks[["TIMSS"]]
    } else if(intersect(file.attributes[["lsa.study"]], names(default.benchmarks)) == "preTIMSS") {
      bench.vals <- default.benchmarks[["preTIMSS"]]
    } else if(intersect(file.attributes[["lsa.study"]], names(default.benchmarks)) == "eTIMSS PSI") {
      bench.vals <- default.benchmarks[["eTIMSS PSI"]]
    } else if(intersect(file.attributes[["lsa.study"]], names(default.benchmarks)) == "TIMSS Advanced") {
      bench.vals <- default.benchmarks[["TIMSS Advanced"]]
    } else if(intersect(file.attributes[["lsa.study"]], names(default.benchmarks)) == "TiPi") {
      bench.vals <- default.benchmarks[["TiPi"]]
    } else if(intersect(file.attributes[["lsa.study"]], names(default.benchmarks)) == "PISA") {
      tmp.PV.root.name <- gsub(pattern = "[[:digit:]]+", replacement = "#", x = vars.list[["PV.root.bench"]], fixed = TRUE)
      tmp.benchmarks <- default.benchmarks[["PISA"]]
      tmp.benchmarks.PVs <- grep(pattern = "root.PVs$", x = names(tmp.benchmarks), value = TRUE)
      tmp.benchmarks.PVs <- names(unlist(sapply(X = tmp.benchmarks.PVs, FUN = function(i) {
        intersect(tmp.benchmarks[[i]], tmp.PV.root.name)
      })))
      tmp.benchmarks.PVs <- gsub(pattern = ".root.PVs", replacement = "", x = tmp.benchmarks.PVs)
      tmp.benchmarks <- default.benchmarks[["PISA"]][[tmp.benchmarks.PVs]]
      bench.vals <- tmp.benchmarks[[as.character(file.attributes[["lsa.cycle"]])]]
    } else if(intersect(file.attributes[["lsa.study"]], names(default.benchmarks)) == "PISA for Development") {
      tmp.PV.root.name <- gsub(pattern = "[[:digit:]]+", replacement = "#", x = vars.list[["PV.root.bench"]], fixed = TRUE)
      tmp.benchmarks <- default.benchmarks[["PISA for Development"]]
      tmp.benchmarks.PVs <- grep(pattern = "root.PVs$", x = names(tmp.benchmarks), value = TRUE)
      tmp.benchmarks.PVs <- names(unlist(sapply(X = tmp.benchmarks.PVs, FUN = function(i) {
        intersect(tmp.benchmarks[[i]], tmp.PV.root.name)
      })))
      tmp.benchmarks.PVs <- gsub(pattern = ".root.PVs", replacement = "", x = tmp.benchmarks.PVs)
      tmp.benchmarks <- default.benchmarks[["PISA for Development"]][[tmp.benchmarks.PVs]]
      bench.vals <- tmp.benchmarks[[as.character(file.attributes[["lsa.cycle"]])]]
    }
  }
  if(bench.type == "discrete") {
    bench.vals <- c(0, rep(sort(bench.vals), times = 2), 2000)
    bench.vals <- split(x = bench.vals, f = rep_len(1:(length(bench.vals)/2), length(bench.vals)))
    names.bench.vals <- lapply(X = bench.vals, FUN = function(i) {
      paste0("From ", i[1], " to below ", i[2])
    })
    names.bench.vals[1] <- paste0("Below ", bench.vals[[1]][2])
    names.bench.vals[length(bench.vals)] <- paste0("At or above ", bench.vals[[length(bench.vals)]][1])
    names.bench.vals <- paste0(1:length(bench.vals), ". ", unlist(names.bench.vals))
    names(bench.vals) <- names.bench.vals
  } else if(bench.type == "cumulative") {
    bench.vals <- as.list(sort(bench.vals))
    names.bench.vals <- paste0(1:length(bench.vals), ". At or above ", unlist(bench.vals))
    names(bench.vals) <- names.bench.vals
  }
  tryCatch({
    if(file.attributes[["lsa.study"]] %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "TiPi") & missing(shortcut)) {
      action.args.list[["shortcut"]] <- FALSE
    }
    data <- produce.analysis.data.table(data.object = data, object.variables = vars.list, action.arguments = action.args.list, imported.file.attributes = file.attributes)
    if(exists("removed.countries.where.any.split.var.is.all.NA") && length(removed.countries.where.any.split.var.is.all.NA) > 0) {
      warnings.collector[["removed.countries.where.any.split.var.is.all.NA"]] <- paste0('Some of the countries had one or more splitting variables which contains only missing values. These countries are: ', paste(removed.countries.where.any.split.var.is.all.NA, collapse = ', '), '.')
    }
    vars.list[["pcts.var"]] <- tmp.pcts.var
    vars.list[["group.vars"]] <- tmp.group.vars
    analysis.info <- list()
    number.of.countries <- length(names(data))
    if(number.of.countries == 1) {
      message("\nValid data from one country have been found. Some computations can be rather intensive. Please be patient.\n")
    } else if(number.of.countries > 1) {
      message("\nValid data from ", number.of.countries, " countries have been found. Some computations can be rather intensive. Please be patient.\n")
    }
    counter <- 0
    compute.all.stats <- function(data) {
      rep.wgts.names <- paste(c("REPWGT", unlist(lapply(X = design.weight.variables[grep("rep.wgts", names(design.weight.variables), value = TRUE)], FUN = function(i) {
        unique(gsub(pattern = "[[:digit:]]*$", replacement = "", x = i))
      }))), collapse = "|")
      rep.wgts.names <- grep(pattern = rep.wgts.names, x = names(data), value = TRUE)
      all.weights <- c(vars.list[["weight.var"]], rep.wgts.names)
      cnt.start.time <- format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS3")
      if(include.missing == FALSE) {
        if(bench.type == "discrete" && !is.null(vars.list[["bckg.var"]])) {
          bckg.var.all.NA <- names(Filter(function(i) {all(is.na(i))}, data))
          if(length(bckg.var.all.NA) > 0) {
            data1 <- copy(data)
            data1[ , (bckg.var.all.NA) := NULL]
            data1 <- na.omit(object = data1)
          } else {
            data1 <- na.omit(object = data)
          }
        } else if(bench.type == "discrete" && is.null(vars.list[["bckg.var"]]) || bench.type == "cumulative") {
          data1 <- copy(data)
          data1 <- na.omit(data1, cols = key.vars)
        }
      } else if (include.missing == TRUE) {
        if(bench.type == "discrete" && !is.null(vars.list[["bckg.var"]])) {
          bckg.avg.vars.all.NA <- names(Filter(function(i) {all(is.na(i))}, data))
          if(length(bckg.avg.vars.all.NA) > 0) {
            data1 <- copy(data)
            data1[ , (bckg.avg.vars.all.NA) := NULL]
            data1 <- na.omit(object = data1, cols = vars.list[["bckg.avg.vars"]][!vars.list[["bckg.avg.vars"]] %in% bckg.avg.vars.all.NA])
          } else {
            data1 <- na.omit(object = data, cols = unlist(vars.list["bckg.avg.vars"]))
          }
        } else if(bench.type == "discrete" && is.null(vars.list[["bckg.var"]]) || bench.type == "cumulative") {
          data1 <- copy(data)
        }
      }
      number.of.cases <- lapply(X = vars.list[["PV.names"]], function(i) {
        tmp.n.cases <- data1[get(vars.list[["weight.var"]]) > 0 , lapply(.SD, function(j) {
          sapply(X = bench.vals, FUN = function(k) {
            if(bench.type == "discrete") {
              length(j[between(x = j, lower = k[1], upper = (k[2] - 0.000000001))])
            } else if(bench.type == "cumulative") {
              length(j[between(x = j, lower = k[1], upper = 2000)])
            }
          })
        }), .SDcols = i, by = key.vars]
        perf.group.col <- as.data.table(names(bench.vals))
        setnames(x = perf.group.col, "Performance_Group")
        tmp.n.cases <- cbind(tmp.n.cases, perf.group.col)
        tmp.n.cases[ , `:=` (n_cases = rowMeans(.SD)), .SDcols = i]
        tmp.n.cases[ , (i) := NULL]
        setkeyv(x = tmp.n.cases, cols = c(key.vars, "Performance_Group"))
      })
      sum.of.weights <- lapply(X = vars.list[["PV.names"]], function(i) {
        lapply(X = i, FUN = function(j) {
          tmp.sum.of.weights <- cbind(data1[ , mget(key.vars)], data1[ , j, with = FALSE], data1[ , mget(all.weights)])
          tmp.sum.of.weights <- lapply(X = bench.vals, FUN = function(k) {
            if(bench.type == "discrete") {
              if(pcts.within == FALSE) {
                tmp.sum.of.weights <- tmp.sum.of.weights[between(x = tmp.sum.of.weights[ , get(grep(pattern = vars.list[["PV.root.bench"]], x = colnames(tmp.sum.of.weights), value = TRUE))], lower = k[1], upper = (k[2] - 0.000000001))]
                tmp.sum.of.weights <- na.omit(tmp.sum.of.weights[ , lapply(.SD, sum), by = key.vars, .SDcols = all.weights])
              } else if (pcts.within == TRUE) {
                if(length(key.vars) > 2) {
                  tmp.sum.of.weights <- split(x = tmp.sum.of.weights, by = key.vars)
                  tmp.sum.of.weights <- Filter(function(l) {nrow(l) > 0}, tmp.sum.of.weights)
                  tmp.sum.of.weights <- lapply(X = tmp.sum.of.weights, FUN = function(l) {
                    l[between(x = l[ , get(grep(pattern = vars.list[["PV.root.bench"]], x = colnames(l), value = TRUE))], lower = k[1], upper = (k[2] - 0.000000001))]
                  })
                  tmp.sum.of.weights <- lapply(tmp.sum.of.weights, function(l) {
                    na.omit(l[ , lapply(.SD, sum), by = key.vars, .SDcols = all.weights])
                  })
                } else if(length(key.vars) <= 2) {
                  tmp.sum.of.weights <- tmp.sum.of.weights[between(x = tmp.sum.of.weights[ , get(grep(pattern = vars.list[["PV.root.bench"]], x = colnames(tmp.sum.of.weights), value = TRUE))], lower = k[1], upper = (k[2] - 0.000000001))]
                  tmp.sum.of.weights <- na.omit(tmp.sum.of.weights[ , lapply(.SD, sum), by = key.vars, .SDcols = all.weights])
                }
              }
            } else if(bench.type == "cumulative") {
              tmp.sum.of.weights <- split(x = tmp.sum.of.weights, by = key.vars, drop = TRUE)
              tmp.sum.of.weights <- lapply(X = tmp.sum.of.weights, FUN = function(l) {
                l[between(x = l[ , get(grep(pattern = vars.list[["PV.root.bench"]], x = colnames(l), value = TRUE))], lower = k[1], upper = 2000)]
              })
              tmp.sum.of.weights <- lapply(X = tmp.sum.of.weights, FUN = function(l) {
                na.omit(l[ , lapply(.SD, sum), by = key.vars, .SDcols = all.weights])
              })
              tmp.sum.of.weights <- rbindlist(tmp.sum.of.weights)
            }
          })
          if(bench.type == "discrete" && pcts.within == FALSE || bench.type == "discrete" && pcts.within == TRUE && length(key.vars) <= 2 || bench.type == "cumulative") {
            tmp.sum.of.weights <- rbindlist(l = lapply(X = seq_along(tmp.sum.of.weights), FUN = function(k) {
              lapply(X = tmp.sum.of.weights[k], FUN = function(l) {
                tmp <- cbind(data.table("Performance_Group" = names.bench.vals[k]), l)
                setcolorder(x = tmp, neworder = c(key.vars, "Performance_Group", all.weights))
                setkeyv(x = tmp, c(key.vars, "Performance_Group"))
                setnames(x = tmp, old = all.weights, new = paste0("V", 1:length(all.weights)))
              })[[1]]
            }))
            tmp.sum.of.weights <- na.omit(object = tmp.sum.of.weights, cols = key.vars[1])
          } else if(bench.type == "discrete" && pcts.within == TRUE && length(key.vars) > 2) {
            tmp.leading.cols <- copy(number.of.cases[[1]])
            tmp.leading.cols <- tmp.leading.cols[ , n_cases := NULL]
            tmp.sum.of.weights <- lapply(X = tmp.sum.of.weights, FUN = function(k) {
              tmp.table <- lapply(X = k, FUN = function(l) {
                tmp.key.cols <- cbind(l[ , mget(key.vars[1:(length(key.vars) - 1)])][1])
                tmp.sums <- l[ , lapply(.SD, sum), .SDcols = all.weights]
                cbind(tmp.key.cols, tmp.sums)
              })
              rbindlist(l = tmp.table)
            })
            last.key.var.table <- unique(number.of.cases[[1]][ , key.vars[length(key.vars)], with = FALSE])
            tmp.sum.of.weights <- lapply(X = tmp.sum.of.weights, FUN = function(k) {
              tmp <- suppressWarnings(cbind(last.key.var.table, k))
              setcolorder(x = tmp, neworder = c(key.vars, all.weights))
              setkeyv(x = tmp, cols = key.vars)
            })
            tmp.names <- lapply(X = as.list(names(tmp.sum.of.weights)), FUN = function(k) {
              data.table(Performance_Group = k)
            })
            tmp.sum.of.weights <- Map(f = cbind, tmp.names, tmp.sum.of.weights)
            tmp.sum.of.weights <- lapply(X = tmp.sum.of.weights, FUN = function(k) {
              setcolorder(x = k, neworder = c(key.vars, "Performance_Group", all.weights))
            })
            tmp.sum.of.weights <- rbindlist(l = tmp.sum.of.weights)
            setkeyv(x = tmp.sum.of.weights, cols = c(key.vars, "Performance_Group"))
            tmp.sum.of.weights <- na.omit(tmp.sum.of.weights)
            tmp.sum.of.weights <- merge(x = tmp.leading.cols, y = tmp.sum.of.weights, all = TRUE)
            setnames(x = tmp.sum.of.weights, old = all.weights, new = paste0("V", 1:length(all.weights)))
            setkeyv(x = tmp.sum.of.weights, cols = c(key.vars, "Performance_Group"))
          }
        })
      })
      number.of.cases.1 <- copy(number.of.cases[[1]])
      missing.rows <- number.of.cases.1[ , n_cases := NULL]
      sum.of.weights <- lapply(X = sum.of.weights, FUN = function(i) {
        lapply(X = i, function(j) {
          tmp <- merge(x = missing.rows, y = j, all = TRUE)
          tmp[ , (grep(pattern = "^V[[:digit:]]+", x = colnames(j), value = TRUE)) := lapply(.SD, function(k) {
            ifelse(test = is.na(k), yes = 0, no = k)
          }), .SDcols = grep(pattern = "^V[[:digit:]]+", x = colnames(j), value = TRUE)]
        })
      })
      sum.of.weights <- lapply(X = sum.of.weights, FUN = function(i) {
        lapply(X = i, function(j) {
          tmp <- merge(x = missing.rows, y = j, all = TRUE)
          tmp[ , (grep(pattern = "^V[[:digit:]]+", x = colnames(j), value = TRUE)) := lapply(.SD, function(k) {
            ifelse(test = is.na(k), yes = 0, no = k)
          }), .SDcols = grep(pattern = "^V[[:digit:]]+", x = colnames(j), value = TRUE)]
        })
      })
      sum.of.weights.1 <- copy(sum.of.weights)
      if(pcts.within == FALSE) {
        PV.bench.percentages <- lapply(X = sum.of.weights.1, function(i) {
          sum.of.all.weights <- data1[ , lapply(.SD, function(j) {
            sum(j)
          }), .SDcols = all.weights, by = key.vars]
          lapply(X = i, FUN = function(j) {
            tmp.key.cols <- j[ , mget(key.vars)]
            sum.of.all.weights <- merge(x = sum.of.all.weights, y = tmp.key.cols)
            sum.of.all.weights[ , (key.vars) := NULL]
            setnames(x = sum.of.all.weights, paste0("V", 1:length(all.weights)))
            tmp.key.cols <- j[ , mget(c(key.vars, "Performance_Group"))]
            j[ , (colnames(tmp.key.cols)) := NULL]
            j <- (j/sum.of.all.weights)*100
            j <- cbind(tmp.key.cols, j)
            setkeyv(x = j, c(key.vars, "Performance_Group"))
          })
        })
      } else if(pcts.within == TRUE) {
        if(bench.type == "discrete" && length(key.vars) <= 2) {
          sum.of.all.weights <- lapply(X = bench.vals, FUN = function(i) {
            tmp.sum.of.all.weights <- lapply(X = vars.list[["PV.names"]], FUN = function(j) {
              summary.per.benchmark <- lapply(X = j, FUN = function(k) {
                tmp <- cbind(data1[ , mget(key.vars)], data1[ , k, with = FALSE], data1[ , mget(all.weights)])
                tmp <- tmp[between(x = tmp[ , get(k)], lower = i[1], upper = (i[2] - 0.000000001))]
                tmp <- tmp[ , lapply(.SD, sum), .SDcols = all.weights]
              })
              summary.per.benchmark <- cbind(data.table(g = 1:length(summary.per.benchmark)), rbindlist(l = summary.per.benchmark))
            })[[1]]
          })
          sum.of.all.weights <- rbindlist(l = sum.of.all.weights)
          setkeyv(x = sum.of.all.weights, cols = "g")
          sum.of.all.weights <- split(x = sum.of.all.weights, by = "g")
          sum.of.all.weights <- list(lapply(X = sum.of.all.weights, FUN = function(i) {
            i <- i[ , g := NULL]
            setnames(x = i, old = all.weights, new = paste0("V", 1:length(all.weights)))
          }))
          tmp.key.cols <- lapply(X = sum.of.weights.1, FUN = function(i) {
            lapply(X = i, FUN = function(j) {
              j <- j[ , mget(key.vars)]
              setkeyv(x = j, cols = key.vars)
            })
          })
          sum.of.all.weights <- Map(f = function(input1, input2) {
            suppressWarnings(Map(f = cbind, input1, input2))
          }, input1 = tmp.key.cols, input2 = sum.of.all.weights)
          sum.of.all.weights <- lapply(X = sum.of.all.weights, FUN = function(i) {
            lapply(X = i, FUN = function(j) {
              j[ , (key.vars) := NULL]
            })
          })
        } else if(bench.type == "discrete" && length(key.vars) > 2) {
          sum.of.all.weights <- lapply(X = bench.vals, FUN = function(i) {
            tmp.sum.of.all.weights <- lapply(X = vars.list[["PV.names"]], FUN = function(j) {
              summary.per.benchmark <- lapply(X = j, FUN = function(k) {
                tmp <- cbind(data1[ , mget(key.vars)], data1[ , k, with = FALSE], data1[ , mget(all.weights)])
                tmp <- split(x = tmp, by = key.vars[1:(length(key.vars) - 1)])
                tmp <- Filter(function(l) {nrow(l) > 0}, tmp)
                tmp <- lapply(X = tmp, FUN = function(l) {
                  l[between(x = l[ , get(k)], lower = i[1], upper = (i[2] - 0.000000001))]
                })
                tmp.leading.colnames <- lapply(X = tmp, FUN = function(l) {
                  unique(l[ , 1:(length(key.vars) - 1)])
                })
                tmp <- lapply(X = tmp, FUN = function(l) {
                  cbind(data.table(pv = k), l[ , lapply(.SD, sum), .SDcols = all.weights])
                })
                tmp <- Map(f = cbind, tmp.leading.colnames, tmp)
                tmp <- rbindlist(l = tmp)
                if(any(is.na(tmp[ , mget(key.vars[1:(length(key.vars) - 1)])]))) {
                  missing.rows.1 <- unique(missing.rows[ , mget(key.vars[1:(length(key.vars) - 1)])])
                  tmp[ , (key.vars[1:(length(key.vars) - 1)]) := NULL]
                  tmp <- cbind(missing.rows.1, tmp)
                } else {
                  tmp
                }
              })
              summary.per.benchmark <- rbindlist(l = summary.per.benchmark)
            })[[1]]
          })
          bench.vals.tables <- lapply(X = names(bench.vals), FUN = function(i) {
            data.table(Performance_Group = i)
          })
          sum.of.all.weights <- Map(f = cbind, bench.vals.tables, sum.of.all.weights)
          sum.of.all.weights <- lapply(X = sum.of.all.weights, FUN = function(i) {
            setcolorder(x = i, neworder = c(key.vars[1:(length(key.vars) - 1)], "Performance_Group", "pv"))
            setkeyv(x = i, cols = c(key.vars[1:(length(key.vars) - 1)], "Performance_Group", "pv"))
          })
          sum.of.all.weights <- rbindlist(l = sum.of.all.weights)
          sum.of.all.weights <- split(x = sum.of.all.weights, by = "pv")
          sum.of.all.weights <- lapply(X = sum.of.all.weights, FUN = function(i) {
            split(x = i, by = c(key.vars[1:(length(key.vars) - 1)]), drop = TRUE)
          })
          sum.of.all.weights <- lapply(X = sum.of.all.weights, FUN = function(i) {
            lapply(X = i, FUN = function(j) {
              do.call("rbind", replicate(n = length(unique(number.of.cases.1[ , get(key.vars[length(key.vars)])])), expr = j, simplify = FALSE))
            })
          })
          sum.of.all.weights <- lapply(X = sum.of.all.weights, FUN = function(i) {
            rbindlist(l = i)
          })
          last.key.var.col.table <- data.table(rep(unique(data1[ , get(key.vars[length(key.vars)])]), each = length(bench.vals)))
          setnames(x = last.key.var.col.table, key.vars[length(key.vars)])
          tmp.leading.cols <- lapply(X = sum.of.all.weights, FUN = function(i) {
            tmp <- cbind(i[ , mget(c(key.vars[1:(length(key.vars) - 1)]))], last.key.var.col.table, data.table(Performance_Group = names(bench.vals)))
            setkeyv(x = tmp, cols = c(key.vars, "Performance_Group"))
          })
          sum.of.all.weights <- lapply(X = sum.of.all.weights, FUN = function(i) {
            i[ , (c(key.vars[1:(length(key.vars) - 1)], "Performance_Group", "pv")) := NULL]
          })
          sum.of.all.weights <- list(lapply(X = sum.of.all.weights, FUN = function(i) {
            setnames(x = i, old = all.weights, new = paste0("V", 1:length(all.weights)))
          }))
        }
        if(length(key.vars) <= 2) {
          tmp.key.cols <- lapply(X = sum.of.weights.1, FUN = function(i) {
            lapply(X = i, FUN = function(j) {
              j[ , mget(c(key.vars, "Performance_Group"))]
            })
          })
        } else if(length(key.vars) > 2) {
          tmp.key.cols <- list(tmp.leading.cols)
          sum.of.weights.1 <- Map(f = function(input1, input2) {
            Map(f = merge, input1, input2, all = TRUE)
          }, input1 = tmp.key.cols, input2 = sum.of.weights.1)
        }
        sum.of.weights.1 <- lapply(X = sum.of.weights.1, FUN = function(i) {
          lapply(X = i, FUN = function(j) {
            j[ , (c(key.vars, "Performance_Group")) := NULL]
          })
        })
        PV.bench.percentages <- Map(f = function(input1, input2) {
          Map(f = `/`, input1, input2)
        }, input1 = sum.of.weights.1, input2 = sum.of.all.weights)
        PV.bench.percentages <- lapply(X = PV.bench.percentages, FUN = function(i) {
          lapply(X = i, FUN = function(j) {
            j <- data.table(j)
            j[ , (colnames(j)) := lapply(.SD, function(k) {
              k*100
            })]
          })
        })
        PV.bench.percentages <- Map(f = function(input1, input2) {
          Map(f = cbind, input1, input2)
        }, input1 = tmp.key.cols, input2 = PV.bench.percentages)
      }
      sum.of.weights.1 <- NULL
      lapply(X = PV.bench.percentages, FUN = function(i) {
        lapply(X = i, FUN = function(j) {
          j[ , (grep(pattern = "^V[[:digit:]]+", x = colnames(j), value = TRUE)) := lapply(.SD, function(k) {
            ifelse(test = is.nan(k), yes = 0, no = k)
          }), .SDcols = grep(pattern = "^V[[:digit:]]+", x = colnames(j), value = TRUE)]
        })
      })
      if(bench.type == "discrete" && !is.null(vars.list[["bckg.var"]])) {
        bckg.means <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
          tmp.data.split.obj <- lapply(X = i, function(j) {
            if(bckg.var %in% colnames(data1)) {
              tmp <- cbind(data1[ , mget(key.vars)], data1[ , j, with = FALSE], data1[ , mget(bckg.var)], data1[ , mget(all.weights)])
              tmp <- lapply(X = bench.vals, FUN = function(k) {
                tmp[between(x = tmp[ , get(grep(pattern = j, x = colnames(tmp), value = TRUE))], lower = k[1], upper = (k[2] - 0.000000001))]
              })
              bench.names <- as.list(names(tmp))
              bench.names <- lapply(X = bench.names, FUN = function(k) {
                data.table(Performance_Group = k)
              })
              tmp <- Map(f = cbind, bench.names, tmp)
              lapply(X = tmp, FUN = function(k) {
                setcolorder(x = k, neworder = c(key.vars, "Performance_Group", grep(pattern = paste(c(key.vars, "Performance_Group"), collapse = "|"), x = colnames(k), value = TRUE, invert = TRUE)))
                setkeyv(x = k, cols = c(key.vars, "Performance_Group"))
              })
              tmp <- lapply(X = tmp, FUN = function(k) {
                lapply(X = vars.list[["bckg.var"]], FUN = compute.multiple.means.all.repwgt, data.object = k, weight.var = all.weights, keys = key.vars, include.missing.arg = action.args.list[["include.missing"]])[[1]]
              })
              tmp <- Map(f = cbind, bench.names, tmp)
              lapply(X = tmp, FUN = function(k) {
                setcolorder(x = k, neworder = c(key.vars, "Performance_Group", grep(pattern = paste(c(key.vars, "Performance_Group"), collapse = "|"), x = colnames(k), value = TRUE, invert = TRUE)))
                setkeyv(x = k, cols = c(key.vars, "Performance_Group"))
              })
              tmp <- unique(rbindlist(l = tmp), by = c(key.vars, "Performance_Group"))
              setkeyv(x = tmp, cols = c(key.vars, "Performance_Group"))
            } else {
              tmp <- unique(data1[ , mget(key.vars)])
              tmp <- suppressWarnings(split(x = tmp, f = key.vars))
              bench.names <- list(data.table(Performance_Group = names(bench.vals)))
              tmp <- suppressWarnings(rbindlist(l = Map(f = cbind, tmp, bench.names)))
              tmp1 <- setDT(as.list(paste(paste0("V", 1:length(all.weights)))))
              tmp1[ , colnames(tmp1) := lapply(.SD, function(k) {
                k <- NA
                as.numeric(k)
              }), .SDcols = colnames(tmp1)]
              tmp <- cbind(tmp, tmp1)
              setkeyv(x = tmp, cols = c(key.vars, "Performance_Group"))
            }
          })
        })
        bckg.variances <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
          tmp.data.split.obj <- lapply(X = i, function(j) {
            if(bckg.var %in% colnames(data1)) {
              tmp <- cbind(data1[ , mget(key.vars)], data1[ , j, with = FALSE], data1[ , mget(bckg.var)], data1[ , mget(all.weights)])
              tmp <- lapply(X = bench.vals, FUN = function(k) {
                tmp[between(x = tmp[ , get(grep(pattern = vars.list[["PV.root.bench"]], x = colnames(tmp), value = TRUE))], lower = k[1], upper = (k[2] - 0.000000001))]
              })
              bench.names <- as.list(names(tmp))
              bench.names <- lapply(X = bench.names, FUN = function(k) {
                data.table(Performance_Group = k)
              })
              tmp <- Map(f = cbind, bench.names, tmp)
              lapply(X = tmp, FUN = function(k) {
                setcolorder(x = k, neworder = c(key.vars, "Performance_Group", grep(pattern = paste(c(key.vars, "Performance_Group"), collapse = "|"), x = colnames(k), value = TRUE, invert = TRUE)))
                setkeyv(x = k, cols = c(key.vars, "Performance_Group"))
              })
              tmp <- lapply(X = tmp, FUN = function(k) {
                lapply(X = vars.list[["bckg.var"]], FUN = compute.dispersion.all.repwgt, dispersion.type = "variance", data.object = k, weight.var = all.weights, keys = key.vars, include.missing.arg = action.args.list[["include.missing"]])[[1]]
              })
              tmp <- Map(f = cbind, bench.names, tmp)
              lapply(X = tmp, FUN = function(k) {
                setcolorder(x = k, neworder = c(key.vars, "Performance_Group", grep(pattern = paste(c(key.vars, "Performance_Group"), collapse = "|"), x = colnames(k), value = TRUE, invert = TRUE)))
                setkeyv(x = k, cols = c(key.vars, "Performance_Group"))
              })
              tmp <- unique(rbindlist(l = tmp), by = c(key.vars, "Performance_Group"))
              setkeyv(x = tmp, cols = c(key.vars, "Performance_Group"))
            } else {
              tmp <- unique(data1[ , mget(key.vars)])
              tmp <- suppressWarnings(split(x = tmp, f = key.vars))
              bench.names <- list(data.table(Performance_Group = names(bench.vals)))
              tmp <- suppressWarnings(rbindlist(l = Map(f = cbind, tmp, bench.names)))
              tmp1 <- setDT(as.list(paste(paste0("V", 1:length(all.weights)))))
              tmp1[ , colnames(tmp1) := lapply(.SD, function(k) {
                k <- NA
                as.numeric(k)
              }), .SDcols = colnames(tmp1)]
              tmp <- cbind(tmp, tmp1)
              setkeyv(x = tmp, cols = c(key.vars, "Performance_Group"))
            }
          })
        })
        bckg.SDs <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
          tmp.data.split.obj <- lapply(X = i, function(j) {
            if(bckg.var %in% colnames(data1)) {
              tmp <- cbind(data1[ , mget(key.vars)], data1[ , j, with = FALSE], data1[ , mget(bckg.var)], data1[ , mget(all.weights)])
              tmp <- lapply(X = bench.vals, FUN = function(k) {
                tmp[between(x = tmp[ , get(grep(pattern = vars.list[["PV.root.bench"]], x = colnames(tmp), value = TRUE))], lower = k[1], upper = (k[2] - 0.000000001))]
              })
              bench.names <- as.list(names(tmp))
              bench.names <- lapply(X = bench.names, FUN = function(k) {
                data.table(Performance_Group = k)
              })
              tmp <- Map(f = cbind, bench.names, tmp)
              lapply(X = tmp, FUN = function(k) {
                setcolorder(x = k, neworder = c(key.vars, "Performance_Group", grep(pattern = paste(c(key.vars, "Performance_Group"), collapse = "|"), x = colnames(k), value = TRUE, invert = TRUE)))
                setkeyv(x = k, cols = c(key.vars, "Performance_Group"))
              })
              tmp <- lapply(X = tmp, FUN = function(k) {
                lapply(X = vars.list[["bckg.var"]], FUN = compute.dispersion.all.repwgt, dispersion.type = "SD", data.object = k, weight.var = all.weights, keys = key.vars, include.missing.arg = action.args.list[["include.missing"]])[[1]]
              })
              tmp <- Map(f = cbind, bench.names, tmp)
              lapply(X = tmp, FUN = function(k) {
                setcolorder(x = k, neworder = c(key.vars, "Performance_Group", grep(pattern = paste(c(key.vars, "Performance_Group"), collapse = "|"), x = colnames(k), value = TRUE, invert = TRUE)))
                setkeyv(x = k, cols = c(key.vars, "Performance_Group"))
              })
              tmp <- unique(rbindlist(l = tmp), by = c(key.vars, "Performance_Group"))
              setkeyv(x = tmp, cols = c(key.vars, "Performance_Group"))
            } else {
              tmp <- unique(data1[ , mget(key.vars)])
              tmp <- suppressWarnings(split(x = tmp, f = key.vars))
              bench.names <- list(data.table(Performance_Group = names(bench.vals)))
              tmp <- suppressWarnings(rbindlist(l = Map(f = cbind, tmp, bench.names)))
              tmp1 <- setDT(as.list(paste(paste0("V", 1:length(all.weights)))))
              tmp1[ , colnames(tmp1) := lapply(.SD, function(k) {
                k <- NA
                as.numeric(k)
              }), .SDcols = colnames(tmp1)]
              tmp <- cbind(tmp, tmp1)
              setkeyv(x = tmp, cols = c(key.vars, "Performance_Group"))
            }
          })
        })
        bckg.var.pct.miss <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
          tmp.data.split.obj <- lapply(X = i, function(j) {
            tmp <- cbind(data[ , mget(key.vars)], data[ , j, with = FALSE], data[ , mget(bckg.var)], data[ , mget(all.weights)])
            tmp <- lapply(X = bench.vals, FUN = function(k) {
              tmp[between(x = tmp[ , get(grep(pattern = vars.list[["PV.root.bench"]], x = colnames(tmp), value = TRUE))], lower = k[1], upper = (k[2] - 0.000000001))]
            })
            tmp.bench.names <- as.list(names(tmp))
            tmp <- Map(f = cbind, tmp.bench.names, tmp)
            tmp <- lapply(X = tmp, FUN = function(j) {
              setnames(x = j, old = "V1", new = "Performance_Group")
              setcolorder(x = j, neworder = c(key.vars, "Performance_Group", grep(pattern = paste(c(key.vars, "Performance_Group"), collapse = "|"), x = colnames(j), value = TRUE, invert = TRUE)))
            })
            tmp <- lapply(X = tmp, FUN = function(k) {
              lapply(X = vars.list[["bckg.var"]], FUN = compute.cont.vars.pct.miss, data.object = k, weight.var = vars.list[["weight.var"]], keys = c(key.vars, "Performance_Group"))[[1]]
            })
            tmp <- rbindlist(l = tmp)
            setkeyv(x = tmp, cols = c(key.vars, "Performance_Group"))
          })
        })
        if(include.missing == FALSE) {
          bckg.var.pct.miss <- lapply(X = bckg.var.pct.miss, FUN = function(i) {
            lapply(X = i, FUN = function(j) {
              na.omit(object = j, cols = key.vars)
            })
          })
        } else {
          bckg.var.pct.miss <- bckg.var.pct.miss
        }
      }
      reshape.list.statistics.PV(estimate.object = sum.of.weights, estimate.name = "Sum_", PV.vars.vector = vars.list[["PV.names"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
      reshape.list.statistics.PV(estimate.object = PV.bench.percentages, estimate.name = "Percentages_", PV.vars.vector = vars.list[["PV.names"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
      if(bench.type == "discrete" && !is.null(vars.list[["bckg.var"]])) {
        reshape.list.statistics.PV(estimate.object = bckg.means, estimate.name = "Mean_", PV.vars.vector = vars.list[["PV.names"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
        bckg.means <- lapply(X = bckg.means, FUN = function(i) {
          lapply(X = i, FUN = function(j) {
            merge(x = number.of.cases.1, y = j, all = TRUE)
          })
        })
        reshape.list.statistics.PV(estimate.object = bckg.variances, estimate.name = "Variance_", PV.vars.vector = vars.list[["PV.names"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
        bckg.variances <- lapply(X = bckg.variances, FUN = function(i) {
          lapply(X = i, FUN = function(j) {
            merge(x = number.of.cases.1, y = j, all = TRUE)
          })
        })
        reshape.list.statistics.PV(estimate.object = bckg.SDs, estimate.name = "SD_", PV.vars.vector = vars.list[["PV.names"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
        bckg.SDs <- lapply(X = bckg.SDs, FUN = function(i) {
          lapply(X = i, FUN = function(j) {
            merge(x = number.of.cases.1, y = j, all = TRUE)
          })
        })
        bckg.var.pct.miss <- lapply(X = bckg.var.pct.miss, FUN = function(i) {
          tmp.key <- key(i[[1]])
          tmp <- rbindlist(l = i)
          setkeyv(x = tmp, cols = tmp.key)
        })
        bckg.var.pct.miss <- lapply(X = bckg.var.pct.miss, FUN = function(i) {
          pct.missing.colname <- grep(pattern = "Percent_Missing_", x = colnames(i), value = TRUE)
          tmp <- i[ , mean(get(pct.missing.colname)), by = key(i)]
          setnames(x = tmp, old = "V1", new = pct.missing.colname)
        })
        bckg.means <- lapply(X = bckg.means, FUN = function(i) {
          Reduce(function(...) merge(...), i)
        })
        bckg.variances <- lapply(X = bckg.variances, FUN = function(i) {
          Reduce(function(...) merge(...), i)
        })
        bckg.SDs <- lapply(X = bckg.SDs, FUN = function(i) {
          Reduce(function(...) merge(...), i)
        })
      }
      sum.of.weights <- lapply(X = sum.of.weights, FUN = function(i) {
        lapply(X = i, FUN = function(j) {
          unique(x = j, by = c(key.vars, "Performance_Group"))
        })
      })
      PV.bench.percentages <- lapply(X = PV.bench.percentages, FUN = function(i) {
        lapply(X = i, FUN = function(j) {
          unique(x = j, by = c(key.vars, "Performance_Group"))
        })
      })
      sum.of.weights <- lapply(X = sum.of.weights, FUN = function(i) {
        Reduce(function(...) merge(...), i)
      })
      PV.bench.percentages <- lapply(X = PV.bench.percentages, FUN = function(i) {
        Reduce(function(...) merge(...), i)
      })
      aggregate.PV.estimates(estimate.object = sum.of.weights, estimate.name = "Sum_", root.PV = vars.list[["PV.root.bench"]], PV.vars.vector = vars.list[["PV.names"]], data.key.variables = key.vars, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
      aggregate.PV.estimates(estimate.object = PV.bench.percentages, estimate.name = "Percentages_", root.PV = vars.list[["PV.root.bench"]], PV.vars.vector = vars.list[["PV.names"]], data.key.variables = key.vars, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
      sum.of.weights <- lapply(X = sum.of.weights, FUN = function(i) {
        i[ , grep(pattern = "_SVR|_MVR", x = colnames(i)) := NULL]
      })
      sum.of.weights <- lapply(X = sum.of.weights, FUN = function(i) {
        if(file.attributes[["lsa.study"]] %in% c("PISA", "PISA for Development", "ICCS", "ICILS")) {
          tmp.PV.root <- gsub(pattern = "[[:digit:]]+", replacement = "N", x = vars.list[["PV.root.bench"]], fixed = TRUE)
          setnames(x = i, old = grep(pattern = tmp.PV.root, x = colnames(i), value = TRUE), new = c(paste0("Sum_", vars.list[["weight.var"]]), paste0("Sum_", vars.list[["weight.var"]], "_SE")))
        } else {
          setnames(x = i, old = grep(pattern = vars.list[["PV.root.bench"]], x = colnames(i), value = TRUE), new = c(paste0("Sum_", vars.list[["weight.var"]]), paste0("Sum_", vars.list[["weight.var"]], "_SE")))
        }
      })
      PV.bench.percentages <- lapply(X = PV.bench.percentages, FUN = function(i) {
        i[ , grep(pattern = "_SVR|_MVR", x = colnames(i)) := NULL]
      })
      PV.bench.percentages <- lapply(X = PV.bench.percentages, FUN = function(i) {
        if(file.attributes[["lsa.study"]] %in% c("PISA", "PISA for Development", "ICCS", "ICILS")) {
          i
        } else {
          setnames(x = i, old = grep(pattern = vars.list[["PV.root.bench"]], x = colnames(i), value = TRUE), new = c(paste0("Percentages_", vars.list[["PV.root.bench"]]), paste0("Percentages_", vars.list[["PV.root.bench"]], "_SE")))
        }
      })
      number.of.cases <- number.of.cases[[1]]
      sum.of.weights <- sum.of.weights[[1]]
      PV.bench.percentages <- PV.bench.percentages[[1]]
      setkeyv(x = sum.of.weights, cols = c(key.vars, "Performance_Group"))
      setkeyv(x = PV.bench.percentages, cols = c(key.vars, "Performance_Group"))
      if(bench.type == "discrete" && !is.null(vars.list[["bckg.var"]])) {
        aggregate.PV.estimates(estimate.object = bckg.means, estimate.name = "Mean_", root.PV = vars.list[["PV.root.bench"]], PV.vars.vector = vars.list[["PV.names"]], data.key.variables = c(key.vars), study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
        aggregate.PV.estimates(estimate.object = bckg.variances, estimate.name = "Variance_", root.PV = vars.list[["PV.root.bench"]], PV.vars.vector = vars.list[["PV.names"]], data.key.variables = c(key.vars), study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
        aggregate.PV.estimates(estimate.object = bckg.SDs, estimate.name = "SD_", root.PV = vars.list[["PV.root.bench"]], PV.vars.vector = vars.list[["PV.names"]], data.key.variables = c(key.vars), study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
        replace.PV.names.with.bckg <- function(estimate) {
          lapply(X = seq_along(vars.list[["bckg.var"]]), FUN = function(i) {
            estimate[[i]][ , grep(pattern = "_SVR$|_MVR$", x = colnames(estimate[[i]]), value = TRUE) := NULL]
            if(file.attributes[["lsa.study"]] %in% c("PISA", "PISA for Development", "ICCS", "ICILS")) {
              PV.root.cols <- grep(pattern = gsub(pattern = "[[:digit:]]+", x = vars.list[["PV.root.bench"]], replacement = "N", fixed = TRUE), x = colnames(estimate[[i]]), value = TRUE)
              bckg.col.name <- gsub(pattern = gsub(pattern = "[[:digit:]]+", x = vars.list[["PV.root.bench"]], replacement = "N", fixed = TRUE), replacement = vars.list[["bckg.var"]][i], x = PV.root.cols)
            } else {
              PV.root.cols <- grep(pattern = vars.list[["PV.root.bench"]], x = colnames(estimate[[i]]), value = TRUE)
              bckg.col.name <- gsub(pattern = vars.list[["PV.root.bench"]], replacement = vars.list[["bckg.var"]][i], x = PV.root.cols)
            }
            setnames(x = estimate[[i]], old = PV.root.cols, new = bckg.col.name)
            setkeyv(x = estimate[[i]], cols = c(key.vars, "Performance_Group"))
          })
        }
        bckg.means <- replace.PV.names.with.bckg(estimate = bckg.means)
        bckg.variances <- replace.PV.names.with.bckg(estimate = bckg.variances)
        bckg.SDs <- replace.PV.names.with.bckg(estimate = bckg.SDs)
        bckg.means <- bckg.means[[1]]
        bckg.variances <- bckg.variances[[1]]
        bckg.SDs <- bckg.SDs[[1]]
        bckg.var.pct.miss <- bckg.var.pct.miss[[1]]
      }
      country.analysis.info <- produce.analysis.info(cnt.ID = unique(data[ , get(key.vars[1])]), data = used.data, study = file.attributes[["lsa.study"]], cycle = file.attributes[["lsa.cycle"]], weight.variable = vars.list[["weight.var"]], rep.design = DESIGN, used.shortcut = shortcut, number.of.reps = rep.wgts.names, in.time = cnt.start.time)
      analysis.info[[country.analysis.info[ , COUNTRY]]] <<- country.analysis.info
      if(is.null(vars.list[["bckg.var"]])) {
        merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, PV.bench.percentages))
      } else if(!is.null(vars.list[["bckg.var"]]) && bench.type == "discrete") {
        merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, PV.bench.percentages, bckg.means, bckg.variances, bckg.SDs, bckg.var.pct.miss))
        merged.outputs <- unique(merged.outputs)
        merged.outputs[ , (grep(pattern = "^Mean_|^Variance_|^SD_|^Percent_Missing_", x = colnames(merged.outputs), value = TRUE)) := lapply(.SD, function(i) {
          ifelse(test = is.na(i), yes = NaN, no = i)
        }), .SDcols = grep(pattern = "^Mean_|^Variance_|^SD_|^Percent_Missing_", x = colnames(merged.outputs), value = TRUE)]
        merged.outputs <- merged.outputs[!is.na(get(key.vars[1])), ]
      } else if(!is.null(vars.list[["bckg.var"]]) && bench.type == "cumulative") {
        merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, PV.bench.percentages))
      }
      counter <<- counter + 1
      message("     ",
              if(nchar(counter) == 1) {
                paste0("( ", counter, "/", number.of.countries, ")   ")
              } else if(nchar(counter) == 2) {
                paste0("(", counter, "/", number.of.countries, ")   ")
              },
              paste0(str_pad(string = unique(merged.outputs[[1]]), width = 40, side = "right"), "processed in ", country.analysis.info[ , DURATION]))
      return(merged.outputs)
    }
    estimates <- rbindlist(lapply(X = data, FUN = compute.all.stats))
    estimates[ , colnames(estimates)[1] := as.character(estimates[ , get(colnames(estimates)[1])])]
    setkeyv(x = estimates, cols = key.vars)
    total.exec.time <- rbindlist(analysis.info)[ , DURATION]
    total.exec.time.millisec <- sum(as.numeric(str_extract(string = total.exec.time, pattern = "[[:digit:]]{3}$")))/1000
    total.exec.time <- sum(as.ITime(total.exec.time), total.exec.time.millisec)
    if(length(unique(estimates[ , get(key.vars[1])])) > 1) {
      message("\nAll ", length(unique(estimates[ , get(key.vars[1])])), " countries with valid data processed in ", format(as.POSIXct("0001-01-01 00:00:00") + total.exec.time - 1, "%H:%M:%OS3"), "\n")
    } else {
      message("\n")
    }
    ptm.add.table.average <- proc.time()
    estimates <- compute.table.average(output.obj = estimates, object.variables = vars.list, data.key.variables = c(key.vars, "Performance_Group"), data.properties = file.attributes)
    message('"Table Average" added to the estimates in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.add.table.average}[[3]], "%H:%M:%OS3"))
    if(isFALSE(graphs)) {
      message("")
    }
    if(isTRUE(graphs)) {
      ptm.add.graphs <- proc.time()
      graphs.object <- copy(x = estimates[get(key.vars[1]) != "Table Average", mget(c(key.vars, "Performance_Group", grep(pattern = "^Percentages_|^Mean_", x = colnames(estimates), value = TRUE)))])
      graphs.object <- split(x = graphs.object, by = key.vars[1], drop = TRUE)
      if(length(key.vars) > 1) {
        lapply(X = graphs.object, FUN = function(i) {
          i[ , perf_group := str_extract(string = Performance_Group, pattern = "^[[:digit:]]+\\.")]
        })
        lapply(X = graphs.object, FUN = function(i) {
          i[ , collapsed_split := factor(do.call(paste, c(.SD, sep = " // "))), .SDcols = c("perf_group", key.vars[2:length(key.vars)])]
          i[ , collapsed_split := factor(x = str_wrap(string = collapsed_split, width = 40), levels = str_wrap(string = collapsed_split, width = 40))]
          i[ , perf_group := NULL]
        })
      }
      if(!is.null(vars.list[["bckg.var"]])) {
        mean.NAs.only.vars.cnt <- lapply(X = graphs.object, FUN = function(i) {
          mean.cols <- grep(pattern = vars.list[["bckg.var"]], x = colnames(i), value = TRUE)
          mean.cols <- grep(pattern = "_SE$", x = mean.cols, value = TRUE, invert = TRUE)
          any.NAs.cnt.mean <- lapply(X = mean.cols, FUN = function(j) {
            if(any(is.na(i[ , get(j)])) == TRUE) {
              unique(i[ , get(key.vars[1])])
            }
          })
        })
        mean.NAs.only.vars.cnt <- unique(unlist(mean.NAs.only.vars.cnt))
        if(length(mean.NAs.only.vars.cnt)) {
          if(graphs == FALSE) {
            warnings.collector[["cnt.NAs.on.analysis.vars"]] <- paste0("In one or more countries the computed mean for the background variable resulted in missing values, no statistics are computed. Check if the variable contained only missings: ", paste(unique(unlist(mean.NAs.only.vars.cnt)), collapse = ", "), ".")
          } else {
            warnings.collector[["cnt.NAs.on.analysis.vars"]] <- paste0("In one or more countries the computed mean for the background variable resulted in missing values, no statistics are computed and no graph is produced. Check if the variable contained only missings: ", paste(unique(unlist(mean.NAs.only.vars.cnt)), collapse = ", "), ".")
          }
        }
      }
      perc.graphs.list <- produce.percentages.plots(data.obj = graphs.object, split.vars.vector = key.vars, type = "bench")
      if(!is.null(vars.list[["bckg.var"]]) && bench.type != "cumulative") {
        means.graphs.list <- produce.means.plots(data.obj = graphs.object, estimates.obj = estimates, split.vars.vector = key.vars, type = "bench")
      }
    }
    if(isTRUE(save.output)) {
      if(isFALSE(graphs)) {
        export.results(output.object = estimates, analysis.type = action.args.list[["executed.analysis.function"]], add.graphs = FALSE, analysis.info.obj = rbindlist(l = analysis.info), destination.file = output.file, open.exported.file = open.output, warns.list = unlist(warnings.collector))
      } else {
        save.graphs(out.path = action.args.list[["output.file"]])
        if(exists("means.plots.files")) {
          export.results(output.object = estimates, analysis.type = action.args.list[["executed.analysis.function"]], add.graphs = TRUE, perc.graphs = percentage.plots.files, non.perc.graphs = means.plots.files, analysis.info.obj = rbindlist(l = analysis.info), destination.file = output.file, open.exported.file = open.output, warns.list = unlist(warnings.collector))
        } else {
          export.results(output.object = estimates, analysis.type = action.args.list[["executed.analysis.function"]], add.graphs = TRUE, perc.graphs = percentage.plots.files, analysis.info.obj = rbindlist(l = analysis.info), destination.file = output.file, open.exported.file = open.output, warns.list = unlist(warnings.collector))
        }
        delete.graphs(out.path = action.args.list[["output.file"]])
      }
      if(exists("perc.graphs.list") || exists("means.graphs.list")) {
        message('Graphs for the estimates produced in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.add.graphs}[[3]], "%H:%M:%OS3"), "\n")
      }
    } else if(isFALSE(save.output)) {
      if(missing(graphs) || graphs == FALSE) {
        if(length(warnings.collector) == 0) {
          return(list(Estimates = estimates, `Analysis information` = rbindlist(l = analysis.info)))
        } else {
          return(list(Estimates = estimates, `Analysis information` = rbindlist(l = analysis.info), Warnings = unlist(unname(warnings.collector))))
        }
      } else if(graphs == TRUE) {
        if(exists("perc.graphs.list") || exists("means.graphs.list")) {
          message('Graphs for the estimates produced in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.add.graphs}[[3]], "%H:%M:%OS3"), "\n")
        }
        if(length(vars.list[["bckg.var"]]) > 0 && bench.type != "cumulative") {
          if(length(warnings.collector) == 0) {
            return(list(Estimates = estimates, `Analysis information` = rbindlist(l = analysis.info), `Percentage graphs` = perc.graphs.list, `Means graphs` = means.graphs.list))
          } else {
            return(list(Estimates = estimates, `Analysis information` = rbindlist(l = analysis.info), `Percentage graphs` = perc.graphs.list, `Means graphs` = means.graphs.list, Warnings = unlist(unname(warnings.collector))))
          }
        } else {
          if(length(warnings.collector) == 0) {
            return(list(Estimates = estimates, `Analysis information` = rbindlist(l = analysis.info), `Percentage graphs` = perc.graphs.list))
          } else {
            return(list(Estimates = estimates, `Analysis information` = rbindlist(l = analysis.info), `Percentage graphs` = perc.graphs.list, Warnings = unlist(unname(warnings.collector))))
          }
        }
      }
    }
  }, interrupt = function(f) {
    message("\nInterrupted by the user. Computations are not finished and output file is not produced.\n")
  })
  if(length(warnings.collector) > 0) {
    if(!is.null(warnings.collector[["removed.countries.where.any.split.var.is.all.NA"]])) {
      warning(warnings.collector[["removed.countries.where.any.split.var.is.all.NA"]], call. = FALSE)
    }
    if(!is.null(warnings.collector[["vars.list.analysis.vars"]])) {
      warning(warnings.collector[["vars.list.analysis.vars"]], call. = FALSE)
    }
    if(!is.null(warnings.collector[["cumulative.pcts.within"]])) {
      warning(warnings.collector[["cumulative.pcts.within"]], call. = FALSE)
    }
    if(!is.null(warnings.collector[["cumulative.bckg.var"]])) {
      warning(warnings.collector[["cumulative.bckg.var"]], call. = FALSE)
    }
    if(length(warnings.collector[["cnt.NAs.on.analysis.vars"]]) > 0) {
      warning(warnings.collector[["cnt.NAs.on.analysis.vars"]], call. = FALSE)
    }
  }
}
