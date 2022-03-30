#' @title Compute correlations between variables within specified groups
#'
#' @description \code{lsa.corr} computes correlation coefficients between variables within groups defined by one or more variables.
#'
#' @param data.file       The file containing \code{lsa.data} object. Either this or \code{data.object}
#'                        shall be specified, but not both. See details.
#' @param data.object     The object in the memory containing \code{lsa.data} object. Either this or
#'                        \code{data.file} shall be specified, but not both. See details.
#' @param split.vars      Categorical variable(s) to split the results by. If no split variables are
#'                        provided, the results will be for the overall countries' populations.
#'                        If one or more variables are provided, the results will be split by all
#'                        but the last variable and the percentages of respondents will be computed
#'                        by the unique values of the last splitting variable.
#' @param bckg.corr.vars  Names of continuous background or contextual variables to compute the
#'                        correlation coefficients for. The results will be computed by all groups
#'                        specified by the splitting variables. See details.
#' @param PV.root.corr    The root names for the sets of plausible values to compute the correlation
#'                        coefficients for. See details.
#' @param corr.type       String of length one, specifying the type of the correlations to compute,
#'                        either \code{"Pearson"} (default) or \code{"Spearman"}.
#' @param weight.var      The name of the variable containing the weights. If no name of a weight
#'                        variable is provide, the function will automatically select the default
#'                        weight variable for the provided data, depending on the respondent type.
#' @param include.missing Logical, shall the missing values of the splitting variables be included
#'                        as categories to split by and all statistics produced for them? The
#'                        default (\code{FALSE}) takes all cases on the splitting variables without
#'                        missing values before computing any statistics. See details.
#' @param shortcut        Logical, shall the "shortcut" method for IEA TIMSS, TIMSS Advanced,
#'                        TIMSS Numeracy, eTIMSS PSI, PIRLS, ePIRLS, PIRLS Literacy and RLII be
#'                        applied? The default (\code{FALSE}) applies the "full" design when
#'                        computing the variance components and the standard errors of the
#'                        estimates.
#' @param save.output     Logical, shall the output be saved in MS Excel file (default) or not
#'                        (printed to the console or assigned to an object).
#' @param output.file     If \code{save.output = TRUE} (default), full path to the output file
#'                        including the file name. If omitted, a file with a default file name
#'                        "Analysis.xlsx" will be written to the working directory
#'                        (\code{getwd()}). Ignored if \code{save.output = FALSE}.
#' @param open.output     Logical, shall the output be open after it has been written? The default
#'                        (\code{TRUE}) opens the output in the default spreadsheet program
#'                        installed on the computer. Ignored if \code{save.output = FALSE}.
#'
#' @details
#' Either \code{data.file} or \code{data.object} shall be provided as source of data. If both of them are provided, the function will stop with an error message.
#'
#' The function computes correlation coefficients by the categories of the splitting variables. The percentages of respondents in each group are computed within the groups specified by the last splitting variable. If no splitting variables are added, the results will be computed only by country.
#'
#' Multiple continuous background variables and/or sets of plausible values can be provided to compute correlation coefficients for. Please note that in this case the results will slightly differ compared to using each pair of the same background continuous variables or PVs in separate analysis. This is because the cases with the missing values are removed in advance and the more variables are provided to compute correlations for, the more cases are likely to be removed. That is, the function support only listwisie deletion.
#'
#' Computation of correlation coefficients involving plausible values requires providing a root of the plausible values names in \code{PV.root.corr}. All studies (except CivED, TEDS-M, SITES, TALIS and TALIS Starting Strong Survey) have a set of PVs per construct (e.g. in TIMSS five for overall mathematics, five for algebra, five for geometry, etc.). In some studies (say TIMSS and PIRLS) the names of the PVs in a set always start with character string and end with sequential number of the PV. For example, the names of the set of PVs for overall mathematics in TIMSS are BSMMAT01, BSMMAT02, BSMMAT03, BSMMAT04 and BSMMAT05. The root of the PVs for this set to be added to \code{PV.root.corr} will be "BSMMAT". The function will automatically find all the variables in this set of PVs and include them in the analysis. In other studies like OECD PISA and IEA ICCS and ICILS the sequential number of each PV is included in the middle of the name. For example, in ICCS the names of the set of PVs are PV1CIV, PV2CIV, PV3CIV, PV4CIV and PV5CIV. The root PV name has to be specified in \code{PV.root.corr} as "PV#CIV". More than one set of PVs can be added. Note, however, that providing multiple continuous variables for the \code{bckg.avg.corr} argument and multiple PV roots for the \code{PV.root.corr} argument will affect the results for the correlation coefficients for the PVs because the cases with missing on \code{bckg.corr.vars} will be removed and this will also affect the results from the PVs (i.e. listwise deletion). On the other hand, using only sets of PVs to correlate should not affect the results on any PV estimates because PVs shall not have any missing values.
#'
#' A sufficient number of variable names (background/contextual) or PV roots have to be provided - either two background variables, or two PV roots, or mixture of them with total length of two (i.e. one background/contextual variable and one PV root).
#'
#' If \code{include.missing = FALSE} (default), all cases with missing values on the splitting variables will be removed and only cases with valid values will be retained in the statistics. Note that the data from the studies can be exported in two different ways: (1) setting all user-defined missing values to \code{NA}; and (2) importing all user-defined missing values as valid ones and adding their codes in an additional attribute to each variable. If the \code{include.missing} is set to \code{FALSE} (default) and the data used is exported using option (2), the output will remove all values from the variable matching the values in its \code{missings} attribute. Otherwise, it will include them as valid values and compute statistics for them.
#'
#' The \code{shortcut} argument is valid only for TIMSS, eTIMSS PSI, TIMSS Advanced, TIMSS Numeracy, PIRLS, ePIRLS, PIRLS Literacy and RLII. Previously, in computing the standard errors, these studies were using 75 replicates because one of the schools in the 75 JK zones had its weights doubled and the other one has been taken out. Since TIMSS 2015 and PIRLS 2016 the studies use 150 replicates and in each JK zone once a school has its weights doubled and once taken out, i.e. the computations are done twice for each zone. For more details see Foy & LaRoche (2016) and Foy & LaRoche (2017). If replication of the tables and figures is needed, the \code{shortcut} argument has to be changed to \code{TRUE}.
#' The function provides two-tailed \emph{t}-test and \emph{p}-values for the correlation coefficients.
#'
#' @return
#' If \code{save.output = FALSE}, a list containing the estimates and analysis information. If \code{save.output = TRUE} (default), an MS Excel (\code{.xlsx}) file (which can be opened in any spreadsheet program), as specified with the full path in the \code{output.file}. If the argument is missing, an Excel file with the generic file name "Analysis.xlsx" will be saved in the working directory (\code{getwd()}). The workbook contains three spreadsheets. The first one ("Estimates") contains a table with the results by country and the final part of the table contains averaged results from all countries' statistics. The results are presented as a correlation matrices by the splitting variables. The following columns can be found in the table, depending on the specification of the analysis:
#'
#' \itemize{
#'   \item \verb{<}Country ID\verb{>} - a column containing the names of the countries in the file for which statistics are computed. The exact column header will depend on the country identifier used in the particular study.
#'   \item \verb{<}Split variable 1\verb{>}, \verb{<}Split variable 2\verb{>}... - columns containing the categories by which the statistics were split by. The exact names will depend on the variables in \code{split.vars}.
#'   \item n_Cases - the number of cases in the sample used to compute the statistics.
#'   \item Sum_\verb{<}Weight variable\verb{>} - the estimated population number of elements per group after applying the weights. The actual name of the weight variable will depend on the weight variable used in the analysis.
#'   \item Sum_\verb{<}Weight variable\verb{>}\verb{_}SE - the standard error of the the estimated population number of elements per group. The actual name of the weight variable will depend on the weight variable used in the analysis.
#'   \item Percentages_\verb{<}Last split variable\verb{>} - the percentages of respondents (population estimates) per groups defined by the splitting variables in \code{split.vars}. The percentages will be for the last splitting variable which defines the final groups.
#'   \item Percentages_\verb{<}Last split variable\verb{>}\verb{_}SE - the standard errors of the percentages from above.
#'   \item Variable - the variable names (background/contextual or PV root names) to be matched against the rows of the following columns, forming the correlation matrices together.
#'   \item Correlation_\verb{<}Background variable\verb{>} - the correlation coefficient of each continuous \verb{<}Background variable\verb{>} specified in \code{bckg.corr.vars} against itself and each of the variables in the column "Variable". There will be one column with correlation coefficient estimate for each variable specified in \code{bckg.corr.vars} and/or set of PVs specified in \code{PV.root.corr}.
#'   \item Correlation_\verb{<}Background variable\verb{>}\verb{_}SE - the standard error of the correlation of each continuous \verb{<}Background variable\verb{>} specified in \code{bckg.corr.vars}. There will be one column with the SE of the correlation coefficient estimate for each variable specified in \code{bckg.corr.vars} and/or set of PVs specified in \code{PV.root.corr}.
#'   \item Correlation_\verb{<}root PV\verb{>} - the correlation coefficient of each set of PVs specified as PV root name in \code{PV.root.corr} against itself and each of the variables in the column "Variable". There will be one column with correlation coefficient estimate for each set of PVs specified in \code{PV.root.corr} and each other set of PVs specified in \code{PV.root.corr} and/or each continuous background variable specified in \code{bckg.corr.vars}.
#'   \item Correlation_\verb{<}root PV\verb{>}\verb{_}SE - the standard error of the correlation of each set of PVs specified as PV root name in \code{PV.root.corr}. There will be one column with the SE of the correlation coefficient estimate for each set of root PVs specified in \code{PV.root.corr} and another set of PVs specified in \code{PV.root.corr} and/or each continuous background variable specified in \code{bckg.corr.vars}.
#'   \item Correlation_\verb{<}root PV\verb{>}\verb{_}SVR - the sampling variance component for the correlation of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.corr}. There will be one column with the sampling variance component for the correlation coefficient estimate for each set of PVs specified in \code{PV.root.corr} with the other variables (other sets of PVs or background/contextual variables).
#'   \item Mean_\verb{<}root PV\verb{>}\verb{_}MVR - the measurement variance component for the correlation of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.corr}. There will be one column with the measurement variance component for the correlation coefficient estimate for each set of PVs specified in \code{PV.root.corr} with the other variables (other sets of PVs or background/contextual variables).
#'   \item Correlation_\verb{<}Background variable\verb{>}\verb{_}SVR - the sampling variance component for the correlation of the particular background variable with a set of PVs specified in \code{PV.root.corr} it is correlated with. There will be one column with the sampling variance component for the average estimate for each background/contextual variable correlated with a set of PVs specified in \code{PV.root.corr}.
#'   \item Correlation_\verb{<}Background variable\verb{>}\verb{_}MVR - the measurement variance component for the correlation of the particular background variable PVs with a set of PVs specified in \code{PV.root.corr}. There will be one column with the measurement variance component for the correlation coefficient estimate for each background/contextual variable correlated with a set of PVs specified in \code{PV.root.corr}.
#'   \item t_\verb{<}root PV\verb{>} - the \emph{t}-test value for the correlation coefficients of a set of PVs when correlating them with other variables (background/contextual or other sets of PVs).
#'   \item t_\verb{<}Background variable\verb{>} - the \emph{t}-test value for the correlation coefficients of background variables when correlating them with other variables (background/contextual or other sets of PVs).
#'   \item p_\verb{<}root PV\verb{>} - the \emph{p}-value for the correlation coefficients of a set of PVs when correlating them with other variables (background/contextual or other sets of PVs).
#'   \item p_\verb{<}Background variable\verb{>} - the \emph{p}-value value for the correlation coefficients of background variables when correlating them with other variables (background/contextual or other sets of PVs).
#' }
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
#' @examples
#' # Compute correlations between the complex student background scales
#' # "Home Educational Resources/SCL", "Students Sense of School Belonging/SCL" and
#' # "Students Value Mathematics/SCL" by sex of students in TIMSS 2015 grade 8
#' # using data file, omit missing from the splitting variable (female and male
#' # as answered by the students), without shortcut, and open the output after the
#' # computations are done
#' \dontrun{
#' lsa.corr(data.file = "C:/Data/TIMSS_2015_G8_Student_Miss_to_NA.RData", split.vars = "BSBG01",
#' bckg.corr.vars = c("BSBGHER", "BSBGSSB", "BSBGSVM"), include.missing = FALSE,
#' output.file = "C:/temp/test.xlsx", open.output = TRUE)
#' }
#'
#' # Compute correlations between the complex student background scales
#' # "Home Educational Resources/SCL" and "Students Sense of School Belonging/SCL"
#' # and the plausible values in overall mathematics and overall science by student
#' # sex and frequency of using computer or tablet at home using TIMSS 2015 grade 8
#' # data loaded in memory, using the shortcut, include the missing values in the
#' # splitting variables, and use the senate weights
#' \dontrun{
#' lsa.corr(data.object = T15_G8_student_data, split.vars = c("BSBG01", "BSBG13A"),
#' bckg.corr.vars = c("BSBGHER", "BSBGSSB"), PV.root.corr = c("BSMMAT", "BSSSCI"),
#' weight.var = "SENWGT", include.missing = FALSE, shortcut = TRUE,
#' output.file = "C:/temp/test.xlsx", open.output = TRUE)
#' }
#'
#' # Compute the correlations between student overall reading achievement, overall mathematics
#' # scores (i.e. using a set of PVs) and student family wealth, using PISA 2018 student data
#' # loaded as object in the memory, by country, and do not open the output after the computations
#' # are finished
#' \dontrun{
#' lsa.corr(data.object = CY07_MSU_STU_QQQ, bckg.corr.vars = "WEALTH",
#' PV.root.corr = c("PV#MATH", "PV#READ"), include.missing = TRUE,
#' output.file = "C:/temp/test.xlsx", open.output = FALSE)
#' }
#'
#' @references
#' LaRoche, S., Joncas, M., & Foy, P. (2016). Sample Design in TIMSS 2015. In M. O. Martin, I. V. S. Mullis, & M. Hooper (Eds.), \emph{Methods and Procedures in TIMSS 2015} (pp. 3.1-3.37). Chestnut Hill, MA: TIMSS & PIRLS International Study Center.
#' LaRoche, S., Joncas, M., & Foy, P. (2017). Sample Design in PIRLS 2016. In M. O. Martin, I. V. S. Mullis, & M. Hooper (Eds.), \emph{Methods and Procedures in PIRLS 2016} (pp. 3.1-3.34). Chestnut Hill, MA: Lynch School of Education, Boston College.
#'
#' @seealso \code{\link{lsa.convert.data}}
#' @export

lsa.corr <- function(data.file, data.object, split.vars, bckg.corr.vars, PV.root.corr, corr.type, weight.var, include.missing = FALSE, shortcut = FALSE, save.output = TRUE, output.file, open.output = TRUE) {
  tmp.options <- options(scipen = 999, digits = 22)
  on.exit(expr = options(tmp.options), add = TRUE)
  if(missing(corr.type)) {
    corr.type <- "Pearson"
  } else if(corr.type == "Spearman") {
    corr.type <- "Spearman"
  } else if(corr.type == "Pearson") {
    corr.type <- "Pearson"
  } else {
    stop('\n\nUnknown method for computting correlations specified in "corr.type" argument. All operations stop here. Check your input.\n\n', call. = FALSE)
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
  vars.list <- get.analysis.and.design.vars(data)
  if(!missing(split.vars)) {
    vars.list[["split.vars"]] <- vars.list[["split.vars"]][!split.vars == key(data)]
    if(length(vars.list[["split.vars"]]) == 0) {
      vars.list[["split.vars"]] <- NULL
    }
  }
  if(is.null(vars.list[["PV.root.corr"]]) & length(vars.list[["bckg.corr.vars"]]) < 2 || is.null(vars.list[["bckg.corr.vars"]]) & length(vars.list[["PV.root.corr"]]) < 2) {
    message("\n\n")
    stop('Insufficient number of variable names passed to "bckg.corr.vars" and/or number of sets of PVs passed "PV.root.corr". All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  action.args.list <- get.action.arguments()
  file.attributes <- get.file.attributes(imported.object = data)
  tryCatch({
    if(file.attributes[["lsa.study"]] %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "TiPi") & missing(shortcut)) {
      action.args.list[["shortcut"]] <- FALSE
    }
    data <- data[get(vars.list[["weight.var"]]) > 0, ]
    data <- produce.analysis.data.table(data.object = data, object.variables = vars.list, action.arguments = action.args.list, imported.file.attributes = file.attributes)
    if(length(key.vars) > 1) {
      data <- lapply(X = data, FUN = function(i) {
        tmp.colnames <- colnames(i)[!colnames(i) %in% c(key.vars, unlist(vars.list[c("split.vars", "bckg.corr.vars", "jk.zones", "rep.ind", "PV.names")]))]
        if(include.missing == FALSE) {
          i <- na.omit(object = i, cols = key.vars)
        }
        i <- na.omit(object = i, cols = vars.list[["bckg.corr.vars"]])
        tmp.data <- Filter(nrow, split(x = i, by = key.vars[2:length(key.vars)]))
        tmp.null.weights.idx <- Filter(isTRUE, lapply(X = tmp.data, FUN = function(j) {
          Filter(isTRUE, sapply(X = j[ , mget(tmp.colnames)], FUN = function(k) {
            all(k == 0)
          }))
        }))
        tmp.data[names(tmp.null.weights.idx)] <- lapply(X = tmp.data[names(tmp.null.weights.idx)], FUN = function(j) {
          j[ , (tmp.colnames) := lapply(.SD, function(k) {
            if(all(k == 0)) {
              k <- 0.00000000000001
            } else {
              k
            }
          }), .SDcols = tmp.colnames]
        })
        rbindlist(tmp.data)
      })
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
        if(!is.null(vars.list[["bckg.corr.vars"]])) {
          bckg.corr.vars.all.NA <- names(Filter(function(i) {all(is.na(i))}, data))
          if(length(bckg.corr.vars.all.NA) > 0) {
            data1 <- copy(data)
            data1[ , (bckg.corr.vars.all.NA) := NULL]
            data1 <- na.omit(object = data1)
            data1[ , (bckg.corr.vars.all.NA) := NA]
          } else {
            data1 <- na.omit(object = data)
          }
        } else {
          data1 <- na.omit(object = copy(data), cols = key.vars)
        }
        if(!is.null(vars.list[["pcts.var"]])) {
          percentages <- na.omit(data1[ , c(.(na.omit(unique(get(vars.list[["pcts.var"]])))), Map(f = wgt.pct, variable = .(get(vars.list[["pcts.var"]])), weight = mget(all.weights))), by = eval(vars.list[["group.vars"]])])
          number.of.cases <- na.omit(data1[eval(parse(text = vars.list[["weight.var"]])) > 0, .(n_Cases = .N), by = key.vars])
          sum.of.weights <- na.omit(data1[ , lapply(.SD, sum), by = key.vars, .SDcols = all.weights])
        } else {
          percentages <- na.omit(data1[ , c(.(na.omit(unique(get(key.vars)))), Map(f = wgt.pct, variable = .(get(key.vars)), weight = mget(all.weights)))])
          number.of.cases <- na.omit(data1[ , .(n_Cases = .N), by = key.vars])
          sum.of.weights <- na.omit(data1[ , lapply(.SD, sum), by = key.vars, .SDcols = all.weights])
        }
      } else if (include.missing == TRUE) {
        bckg.corr.vars.all.NA <- names(Filter(function(i) {all(is.na(i))}, data))
        if(length(bckg.corr.vars.all.NA) > 0) {
          data1 <- copy(data)
          data1[ , (bckg.corr.vars.all.NA) := NULL]
          data1 <- na.omit(object = data1, cols = vars.list[["bckg.corr.vars"]][!vars.list[["bckg.corr.vars"]] %in% bckg.corr.vars.all.NA])
          data1[ , (bckg.corr.vars.all.NA) := NA]
        } else {
          data1 <- na.omit(object = data, cols = unlist(vars.list["bckg.corr.vars"]))
        }
        if(!is.null(vars.list[["pcts.var"]])) {
          percentages <- data1[ , c(.(na.omit(unique(get(vars.list[["pcts.var"]])))), Map(f = wgt.pct, variable = .(get(vars.list[["pcts.var"]])), weight = mget(all.weights))), by = eval(vars.list[["group.vars"]])]
          number.of.cases <- data1[eval(parse(text = vars.list[["weight.var"]])) > 0, .(n_Cases = .N), by = key.vars]
          sum.of.weights <- data1[ , lapply(.SD, sum), by = key.vars, .SDcols = all.weights]
        } else {
          if(!is.null(vars.list[["bckg.corr.vars"]])) {
            percentages <- data1[ , c(.(na.omit(unique(get(key.vars)))), Map(f = wgt.pct, variable = .(get(key.vars)), weight = mget(all.weights)))]
            number.of.cases <- data1[ , .(n_Cases = .N), by = key.vars]
            sum.of.weights <- data1[ , lapply(.SD, sum), by = key.vars, .SDcols = all.weights]
          } else {
            percentages <- data[ , c(.(na.omit(unique(get(key.vars)))), Map(f = wgt.pct, variable = .(get(key.vars)), weight = mget(all.weights)))]
            number.of.cases <- data[ , .(n_Cases = .N), by = key.vars]
            sum.of.weights <- data[ , lapply(.SD, sum), by = key.vars, .SDcols = all.weights]
          }
        }
      }
      percentages <- list(percentages)
      sum.of.weights <- list(sum.of.weights)
      if(!is.null(vars.list[["pcts.var"]])) {
        reshape.list.statistics.bckg(estimate.object = percentages, estimate.name = "Percentages_", bckg.vars.vector = vars.list[["pcts.var"]], weighting.variable = vars.list[["weight.var"]], data.key.variables = key.vars, new.names.vector = vars.list[["pcts.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
      } else {
        reshape.list.statistics.bckg(estimate.object = percentages, estimate.name = "Percentages_", bckg.vars.vector = NULL, weighting.variable = vars.list[["weight.var"]], data.key.variables = key.vars, new.names.vector = key.vars, replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
      }
      percentages <- rbindlist(percentages)
      if(nrow(number.of.cases) > nrow(percentages)) {
        percentages <- merge(number.of.cases[ , mget(key.vars)], percentages, all.x = TRUE)
        percentages[ , (grep(pattern = "Percentages_[[:alnum:]]+$", x = colnames(percentages), value = TRUE)) := lapply(.SD, function(i){i[is.na(i)] <- 100; i}), .SDcols = grep(pattern = "Percentages_[[:alnum:]]+$", x = colnames(percentages), value = TRUE)]
        percentages[ , (grep(pattern = "Percentages_[[:alnum:]]+_SE$", x = colnames(percentages), value = TRUE)) := lapply(.SD, function(i){i[is.na(i)] <- 0; i}), .SDcols = grep(pattern = "Percentages_[[:alnum:]]+_SE$", x = colnames(percentages), value = TRUE)]
      }
      reshape.list.statistics.bckg(estimate.object = sum.of.weights, estimate.name = "Sum_", weighting.variable = vars.list[["weight.var"]], data.key.variables = key.vars, new.names.vector = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
      if(!is.null(vars.list[["PV.names"]])) {
        PV.names.to.split.by <- transpose(vars.list[["PV.names"]])
        PV.names.to.keep <- lapply(X = PV.names.to.split.by, FUN = function(i) {
          grep(pattern = paste(c(key.vars, i, vars.list[["bckg.corr.vars"]], all.weights, vars.list[["jk.zones"]], vars.list[["rep.ind"]]), collapse = "|"), x = colnames(data1), value = TRUE)
        })
        data1 <- lapply(X = PV.names.to.keep, FUN = function(i) {
          data1[ , mget(i)]
        })
      }
      if(!is.null(vars.list[["bckg.corr.vars"]]) & is.null(vars.list[["PV.root.corr"]])) {
        bckg.correlations <- list(compute.correlations.all.repwgt(data.object = data1, vars.vector = vars.list[["bckg.corr.vars"]], weight.var = all.weights, keys = key.vars, method = corr.type))
      } else if(is.null(vars.list[["bckg.corr.vars"]]) & !is.null(vars.list[["PV.root.corr"]])) {
        PV.correlations <- list(lapply(X = data1, FUN = function(i) {
          compute.correlations.all.repwgt(data.object = i, vars.vector = grep(pattern = paste(vars.list[["PV.root.corr"]], collapse = "|"), x = colnames(i), value = TRUE), weight.var = all.weights, keys = key.vars, method = corr.type)
        }))
      } else if(!is.null(vars.list[["bckg.corr.vars"]]) & !is.null(vars.list[["PV.root.corr"]])) {
        PV.correlations <- list(lapply(X = data1, FUN = function(i) {
          compute.correlations.all.repwgt(data.object = i, vars.vector = grep(pattern = paste(c(vars.list[["PV.root.corr"]], vars.list[["bckg.corr.vars"]]), collapse = "|"), x = colnames(i), value = TRUE), weight.var = all.weights, keys = key.vars, method = corr.type)
        }))
        PV.correlations <- lapply(X = PV.correlations, FUN = function(i) {
          lapply(X = i, FUN = function(j) {
            j[ , c("V1", "V2") := lapply(.SD, function(j) {
              factor(x = j, levels = unique(c(grep(pattern = paste(unlist(vars.list[["PV.names"]]), collapse = "|"), x = j, value = TRUE), vars.list[["bckg.corr.vars"]])))
            }), .SDcols = c("V1", "V2")]
          })
        })
        lapply(X = PV.correlations, FUN = function(i) {
          lapply(X = i, FUN = function(j) {
            setkeyv(x = j, cols = c(key.vars, "V1", "V2"))
          })
        })
      }
      if(!is.null(vars.list[["bckg.corr.vars"]]) & is.null(vars.list[["PV.root.corr"]])) {
        reshape.list.statistics.bckg(estimate.object = bckg.correlations, estimate.name = "Correlation_", data.key.variables = key.vars, new.names.vector = vars.list[["bckg.corr.vars"]], bckg.vars.vector = vars.list[["bckg.corr.vars"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
        bckg.correlations <- bckg.correlations[[1]]
        bckg.correlations <- bckg.correlations[ , c("V1", "V2") := lapply(.SD, function(i) {
          factor(x = i, levels = vars.list[["bckg.corr.vars"]])
        }), .SDcols = c("V1", "V2")]
        setkeyv(x = bckg.correlations, cols = c(key.vars, "V1", "V2"))
        bckg.correlations <- split(x = bckg.correlations, by = key.vars, drop = TRUE)
        bckg.correlations <- rbindlist(l = lapply(X = bckg.correlations, FUN = function(i) {
          cor.variables <- paste0("Correlation_", as.character(unique(i[ , V1])))
          cor.variables.SE <- paste0(cor.variables, "_SE")
          all.cor.variables <- c(cor.variables, cor.variables.SE)
          setnames(x = i, old = "V1", new = "Variable")
          tmp.corr <- matrix(i[ , get(grep(pattern = paste(cor.variables, collapse = "|"), x = colnames(i), value = TRUE)[1])], ncol = length(vars.list[["bckg.corr.vars"]]))
          tmp.corr.se <- matrix(i[ , get(grep(pattern = paste(cor.variables.SE, collapse = "|"), x = colnames(i), value = TRUE))], ncol = length(vars.list[["bckg.corr.vars"]]))
          i <- cbind(unique(i[ , mget(c(key.vars, "Variable"))]), data.table(tmp.corr, tmp.corr.se))
          setnames(x = i, old = grep(pattern = "^V[[:digit:]]+$", x = colnames(i)), new = all.cor.variables)
        }))
        if(length(bckg.corr.vars.all.NA) > 0) {
          bckg.correlations[ , (grep(pattern = "Correlation_", x = colnames(bckg.correlations), value = TRUE)) := NaN]
        }
      } else if(!is.null(vars.list[["PV.root.corr"]])) {
        reshape.list.statistics.PV(estimate.object = PV.correlations, estimate.name = "Correlation", PV.vars.vector = "", weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
        PV.correlations <- lapply(X = PV.correlations, FUN = function(i) {
          i <- lapply(X = seq_along(i), FUN = function(j) {
            i[[j]][ , (c("V1", "V2")) := lapply(.SD, function(k) {
              k <- as.character(k)
              if(file.attributes[["lsa.study"]] %in% c("PISA", "PISA for Development", "ICCS", "ICILS")) {
                k <- ifelse(test = k %in% unlist(vars.list[["PV.names"]]), yes = sub(pattern = "^PV[[:digit:]]+", replacement = "PVN", x = k), no = k)
                k <- factor(x = k, levels = c(gsub(pattern = "[[:digit:]]+", replacement = "N", x = vars.list[["PV.root.corr"]], fixed = TRUE), vars.list[["bckg.corr.vars"]]))
              } else {
                k <- ifelse(test = k %in% unlist(vars.list[["PV.names"]]), yes = sub(pattern = "[[:digit:]]+$", replacement = "", x = k), no = k)
                k <- factor(x = k, levels = c(vars.list[["PV.root.corr"]], vars.list[["bckg.corr.vars"]]))
              }
            }), .SDcols = c("V1", "V2")]
            setkeyv(x = i[[j]], c(key.vars, "V1", "V2"))
            if(file.attributes[["lsa.study"]] %in% c("PISA", "PISA for Development", "ICCS", "ICILS")) {
              setnames(x = i[[j]], old = "Correlation", new = paste0("Correlation_", gsub(pattern = "^PVN", replacement = paste0("PV", j), x = i[[j]][ , V1][1])))
              setnames(x = i[[j]], old = "Correlation_SumSq", new = paste0("Correlation_", gsub(pattern = "^PVN", replacement = paste0("PV", j), x = i[[j]][ , V1][1]),"_SumSq"))
            } else {
              setnames(x = i[[j]], old = "Correlation", new = paste0("Correlation_", i[[j]][ , V1][1], "0", j))
              setnames(x = i[[j]], old = "Correlation_SumSq", new = paste0("Correlation_", i[[j]][ , V1][1], "0", j, "_SumSq"))
            }
          })
        })
        PV.correlations <- lapply(X = PV.correlations, FUN = function(i) {
          Reduce(function(...) merge(...), i)
        })
        PV.names.cols <- lapply(X = PV.correlations, FUN = function(i) {
          i[ , .(V1, V2)]
        })
        PV.correlations <- lapply(X = PV.correlations, FUN = function(i) {
          i[ , (c("V1", "V2")) := NULL]
        })
        aggregate.PV.estimates(estimate.object = PV.correlations, estimate.name = "Correlation_", root.PV = vars.list[["PV.root.corr"]], PV.vars.vector = vars.list[["PV.names"]], data.key.variables = key.vars, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
        PV.correlations <- Map(f = function(input1, input2) {
          cbind(input1[ , mget(key.vars)], input2, input1[ , mget(grep(pattern = paste(key.vars, collapse = "|"), x = colnames(input1), value = TRUE, invert = TRUE))])
        }, input1 = PV.correlations, input2 = PV.names.cols)
        PV.correlations <- lapply(X = PV.correlations, FUN = function(i) {
          corr.colls <- grep(pattern = "Correlation_", x = colnames(i), value = TRUE)
          setnames(x = i, old = corr.colls, new = gsub(pattern = paste(paste0("_", i[ , V1]), collapse = "|"), replacement = "", x = corr.colls))
        })
        PV.correlations <- PV.correlations[[1]]
        PV.correlations[ , (grep(pattern = "^Correlation", x = colnames(PV.correlations), value = TRUE)) := lapply(.SD, function(i) {
          ifelse(test = is.na(i), yes = NaN, no = i)
        }), .SDcols = grep(pattern = "^Correlation", x = colnames(PV.correlations), value = TRUE)]
        PV.correlations <- PV.correlations[ , c("V1", "V2") := lapply(.SD, function(i) {
          if(file.attributes[["lsa.study"]] %in% c("PISA", "PISA for Development", "ICCS", "ICILS")) {
            factor(x = i, levels = c(gsub(pattern = "PV[[:digit:]]+", replacement = "PVN", x = vars.list[["PV.root.corr"]], fixed = TRUE), vars.list[["bckg.corr.vars"]]))
          } else {
            factor(x = i, levels = c(vars.list[["PV.root.corr"]], vars.list[["bckg.corr.vars"]]))
          }
        }), .SDcols = c("V1", "V2")]
        setkeyv(x = PV.correlations, cols = c(key.vars, "V1", "V2"))
        PV.correlations <- split(x = PV.correlations, by = key.vars, drop = TRUE)
        PV.correlations <- rbindlist(l = lapply(X = PV.correlations, FUN = function(i) {
          cor.variables <- paste0("Correlation_", as.character(unique(i[ , V1])))
          cor.variables.SE <- paste0(cor.variables, "_SE")
          cor.variables.SVR <- paste0(cor.variables, "_SVR")
          cor.variables.MVR <- paste0(cor.variables, "_MVR")
          all.cor.variables <- c(cor.variables, cor.variables.SE, cor.variables.SVR, cor.variables.MVR)
          setnames(x = i, old = "V1", new = "Variable")
          number.of.columns <- sum(length(vars.list[["PV.root.corr"]]), length(vars.list[["bckg.corr.vars"]]))
          tmp.corr <- matrix(i[ , get("Correlation")], ncol = number.of.columns)
          tmp.corr.se <- matrix(i[ , get("Correlation_SE")], ncol = number.of.columns)
          tmp.corr.SVR <- matrix(i[ , get("Correlation_SVR")], ncol = number.of.columns)
          tmp.corr.MVR <- matrix(i[ , get("Correlation_MVR")], ncol = number.of.columns)
          i <- cbind(unique(i[ , mget(c(key.vars, "Variable"))]), data.table(tmp.corr, tmp.corr.se, tmp.corr.SVR, tmp.corr.MVR))
          setnames(x = i, old = grep(pattern = "^V[[:digit:]]+$", x = colnames(i)), new = all.cor.variables)
        }))
        if(!is.null(vars.list[["bckg.corr.vars"]]) && length(bckg.corr.vars.all.NA) > 0) {
          PV.correlations[ , (grep(pattern = "Correlation_", x = colnames(PV.correlations), value = TRUE)) := NaN]
        }
        merged.PV.estimates <- PV.correlations
        PV.correlations <- NULL
      }
      country.analysis.info <- produce.analysis.info(cnt.ID = unique(data[ , get(key.vars[1])]), data = used.data, study = file.attributes[["lsa.study"]], cycle = file.attributes[["lsa.cycle"]], weight.variable = vars.list[["weight.var"]], rep.design = DESIGN, used.shortcut = shortcut, number.of.reps = rep.wgts.names, in.time = cnt.start.time)
      analysis.info[[country.analysis.info[ , COUNTRY]]] <<- country.analysis.info
      if(!is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.corr.vars"]]) && is.null(vars.list[["PV.root.corr"]])) {
        merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.correlations))
      } else if(!is.null(vars.list[["split.vars"]]) && is.null(vars.list[["bckg.corr.vars"]]) && !is.null(vars.list[["PV.root.corr"]])){
        merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, merged.PV.estimates))
      } else if(!is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.corr.vars"]]) && !is.null(vars.list[["PV.root.corr"]])) {
        merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, merged.PV.estimates))
      } else if(is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.corr.vars"]]) && is.null(vars.list[["PV.root.corr"]])) {
        merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.correlations))
      } else if(is.null(vars.list[["split.vars"]]) && is.null(vars.list[["bckg.corr.vars"]]) && !is.null(vars.list[["PV.root.corr"]])) {
        merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, merged.PV.estimates))
      } else if(is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.corr.vars"]]) && !is.null(vars.list[["PV.root.corr"]])) {
        merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, merged.PV.estimates))
      }
      cor.columns <- grep(pattern = "^Correlation_", x = colnames(merged.outputs), value = TRUE)
      cor.SE.columns <- grep(pattern = "_SE$", x = cor.columns, value = TRUE)
      cor.columns <- grep(pattern = "_SE$|_SVR$|_MVR$", x = cor.columns, value = TRUE, invert = TRUE)
      if(file.attributes[["lsa.study"]] %in% c("PISA", "PISA for Development", "ICCS", "ICILS")) {
        t.value.columns <- paste0("t_", c(gsub(pattern = "[[:digit:]]+", replacement = "N", x = vars.list[["PV.root.corr"]], fixed = TRUE), vars.list[["bckg.corr.vars"]]))
      } else {
        t.value.columns <- paste0("t_", c(vars.list[["PV.root.corr"]], vars.list[["bckg.corr.vars"]]))
      }
      merged.outputs[ , (t.value.columns) := Map(f = `/`, mget(cor.columns), mget(cor.SE.columns))]
      merged.outputs[ , (t.value.columns) := lapply(.SD, function(i) {
        ifelse(test = is.infinite(i), yes = NA, no = i)
      }), .SDcols = t.value.columns]
      if(file.attributes[["lsa.study"]] %in% c("PISA", "PISA for Development", "ICCS", "ICILS")) {
        p.value.columns <- paste0("p_", c(gsub(pattern = "[[:digit:]]+", replacement = "N", x = vars.list[["PV.root.corr"]], fixed = TRUE), vars.list[["bckg.corr.vars"]]))
      } else {
        p.value.columns <- paste0("p_", c(vars.list[["PV.root.corr"]], vars.list[["bckg.corr.vars"]]))
      }
      merged.outputs[ , degrees.of.freedom := get(paste0("Sum_", vars.list[["weight.var"]])) - 2L]
      merged.outputs[ , (p.value.columns) := lapply(.SD, function(i) {
        2 * pt(q = -abs(i), df = degrees.of.freedom)
      }), .SDcols = t.value.columns]
      merged.outputs[ , degrees.of.freedom := NULL]
      merged.outputs[ , (c(t.value.columns, p.value.columns)) := lapply(.SD, function(i) {
        ifelse(test = is.na(i), yes = NaN, no = i)
      }), .SDcols = c(t.value.columns, p.value.columns)]
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
      message("\nAll ", length(unique(estimates[ , get(key.vars[1])])), " countries with valid data processed in ", format(as.POSIXct("0001-01-01 00:00:00") + total.exec.time - 1, "%H:%M:%OS3"))
    } else {
      message("\n")
    }
    ptm.add.table.average <- proc.time()
    if(!is.null(vars.list[["bckg.corr.vars"]]) && anyNA(estimates[ , Variable])) {
      estimates[ , Variable := `levels<-` (addNA(Variable), c(levels(Variable), "<NA>"))]
      estimates[ , n_Cases := as.numeric(n_Cases)]
      estimates[Variable == "<NA>" , (grep(pattern = paste(c("n_Cases", "Sum_", "Percentages_", "Correlation_"), collapse = "|"), x = colnames(estimates), value = TRUE)) := NaN]
    }
    estimates <- compute.table.average(output.obj = estimates, object.variables = vars.list, data.key.variables = c(key.vars, "Variable"), data.properties = file.attributes)
    message('"Table Average" added to the estimates in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.add.table.average}[[3]], "%H:%M:%OS3"), "\n")
    if(isTRUE(save.output)) {
      export.results(output.object = estimates, analysis.type = action.args.list[["executed.analysis.function"]], analysis.info.obj = rbindlist(l = analysis.info), destination.file = output.file, open.exported.file = open.output)
    } else if(isFALSE(save.output)) {
      return(list(Estimates = estimates, `Analysis information` = rbindlist(l = analysis.info)))
    }
    if(exists("removed.countries.where.any.split.var.is.all.NA") && length(removed.countries.where.any.split.var.is.all.NA) > 0) {
      warning('Some of the countries had one or more splitting variables which contains only missing values. These countries are: "', paste(removed.countries.where.any.split.var.is.all.NA, collapse = '", "'), '".', call. = FALSE)
    }
  }, interrupt = function(f) {
    message("\nInterrupted by the user. Computations are not finished and output file is not produced.\n")
  })
  vars.list.analysis.vars <- grep(pattern = "split.vars|bckg.corr.vars", x = names(vars.list), value = TRUE)
  vars.list.analysis.vars <- unlist(vars.list[vars.list.analysis.vars])
  vars.list.analysis.vars <- grep(pattern = paste(unique(unlist(studies.all.design.variables)), collapse = "|"), x = vars.list.analysis.vars, value = TRUE)
  if(length(vars.list.analysis.vars) > 0) {
    warning('Some of the variables specified as analysis variables (in "split.vars" and/or "bckg.corr.vars") are design variables (sampling variables or PVs). This kind of variables shall not be used for analysis. Check your input.', call. = FALSE)
  }
}
