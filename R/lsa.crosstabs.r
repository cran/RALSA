#' @title Compute crosstabulations and design corrected chi-square statistics
#'
#' @description \code{lsa.crosstabs} computes two-way tables and estimates the Rao-Scott first- and second-order adjusted chi-square.
#'
#' @param data.file       The file containing \code{lsa.data} object. Either this or
#'                        \code{data.object}
#'                        shall be specified, but not both. See details.
#' @param data.object     The object in the memory containing \code{lsa.data} object. Either this
#'                        or \code{data.file} shall be specified, but not both. See details.
#' @param split.vars      Categorical variable(s) to split the results by. If no split variables
#'                        are provided, the results will be for the overall countries'
#'                        populations. If one or more variables are provided, the results will be
#'                        split by all but the last variable and the percentages of respondents
#'                        will be computed by the unique values of the last splitting variable.
#' @param bckg.row.var    Name of the categorical background row variable. The results will be
#'                        computed by all groups specified by the splitting variables. See details.
#' @param bckg.col.var    Name of the categorical background column variable. The results will be
#'                        computed by all groups specified by the splitting variables. See details.
#' @param expected.cnts   Logical, shall the expected counts be computed as well? The default
#'                        (\code{TRUE}) will compute the expected counts. If \code{FALSE}, only
#'                        the observed counts will be included in the output.
#' @param row.pcts        Logical, shall row percentages be computed? The default (\code{FALSE})
#'                        will skip the computation of the row percentages.
#' @param column.pcts     Logical, shall column percentages be computed? The default (\code{FALSE})
#'                        will skip the computation of the column percentages.
#' @param total.pcts      Logical, shall percentages of total be computed? The default
#'                        (\code{FALSE}) will skip the computation of the total percentages.
#' @param weight.var      The name of the variable containing the weights. If no name of a weight
#'                        variable is provided, the function will automatically select the default
#'                        weight variable for the provided data, depending on the respondent type.
#' @param include.missing Logical, shall the missing values of the splitting variables be included
#'                        as categories to split by and all statistics produced for them? The
#'                        default (\code{FALSE}) takes all cases on the splitting variables
#'                        without missing values before computing any statistics. See details.
#' @param shortcut        Logical, shall the "shortcut" method for IEA TIMSS, TIMSS Advanced,
#'                        TIMSS Numeracy, eTIMSS, PIRLS, ePIRLS, PIRLS Literacy and RLII be
#'                        applied? The default (\code{FALSE}) applies the "full" design when
#'                        computing the variance components and the standard errors of the
#'                        estimates.
#' @param output.file     Full path to the output file including the file name. If omitted, a file
#'                        with a default file name "Analysis.xlsx" will be written to the working
#'                        directory (\code{getwd()}).
#' @param open.output     Logical, shall the output be open after it has been written? The default
#'                        (\code{TRUE}) opens the output in the default spreadsheet program
#'                        installed on the computer.
#'
#' @details
#' The function computes two-way tables between two categorical variables and estimates the Rao-Scott first- and second-order design correction of the chi-square statistics. All statistics are computed within the groups specified by the last splitting variable. If no splitting variables are added, the results will be computed only by country.
#'
#' Either \code{data.file} or \code{data.object} shall be provided as source of data. If both of them are provided, the function will stop with an error message.
#'
#' Only two (row and column) categorical variables can be provided. The function always computes the observed counts. If requested, the expected counts, row percentages, column percentages and total percentages can be computed as well.
#'
#' If \code{include.missing = FALSE} (default), all cases with missing values on the splitting variables will be removed and only cases with valid values will be retained in the statistics. Note that the data from the studies can be exported in two different ways: (1) setting all user-defined missing values to \code{NA}; and (2) importing all user-defined missing values as valid ones and adding their codes in an additional attribute to each variable. If the \code{include.missing} is set to \code{FALSE} (default) and the data used is exported using option (2), the output will remove all values from the variable matching the values in its \code{missings} attribute. Otherwise, it will include them as valid values and compute statistics for them.
#'
#' The \code{shortcut} argument is valid only for TIMSS, eTIMSS, TIMSS Advanced, TIMSS Numeracy, PIRLS, ePIRLS, PIRLS Literacy and RLII. Previously, in computing the standard errors, these studies were using 75 replicates because one of the schools in the 75 JK zones had its weights doubled and the other one has been taken out. Since TIMSS 2015 and PIRLS 2016 the studies use 150 replicates and in each JK zone once a school has its weights doubled and once taken out, i.e. the computations are done twice for each zone. For more details see Foy & LaRoche (2016) and Foy & LaRoche (2017).
#'
#' The function also computes chi-square statistics with Rao-Scott first- and second-order design corrections because of the clustering in complex survey designs. For more details, see Rao & Scott (1984, 1987) and Skinner (2019).
#'
#' @return
#' A MS Excel (\code{.xlsx}) file (which can be opened in any spreadsheet program), as specified with the full path in the \code{output.file}. If the argument is missing, an Excel file with the generic file name "Analysis.xlsx" will be saved in the working directory (\code{getwd()}). The workbook contains four spreadsheets. The first one ("Estimates") contains a table with the results by country and the final part of the table contains averaged results from all countries' statistics. The following columns can be found in the table, depending on the specification of the analysis:
#'
#' \itemize{
#'   \item \verb{<}Country ID\verb{>} - a column containing the names of the countries in the file for which statistics are computed. The exact column header will depend on the country identifier used in the particular study.
#'   \item \verb{<}Split variable 1\verb{>}, \verb{<}Split variable 2\verb{>}... - columns containing the categories by which the statistics were split by. The exact names will depend on the variables in \code{split.vars}.
#'   \item n_Cases - the number of cases in the sample used to compute the statistics for each split combination defined by the \code{split.vars}, if any, and the \code{bckg.row.var}.
#'   \item Sum_\verb{<}Weight variable\verb{>} - the estimated population number of elements per group after applying the weights. The actual name of the weight variable will depend on the weight variable used in the analysis.
#'   \item Sum_\verb{<}Weight variable\verb{>}\verb{_}SE - the standard error of the the estimated population number of elements per group. The actual name of the weight variable will depend on the weight variable used in the analysis.
#'   \item Percentages_\verb{<}Row variable\verb{>} - the percentages of respondents (population estimates) per groups defined by the splitting variables in \code{split.vars}, if any, and the row variable in \code{bckg.row.var}. The percentages will be for the combination of categories in the last splitting variable and the row variable which define the final groups.
#'   \item Percentages_\verb{<}Row variable\verb{>}\verb{_}SE - the standard errors of the percentages from above.
#'   \item Type - the type of computed values depending on the logical values passed to the \code{expected.cnts}, \code{row.pcts}, \code{column.pcts}, and \code{total.pcts} arguments: "Observed count", "Expected count", "Row percent", "Column percent", and "Percent of total".
#'   \item \verb{<}Column variable name Category 1\verb{>}, \verb{<}Column variable name Category 1\verb{>},... - the estimated values for all combinations between the row and column variables passed to \code{bckg.row.var} and \code{bckg.col.var}. There will be one column for each category of the column variable.
#'   \item \verb{<}Column variable name Category 1, 2,... n\verb{>}\verb{_}SE - the standard errors of the estimated values from the above.
#'   \item Total - the grand totals for each of the estimated value types ("Observed count", "Expected count", "Row percent", "Column percent", and "Percent of total") depending on the logical values (\code{TRUE}, \code{FALSE}) passed to the \code{expected.cnts}, \code{row.pcts}, \code{column.pcts}, and \code{total.pcts} arguments.
#'   \item Total\verb{_}SE - the standard errors of the estimated values from the above.
#'   }
#' The second sheet contains some additional information related to the analysis per country in the following columns:
#' \itemize{
#'   \item \verb{<}Country ID\verb{>} - a column containing the names of the countries in the file for which statistics are computed. The exact column header will depend on the country identifier used in the particular study.
#'   \item \verb{<}Split variable 1\verb{>}, \verb{<}Split variable 2\verb{>}... - columns containing the categories by which the statistics were split by. The exact names will depend on the variables in \code{split.vars}.
#'   \item Statistics - contains the names of the different statistics types: chi-squares, degrees of freedom (sample and design), and p-values.
#'   \item Value - the estimated values for the statistics from above.
#' }
#' The third sheet contains some additional information related to the analysis per country in the following columns:
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
#' The fourth sheet contains the call to the function with values for all parameters as it was executed. This is useful if the analysis needs to be replicated later.
#'
#' @examples
#' # Compute two-way table between student sex and how much they proud they are proud to go to
#' # school using PIRLS 2016 student data.
#' \dontrun{
#' lsa.crosstabs(data.file = "C:/Data/PIRLS_2016_G8_Student_Miss_to_NA.RData",
#' bckg.row.var = "ITSEX", bckg.col.var = "ASBG12E")
#' }
#'
#' # Same as the above, this time also computing the expected counts, row percentages, column
#' # percentages, percentages of total.
#' \dontrun{
#' lsa.crosstabs(data.file = "C:/Data/PIRLS_2016_G8_Student_Miss_to_NA.RData",
#' bckg.row.var = "ITSEX", bckg.col.var = "ASBG12E", expected.cnts = TRUE,
#' row.pcts = TRUE, column.pcts = TRUE, total.pcts = TRUE)
#' }
#'
#' @references
#' LaRoche, S., Joncas, M., & Foy, P. (2016). Sample Design in TIMSS 2015. In M. O. Martin, I. V. S. Mullis, & M. Hooper (Eds.), \emph{Methods and Procedures in TIMSS 2015} (pp. 3.1-3.37). Chestnut Hill, MA: TIMSS & PIRLS International Study Center.
#' LaRoche, S., Joncas, M., & Foy, P. (2017). Sample Design in PIRLS 2016. In M. O. Martin, I. V. S. Mullis, & M. Hooper (Eds.), \emph{Methods and Procedures in PIRLS 2016} (pp. 3.1-3.34). Chestnut Hill, MA: Lynch School of Education, Boston College.
#' Rao, J. N. K., & Scott, A. J. (1984). On Chi-Squared Tests for Multiway Contingency Tables with Cell Proportions Estimated from Survey Data. \emph{The Annals of Statistics}, \emph{12}(1). https://doi.org/10.1214/aos/1176346391
#' Rao, J. N. K., & Scott, A. J. (1987). On Simple Adjustments to Chi-Square Tests with Sample Survey Data. \emph{The Annals of Statistics}, \emph{15}(1), 385-397.
#' Skinner, C. (2019). Analysis of Categorical Data for Complex Surveys. \emph{International Statistical Review}, \emph{87}(S1), S64-S78. https://doi.org/10.1111/insr.12285
#'
#' @seealso \code{\link{lsa.convert.data}}
#' @export

lsa.crosstabs <- function(data.file, data.object, split.vars, bckg.row.var, bckg.col.var, expected.cnts = TRUE, row.pcts = FALSE, column.pcts = FALSE, total.pcts = FALSE, weight.var, include.missing = FALSE, shortcut = FALSE, output.file, open.output = TRUE) {
  tmp.options <- options(scipen = 999, digits = 22)
  on.exit(expr = options(tmp.options), add = TRUE)
  warnings.collector <- list()
  warnings.collector["insufficient.cases"] <- list(NULL)
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
  action.args.list <- get.action.arguments()
  file.attributes <- get.file.attributes(imported.object = data)
  tryCatch({
    if(file.attributes[["lsa.study"]] %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "TiPi") & missing(shortcut)) {
      action.args.list[["shortcut"]] <- FALSE
    }
    bckg.vars.levels <- list()
    if(is.factor(data[ , get(vars.list[["bckg.row.var"]])])) {
      bckg.vars.levels[["row.levels"]] <- levels(data[ , get(vars.list[["bckg.row.var"]])])
    }
    if(is.factor(data[ , get(vars.list[["bckg.col.var"]])])) {
      bckg.vars.levels[["col.levels"]] <- levels(data[ , get(vars.list[["bckg.col.var"]])])
    }
    data <- produce.analysis.data.table(data.object = data, object.variables = vars.list, action.arguments = action.args.list, imported.file.attributes = file.attributes)
    bckg.crosstabs.vars.all.NA <- names(Filter(length,
                                               lapply(X = data, FUN = function(i) {
                                                 names(Filter(function(j) {all(is.na(j))}, i[ , mget(unlist(vars.list[c("bckg.row.var", "bckg.col.var")]))]))
                                               })
    ))
    if(length(bckg.crosstabs.vars.all.NA)) {
      data <- data[!names(data) %in% bckg.crosstabs.vars.all.NA]
      data <- lapply(X = data, FUN = function(i) {
        na.omit(object = i, cols = unlist(vars.list[c("bckg.row.var", "bckg.col.var")]))
      })
      warnings.collector[["removed.countries.row.col.vars"]] <- paste0('One or more countries in the data have no valid values for the "bckg.row.vars" and/or "bckg.col.vars" and have been removed: ', paste(bckg.crosstabs.vars.all.NA, collapse = ", "), ".")
    } else {
      data <- lapply(X = data, FUN = function(i) {
        na.omit(object = i, cols = unlist(vars.list[c("bckg.row.var", "bckg.col.var")]))
      })
    }
    if(length(data) == 0) {
      stop('\nAll countries in the data have only missing values on "bckg.row.var" and/or "bckg.col.var". All operations stop here.\n\n', call. = FALSE)
    }
    missing.JKZONES <- lapply(X = data, FUN = function(i) {
      jk.zone.col <- intersect(unique(unname(unlist(design.weight.variables[c("IEA.JK2.dflt.std.bckg.zones", "IEA.JK2.dflt.sch.bckg.zones", "IEA.JK2.dflt.tch.bckg.zones")]))), colnames(i))
      if(length(jk.zone.col) > 0) {
        all(is.na(i[ , get(jk.zone.col)]))
      } else {
        ""
      }
    })
    if(length(names(Filter(isTRUE, missing.JKZONES))) > 0) {
      data[names(Filter(isTRUE, missing.JKZONES))] <- NULL
      warnings.collector[["data.no.JKZONE.JKREP"]] <- paste0('One or more countries in the data have no valid values for the JK zone and replication indicator variables and have been removed: ', paste(names(Filter(isTRUE, missing.JKZONES)), collapse = ", "), ".")
    }
    if(length(names(data)) == 0) {
      stop('\nAll countries with valid valid values on "bckg.row.var" and/or "bckg.col.var" in the data have only missing values on the jackknifing zones. All operations stop here.\n\n', call. = FALSE)
    }
    vars.list[["pcts.var"]] <- tmp.pcts.var
    vars.list[["group.vars"]] <- tmp.group.vars
    analysis.info <- list()
    Rao.Scott.adj.chi.sq <- list()
    Rao.Scott.adj.chi.sq <- list()
    number.of.countries <- length(names(data))
    if(number.of.countries == 1) {
      message("\nValid data from one country have been found. Some computations can be rather intensive. Please be patient.\n")
    } else if(number.of.countries > 1) {
      message("\nValid data from ", number.of.countries, " countries have been found. Some computations can be rather intensive. Please be patient.\n")
    }
    counter <- 0
    compute.all.stats <- function(data) {
      if(!is.null(bckg.vars.levels[["row.levels"]])) {
        data[ , vars.list[["bckg.row.var"]] := factor(get(vars.list[["bckg.row.var"]]))]
        data[ , setattr(x = get(vars.list[["bckg.row.var"]]), name = "levels", value = bckg.vars.levels[["row.levels"]])]
      }
      if(!is.null(bckg.vars.levels[["col.levels"]])) {
        data[ , vars.list[["bckg.col.var"]] := factor(get(vars.list[["bckg.col.var"]]))]
        data[ , setattr(x = get(vars.list[["bckg.col.var"]]), name = "levels", value = bckg.vars.levels[["col.levels"]])]
      }
      rep.wgts.names <- paste(c("REPWGT", unlist(lapply(X = design.weight.variables[grep("rep.wgts", names(design.weight.variables), value = TRUE)], FUN = function(i) {
        unique(gsub(pattern = "[[:digit:]]*$", replacement = "", x = i))
      }))), collapse = "|")
      rep.wgts.names <- grep(pattern = rep.wgts.names, x = names(data), value = TRUE)
      all.weights <- c(vars.list[["weight.var"]], rep.wgts.names)
      cnt.start.time <- format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS3")
      if(!is.null(vars.list[["pcts.var"]])) {
        percentages.bckg.row.var <- na.omit(data[ , Map(f = wgt.pct, variable = .(get(key.vars[1])), weight = mget(all.weights)), keyby = c(eval(vars.list[["split.vars"]]), vars.list[["bckg.row.var"]])])
        percentages.bckg.row.var <- percentages.bckg.row.var[ , mget(vars.list[["bckg.row.var"]])]
        percentages <- na.omit(data[ , Map(f = wgt.pct, variable = .(get(vars.list[["bckg.row.var"]])), weight = mget(all.weights)), keyby = eval(key.vars)])
        percentages <- cbind(percentages[ , mget(key.vars)], percentages.bckg.row.var, percentages[ , mget(grep(pattern = "^V[[:digit:]]+$", x = colnames(percentages), value = TRUE))])
        setnames(x = percentages, old = c(vars.list[["bckg.row.var"]], grep(pattern = "^V[[:digit:]]", x = colnames(percentages), value = TRUE)), new = paste0("V", 1:length(c(vars.list[["bckg.row.var"]], grep(pattern = "^V[[:digit:]]", x = colnames(percentages), value = TRUE)))))
        if(length(vars.list[["group.vars"]]) == 1) {
          percentages.total <- na.omit(data[ , c(Map(f = wgt.pct, variable = .(get(vars.list[["group.vars"]])), weight = mget(all.weights))), keyby = eval(vars.list[["split.vars"]])])
        } else {
          percentages.total <- na.omit(data[ , c(Map(f = wgt.pct, variable = .(get(vars.list[["group.vars"]][1])), weight = mget(all.weights))), keyby = c(eval(vars.list[["group.vars"]][2:(length(vars.list[["group.vars"]]))]), eval(vars.list[["pcts.var"]]))])
        }
        percentages.total <- cbind(unique(percentages[ , vars.list[["group.vars"]][[1]], with = FALSE]), data.table("Total"), percentages.total)
        setnames(x = percentages.total, old = grep(pattern = "^V[[:digit:]]+$", x = colnames(percentages.total)), new = paste0("V", 1:length(grep(pattern = "^V[[:digit:]]+$", x = colnames(percentages.total)))))
        setcolorder(x = percentages.total, neworder = c(key.vars, grep(pattern = "^V[[:digit:]]+$", x = colnames(percentages.total), value = TRUE)))
        percentages.total[ , V1 := factor(V1)]
        percentages <- rbindlist(l = list(percentages, percentages.total), use.names = TRUE, fill = TRUE)
        percentages.total <- NULL
        if(length(key.vars) > 2) {
          keys.to.compute.on <- c(key.vars[2:length(key.vars)])
          keys.to.compute.on <- rep(x = list(keys.to.compute.on), times = length(keys.to.compute.on))
          keys.to.compute.on <- lapply(X = 1:length(keys.to.compute.on), FUN = function(i) {
            keys.to.compute.on[[i]][0:(length(keys.to.compute.on[[i]]) - i)]
          })
          keys.to.compute.on <- Filter(length, keys.to.compute.on)
          percentages.total <- lapply(X = keys.to.compute.on, FUN = function(i) {
            na.omit(data[ , c(Map(f = wgt.pct, variable = .(get(vars.list[["group.vars"]][1])), weight = mget(all.weights))), keyby = i])
          })
          percentages.total <- rbindlist(l = percentages.total, use.names = TRUE, fill = TRUE)
          percentages.total[ , (key.vars[2:(length(key.vars) - 1)]) := lapply(.SD, function(i) {
            i <- factor(ifelse(test = is.na(i), yes = "Total", no = paste(i)), levels = c(levels(i), "Total"))
            droplevels(i)
          }), .SDcols = (key.vars[2:(length(key.vars) - 1)])]
          percentages.lead.cols <- unique(percentages[ , mget(c(vars.list[["group.vars"]][1], "V1"))])
          percentages.lead.cols <- percentages.lead.cols[V1 != "Total", ]
          percentages.total <- cbind(percentages.lead.cols, unique(percentages[ , mget(vars.list[["pcts.var"]])]), percentages.total)
          setnames(x = percentages.total, old = grep(pattern = "^V[[:digit:]]+$", x = colnames(percentages.total)), new = paste0("V", 1:length(grep(pattern = "^V[[:digit:]]+$", x = colnames(percentages.total)))))
          setcolorder(x = percentages.total, neworder = c(key.vars, grep(pattern = "^V[[:digit:]]+$", x = colnames(percentages.total), value = TRUE)))
          percentages.total[ , (c(vars.list[["pcts.var"]], "V1")) := factor("Total")]
          percentages <- rbindlist(l = list(percentages, percentages.total), use.names = TRUE, fill = TRUE)
          tmp.lead.cols <- NULL
          percentages.total <- NULL
        }
        percentages.total <- na.omit(data[ , c(Map(f = wgt.pct, variable = .(get(vars.list[["group.vars"]][1])), weight = mget(all.weights)))])
        tmp.lead.cols <- percentages[ , mget(key.vars)]
        tmp.lead.cols[ , (key.vars[2:length(key.vars)]) := lapply(.SD, function(i) {
          i <- factor("Total")
        }), .SDcols = key.vars[2:length(key.vars)]]
        tmp.lead.cols <- unique(tmp.lead.cols)
        percentages.total <- cbind(tmp.lead.cols, factor("Total"), percentages.total)
        setnames(x = percentages.total, old = grep(pattern = "^V[[:digit:]]+$", x = colnames(percentages.total)), new = paste0("V", 1:length(grep(pattern = "^V[[:digit:]]+$", x = colnames(percentages.total)))))
        percentages <- rbindlist(l = list(percentages, percentages.total), use.names = TRUE, fill = TRUE)
        tmp.lead.cols <- NULL
        percentages.total <- NULL
        number.of.cases <- na.omit(data[eval(parse(text = vars.list[["weight.var"]])) > 0, .(n_Cases = .N), keyby = c(key.vars, vars.list[["bckg.row.var"]])])
        number.of.cases.total <- na.omit(data[eval(parse(text = vars.list[["weight.var"]])) > 0, .(n_Cases = .N), keyby = key.vars])
        number.of.cases.total[ , (vars.list[["bckg.row.var"]]) := factor("Total")]
        setcolorder(x = number.of.cases.total, neworder = c(key.vars, vars.list[["bckg.row.var"]], "n_Cases"))
        number.of.cases <- rbindlist(l = list(number.of.cases, number.of.cases.total), use.names = TRUE, fill = TRUE)
        number.of.cases.total <- NULL
        if(length(key.vars) > 2) {
          keys.to.compute.on <- c(key.vars[2:length(key.vars)])
          keys.to.compute.on <- rep(x = list(keys.to.compute.on), times = length(keys.to.compute.on))
          keys.to.compute.on <- lapply(X = 1:length(keys.to.compute.on), FUN = function(i) {
            keys.to.compute.on[[i]][0:(length(keys.to.compute.on[[i]]) - i)]
          })
          keys.to.compute.on <- Filter(length, keys.to.compute.on)
          percentages.total <- lapply(X = keys.to.compute.on, FUN = function(i) {
            na.omit(data[ , c(Map(f = wgt.pct, variable = .(get(vars.list[["group.vars"]][1])), weight = mget(all.weights))), keyby = i])
          })
          number.of.cases.total <- lapply(X = keys.to.compute.on, FUN = function(i) {
            na.omit(data[eval(parse(text = vars.list[["weight.var"]])) > 0, .(n_Cases = .N), keyby = i])
          })
          number.of.cases.total <- rbindlist(l = number.of.cases.total, use.names = TRUE, fill = TRUE)
          number.of.cases.total[ , (key.vars[2:(length(key.vars) - 1)]) := lapply(.SD, function(i) {
            i <- factor(ifelse(test = is.na(i), yes = "Total", no = paste(i)), levels = c(levels(i), "Total"))
            droplevels(i)
          }), .SDcols = (key.vars[2:(length(key.vars) - 1)])]
          number.of.cases.lead.cols <- unique(number.of.cases[ , mget(c(vars.list[["group.vars"]][1], vars.list[["bckg.row.var"]]))])
          number.of.cases.lead.cols <- number.of.cases.lead.cols[get(vars.list[["bckg.row.var"]]) != "Total", ]
          tmp.lead.cols <- number.of.cases[ , mget(c(key.vars[c(1, length(key.vars))], vars.list[["bckg.row.var"]]))]
          tmp.lead.cols[ , (c(key.vars[length(key.vars)], vars.list[["bckg.row.var"]])) := lapply(.SD, function(i) {
            i <- factor("Total")
          }), .SDcols = c(key.vars[length(key.vars)], vars.list[["bckg.row.var"]])]
          tmp.lead.cols <- unique(tmp.lead.cols)
          number.of.cases.total <- cbind(tmp.lead.cols, number.of.cases.total)
          setcolorder(x = number.of.cases.total, neworder = c(key.vars, grep(pattern = "^V[[:digit:]]+$", x = colnames(number.of.cases.total), value = TRUE)))
          number.of.cases <- rbindlist(l = list(number.of.cases, number.of.cases.total), use.names = TRUE, fill = TRUE)
          tmp.lead.cols <- NULL
          number.of.cases.total <- NULL
        }
        number.of.cases.total <- na.omit(data[eval(parse(text = vars.list[["weight.var"]])) > 0, .(n_Cases = .N)])
        tmp.lead.cols <- number.of.cases[ , mget(key.vars)]
        tmp.lead.cols[ , (key.vars[2:length(key.vars)]) := lapply(.SD, function(i) {
          i <- factor("Total")
        }), .SDcols = key.vars[2:length(key.vars)]]
        tmp.lead.cols <- unique(tmp.lead.cols)
        number.of.cases.total <- cbind(tmp.lead.cols, factor("Total"), number.of.cases.total)
        setnames(x = number.of.cases.total, old = grep(pattern = "^V[[:digit:]]+$", x = colnames(number.of.cases.total)), new = vars.list[["bckg.row.var"]])
        number.of.cases <- rbindlist(l = list(number.of.cases, number.of.cases.total), use.names = TRUE, fill = TRUE)
        tmp.lead.cols <- NULL
        number.of.cases.total <- NULL
        sum.of.weights <- na.omit(data[ , lapply(.SD, sum), keyby = c(key.vars, vars.list[["bckg.row.var"]]), .SDcols = all.weights])
        sum.of.weights.total <- na.omit(data[ , lapply(.SD, sum), keyby = key.vars, .SDcols = all.weights])
        sum.of.weights.total[ , (vars.list[["bckg.row.var"]]) := factor("Total")]
        setcolorder(x = sum.of.weights.total, neworder = c(key.vars, vars.list[["bckg.row.var"]], grep(pattern = paste(c(key.vars, vars.list[["bckg.row.var"]]), collapse = "|"), x = colnames(sum.of.weights.total), value = TRUE, invert = TRUE)))
        sum.of.weights <- rbindlist(l = list(sum.of.weights, sum.of.weights.total), use.names = TRUE, fill = TRUE)
        sum.of.weights.total <- NULL
        if(length(key.vars) > 2) {
          keys.to.compute.on <- c(key.vars[2:length(key.vars)])
          keys.to.compute.on <- rep(x = list(keys.to.compute.on), times = length(keys.to.compute.on))
          keys.to.compute.on <- lapply(X = 1:length(keys.to.compute.on), FUN = function(i) {
            keys.to.compute.on[[i]][0:(length(keys.to.compute.on[[i]]) - i)]
          })
          keys.to.compute.on <- Filter(length, keys.to.compute.on)
          sum.of.weights.total <- lapply(X = keys.to.compute.on, FUN = function(i) {
            na.omit(data[ , lapply(.SD, sum), keyby = i, .SDcols = all.weights])
          })
          sum.of.weights.total <- rbindlist(l = sum.of.weights.total, use.names = TRUE, fill = TRUE)
          sum.of.weights.total[ , (key.vars[2:(length(key.vars) - 1)]) := lapply(.SD, function(i) {
            i <- factor(ifelse(test = is.na(i), yes = "Total", no = paste(i)), levels = c(levels(i), "Total"))
            droplevels(i)
          }), .SDcols = (key.vars[2:(length(key.vars) - 1)])]
          sum.of.weights.lead.cols <- unique(sum.of.weights[ , mget(c(vars.list[["group.vars"]][1], vars.list[["bckg.row.var"]]))])
          sum.of.weights.lead.cols <- sum.of.weights.lead.cols[get(vars.list[["bckg.row.var"]]) != "Total", ]
          tmp.lead.cols <- sum.of.weights[ , mget(c(key.vars[c(1, length(key.vars))], vars.list[["bckg.row.var"]]))]
          tmp.lead.cols[ , (c(key.vars[length(key.vars)], vars.list[["bckg.row.var"]])) := lapply(.SD, function(i) {
            i <- factor("Total")
          }), .SDcols = c(key.vars[length(key.vars)], vars.list[["bckg.row.var"]])]
          tmp.lead.cols <- unique(tmp.lead.cols)
          sum.of.weights.total <- cbind(tmp.lead.cols, sum.of.weights.total)
          setcolorder(x = sum.of.weights.total, neworder = c(key.vars, grep(pattern = "^V[[:digit:]]+$", x = colnames(sum.of.weights.total), value = TRUE)))
          sum.of.weights <- rbindlist(l = list(sum.of.weights, sum.of.weights.total), use.names = TRUE, fill = TRUE)
          tmp.lead.cols <- NULL
          sum.of.weights.total <- NULL
        }
        sum.of.weights.total <- na.omit(data[ , lapply(.SD, sum), .SDcols = all.weights])
        tmp.lead.cols <- number.of.cases[ , mget(key.vars)]
        tmp.lead.cols[ , (key.vars[2:length(key.vars)]) := lapply(.SD, function(i) {
          i <- factor("Total")
        }), .SDcols = key.vars[2:length(key.vars)]]
        tmp.lead.cols <- unique(tmp.lead.cols)
        sum.of.weights.total <- cbind(tmp.lead.cols, factor("Total"), sum.of.weights.total)
        setnames(x = sum.of.weights.total, old = grep(pattern = "^V[[:digit:]]+$", x = colnames(sum.of.weights.total)), new = vars.list[["bckg.row.var"]])
        sum.of.weights <- rbindlist(l = list(sum.of.weights, sum.of.weights.total), use.names = TRUE, fill = TRUE)
        tmp.lead.cols <- NULL
        sum.of.weights.total <- NULL
      } else {
        percentages <- na.omit(data[ , c(.(na.omit(unique(get(key.vars)))), Map(f = wgt.pct, variable = .(get(vars.list[["bckg.row.var"]])), weight = mget(all.weights))), by = eval(key.vars)])
        tmp.levels <- levels(droplevels(data[ , get(vars.list[["bckg.row.var"]])]))
        percentages[ , V1 := factor(x = tmp.levels, levels = tmp.levels)]
        percentages.total <- na.omit(data[ , Map(f = wgt.pct, variable = .(get(key.vars)), weight = mget(all.weights))])
        percentages.total <- cbind(unique(percentages[ , mget(key.vars)]), data.table("Total"), percentages.total)
        setnames(x = percentages.total, old = grep(pattern = "^V[[:digit:]]+$", x = colnames(percentages.total)), new = paste0("V", 1:length(grep(pattern = "^V[[:digit:]]+$", x = colnames(percentages.total)))))
        percentages <- rbindlist(l = list(percentages, percentages.total), use.names = TRUE, fill = TRUE)
        number.of.cases <- na.omit(data[ , .(n_Cases = .N), keyby = c(key.vars, vars.list[["bckg.row.var"]])])
        number.of.cases.total <- na.omit(data[ , .(n_Cases = .N)])
        number.of.cases.total <- cbind(unique(number.of.cases[ , mget(key.vars)]), data.table("Total"), number.of.cases.total)
        setnames(x = number.of.cases.total, old = "V1", new = vars.list[["bckg.row.var"]])
        number.of.cases <- rbindlist(l = list(number.of.cases, number.of.cases.total))
        sum.of.weights <- na.omit(data[ , lapply(.SD, sum), keyby = c(key.vars, vars.list[["bckg.row.var"]]), .SDcols = all.weights])
        sum.of.weights.total <- na.omit(data[ , lapply(.SD, sum), .SDcols = all.weights])
        sum.of.weights.total <- cbind(unique(sum.of.weights[ , mget(key.vars)]), data.table("Total"), sum.of.weights.total)
        setnames(x = sum.of.weights.total, old = "V1", new = vars.list[["bckg.row.var"]])
        sum.of.weights <- rbindlist(l = list(sum.of.weights, sum.of.weights.total))
      }
      bckg.crosstabs <- list(compute.crosstabs.all.repwgt(data.object = data, var1 = vars.list[["bckg.row.var"]], var2 = vars.list[["bckg.col.var"]], exp.cnts = expected.cnts, pcts.in.rows = row.pcts, pcts.in.cols = column.pcts, pcts.total = total.pcts, keys = key.vars, weight = all.weights))
      percentages <- list(percentages)
      sum.of.weights <- list(sum.of.weights)
      if(!is.null(vars.list[["pcts.var"]])) {
        reshape.list.statistics.bckg(estimate.object = percentages, estimate.name = "Percentages_", bckg.vars.vector = vars.list[["bckg.row.var"]], weighting.variable = vars.list[["weight.var"]], data.key.variables = key.vars, new.names.vector = vars.list[["bckg.row.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
      } else {
        reshape.list.statistics.bckg(estimate.object = percentages, estimate.name = "Percentages_", bckg.vars.vector = vars.list[["bckg.row.var"]], weighting.variable = vars.list[["weight.var"]], data.key.variables = key.vars, new.names.vector = c(vars.list[["bckg.row.var"]]), replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
      }
      percentages <- rbindlist(percentages)
      if(nrow(number.of.cases) > nrow(percentages)) {
        percentages <- merge(number.of.cases[ , mget(c(key.vars, vars.list[["bckg.row.var"]]))], percentages, by = c(key.vars, vars.list[["bckg.row.var"]]), all.x = TRUE)
        percentages[ , (grep(pattern = "Percentages_[[:alnum:]]+$", x = colnames(percentages), value = TRUE)) := lapply(.SD, function(i){i[is.na(i)] <- 100; i}), .SDcols = grep(pattern = "Percentages_[[:alnum:]]+$", x = colnames(percentages), value = TRUE)]
        percentages[ , (grep(pattern = "Percentages_[[:alnum:]]+_SE$", x = colnames(percentages), value = TRUE)) := lapply(.SD, function(i){i[is.na(i)] <- 0; i}), .SDcols = grep(pattern = "Percentages_[[:alnum:]]+_SE$", x = colnames(percentages), value = TRUE)]
      }
      reshape.list.statistics.bckg(estimate.object = sum.of.weights, estimate.name = "Sum_", weighting.variable = vars.list[["weight.var"]], data.key.variables = key.vars, new.names.vector = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
      reshape.list.statistics.bckg(estimate.object = bckg.crosstabs, estimate.name = "Crosstab", data.key.variables = key.vars, new.names.vector = "", bckg.vars.vector = "", weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
      bckg.crosstabs <- lapply(X = bckg.crosstabs, FUN = function(i) {
        i <- dcast(data = i, formula = as.formula(paste0(paste(key.vars, collapse = " + "), " + ", vars.list[["bckg.row.var"]], " + ", "Type", " ~ ", vars.list[["bckg.col.var"]])), value.var = c("Crosstab", "Crosstab_SE"))
        setnames(x = i, old = colnames(i), new = gsub(pattern = "[[:space:]]+", replacement = "_", x = colnames(i)))
      })
      if(file.attributes[["lsa.study"]] %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "TiPi", "CivED", "ICCS", "ICILS", "SITES", "REDS")) {
        Rao.Scott.design.var.scale.fac <- 1
        if(file.attributes[["lsa.study"]] %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "TiPi") && shortcut == FALSE) {
          Rao.Scott.deg.freedom <- (length(all.weights) - 1) / 2
        } else if(file.attributes[["lsa.study"]] %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "TiPi") && shortcut == TRUE) {
          Rao.Scott.deg.freedom <- (length(all.weights) - 2)
        } else if(file.attributes[["lsa.study"]] %in% c("CivED", "ICCS", "ICILS", "SITES", "REDS")) {
          Rao.Scott.deg.freedom <- (length(all.weights) - 2)
        }
      } else if(file.attributes[["lsa.study"]] %in% c("TEDS-M", "PISA", "PISA for Development", "TALIS", "TALIS 3S")) {
        Rao.Scott.design.var.scale.fac <- 0.05
        Rao.Scott.deg.freedom <- length(all.weights) - 2
      }
      country.Rao.Scott.adj.chi.sq <- compute.Rao.Scott.adj.chi.sq(data.obj = data, var1 = vars.list[["bckg.row.var"]], var2 = vars.list[["bckg.col.var"]], weights = all.weights, des.scale.fac = Rao.Scott.design.var.scale.fac, deg.freedom = Rao.Scott.deg.freedom, miss.to.include = include.missing, keys = key.vars)
      if(exists("cnt.warn.insuff.RS.collector")) {
        warnings.collector[["insufficient.cases"]] <<- c(warnings.collector[["insufficient.cases"]], cnt.warn.insuff.RS.collector)
      }
      cnt.RS.chi.sq.name <- unique(as.character(country.Rao.Scott.adj.chi.sq[ , get(key.vars[1])]))
      Rao.Scott.adj.chi.sq[[cnt.RS.chi.sq.name]] <<- country.Rao.Scott.adj.chi.sq
      country.analysis.info <- produce.analysis.info(cnt.ID = unique(data[ , get(key.vars[[1]])]), data = used.data, study = file.attributes[["lsa.study"]], cycle = file.attributes[["lsa.cycle"]], weight.variable = vars.list[["weight.var"]], rep.design = DESIGN, used.shortcut = shortcut, number.of.reps = rep.wgts.names, in.time = cnt.start.time)
      analysis.info[[country.analysis.info[ , as.character(COUNTRY)]]] <<- country.analysis.info
      setkeyv(x = number.of.cases, cols = c(key.vars, vars.list[["bckg.row.var"]]))
      setkeyv(x = sum.of.weights[[1]], cols = c(key.vars, vars.list[["bckg.row.var"]]))
      setkeyv(x = percentages, cols = c(key.vars, vars.list[["bckg.row.var"]]))
      setkeyv(x = bckg.crosstabs[[1]], cols = c(key.vars, vars.list[["bckg.row.var"]]))
      merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.crosstabs))
      counter <<- counter + 1
      message("     ",
              if(nchar(counter) == 1) {
                paste0("( ", counter, "/", number.of.countries, ")   ")
              } else if(nchar(counter) == 2) {
                paste0("(", counter, "/", number.of.countries, ")   ")
              },
              paste0(str_pad(string = unique(merged.outputs[[1]]), width = 40, side = "right"), " processed in ", country.analysis.info[ , DURATION]))
      return(merged.outputs)
    }
    estimates <- rbindlist(lapply(X = data, FUN = compute.all.stats), use.names = TRUE, fill = TRUE)
    estimates.bckg.col.var.underscored <- gsub(pattern = "[[:space:]]+", replacement = "_", x = bckg.vars.levels[["col.levels"]])
    estimates.NOT.bckg.col <- grep(pattern = paste(estimates.bckg.col.var.underscored, collapse = "|"), x = colnames(estimates), value = TRUE, invert = TRUE)
    estimates.NOT.bckg.col.lead <- grep(pattern = "_Total$|_SE_Total$", x = estimates.NOT.bckg.col, value = TRUE, invert = TRUE)
    estimates.NOT.bckg.col.end <- grep(pattern = "_Total$|_SE_Total$", x = estimates.NOT.bckg.col, value = TRUE)
    setcolorder(x = estimates, neworder = c(
      grep(pattern = paste(estimates.NOT.bckg.col.lead, collapse = "|"), x = colnames(estimates), value = TRUE),
      grep(pattern = paste(estimates.bckg.col.var.underscored, collapse = "|"), x = colnames(estimates), value = TRUE),
      grep(pattern = paste(estimates.NOT.bckg.col.end, collapse = "|"), x = colnames(estimates), value = TRUE)
    ))
    estimates[ , colnames(estimates)[1] := as.character(estimates[ , get(colnames(estimates)[1])])]
    setkeyv(x = estimates, cols = key.vars)
    total.exec.time <- rbindlist(analysis.info)[ , DURATION]
    total.exec.time.millisec <- sum(as.numeric(str_extract(string = total.exec.time, pattern = "[[:digit:]]{3}$")))/1000
    total.exec.time <- sum(as.ITime(total.exec.time), total.exec.time.millisec)
    if(length(unique(estimates[ , get(key.vars[1])])) > 1) {
      message("\nAll ", length(unique(estimates[ , get(key.vars[1])])), " countries with valid data processed in ", format(as.POSIXct("0001-01-01 00:00:00") + total.exec.time - 1, "%H:%M:%OS3"))
    } else {
      message("")
    }
    ptm.add.table.average <- proc.time()
    estimates <- estimates[!is.na(Type), ]
    estimates <- compute.table.average(output.obj = estimates, object.variables = vars.list, data.key.variables = c(key.vars, vars.list[["bckg.row.var"]], "Type"), data.properties = file.attributes)
    message('"Table Average" added to the estimates in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.add.table.average}[[3]], "%H:%M:%OS3"), "\n")
    SE.cols.num.pos <- grep(pattern = "Crosstab_SE_", x = colnames(estimates))
    SE.cols <- grep(pattern = "Crosstab_SE_", x = colnames(estimates), value = TRUE)
    setnames(x = estimates, old = SE.cols, new = paste0(SE.cols, "_SE"))
    SE.cols <- grep(pattern = "Crosstab_SE_", x = colnames(estimates), value = TRUE)
    setnames(x = estimates, old = SE.cols, new = gsub(pattern = "Crosstab_SE_", replacement = paste0(vars.list[["bckg.col.var"]], "_"), x = SE.cols))
    estimate.cols.num.pos <- grep(pattern = "Crosstab_", x = colnames(estimates))
    estimate.cols <- grep(pattern = "Crosstab_", x = colnames(estimates), value = TRUE)
    setnames(x = estimates, old = estimate.cols, new = gsub(pattern = "Crosstab_", replacement = paste0(vars.list[["bckg.col.var"]], "_"), x = estimate.cols))
    new.order <- colnames(estimates)[c(rbind(estimate.cols.num.pos, SE.cols.num.pos))]
    setcolorder(x = estimates, neworder = c(key.vars, grep(pattern = "n_Cases|Sum_|Percentages_", x = colnames(estimates), value = TRUE), vars.list[["bckg.row.var"]], "Type", new.order))
    Total.cols <- paste0(vars.list[["bckg.col.var"]], c("_Total", "_Total_SE"))
    setnames(x = estimates, old = Total.cols, new = c("Total", "Total_SE"))
    ptm.add.RS.chi.square <- proc.time()
    Rao.Scott.adj.chi.sq <- rbindlist(l = Rao.Scott.adj.chi.sq)
    message('Rao-Scott adjusted chi-square statistics table assembled in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.add.RS.chi.square}[[3]], "%H:%M:%OS3"), "\n")
    export.results(output.object = estimates, analysis.type = action.args.list[["executed.analysis.function"]], Rao.Scott.adj.chi.sq.obj = Rao.Scott.adj.chi.sq, analysis.info.obj = rbindlist(l = analysis.info), destination.file = output.file, open.exported.file = open.output)
    if(exists("removed.countries.where.any.split.var.is.all.NA") && length(removed.countries.where.any.split.var.is.all.NA) > 0) {
      warning('Some of the countries had one or more splitting variables which contains only missing values. These countries are: "', paste(removed.countries.where.any.split.var.is.all.NA, collapse = '", "'), '".', call. = FALSE)
    }
  }, interrupt = function(f) {
    message("\nInterrupted by the user. Computations are not finished and output file is not produced.\n")
  })
  vars.list.analysis.vars <- grep(pattern = "split.vars|bckg.row.var|bckg.col.var", x = names(vars.list), value = TRUE)
  vars.list.analysis.vars <- unlist(vars.list[vars.list.analysis.vars])
  vars.list.analysis.vars <- grep(pattern = paste(unique(unlist(studies.all.design.variables)), collapse = "|"), x = vars.list.analysis.vars, value = TRUE)
  if(length(vars.list.analysis.vars) > 0) {
    warning('Some of the variables specified as analysis variables (in "split.vars" and/or "bckg.avg.vars") are design variables (sampling variables or PVs). This kind of variables shall not be used for analysis. Check your input.', call. = FALSE)
  }
  if(length(warnings.collector) > 0) {
    if(!is.null(warnings.collector[["data.no.JKZONE.JKREP"]])) {
      warning(warnings.collector[["data.no.JKZONE.JKREP"]], call. = FALSE)
    }
    if(!is.null(warnings.collector[["removed.countries.row.col.vars"]])) {
      warning(warnings.collector[["removed.countries.row.col.vars"]], call. = FALSE)
    }
    if(!is.null(warnings.collector[["insufficient.cases"]])) {
      warning(paste0('In one or more countries in the data some split combinations did not contain sufficient combinations between the "bckg.row.var" and "bckg.col.var" to compute the Rao-Scott first- and second-order chi-square adjustments: ', paste(warnings.collector[["insufficient.cases"]], collapse = ", "), '.\n These split combinations were removed and the statistics for them are not to be found in the output.'), call. = FALSE)
    }
  }
}
