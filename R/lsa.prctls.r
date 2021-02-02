#' @title Compute percentiles of continuous variables within groups
#'
#' @description \code{lsa.prctls} computes percentiles of continuous variables within groups defined by one or more variables.
#'
#' @param data.file        The file containing \code{lsa.data} object. Either this or \code{data.object}
#'                         shall be specified, but not both. See details.
#' @param data.object      The object in the memory containing \code{lsa.data} object. Either this or
#'                         \code{data.file} shall be specified, but not both. See details.
#' @param split.vars       Categorical variable(s) to split the results by. If no split variables are
#'                         provided, the results will be for the overall countries' populations. If
#'                         one or more variables are provided, the results will be split by all but
#'                         the last variable and the percentages of respondents will be computed by
#'                         the unique values of the last splitting variable.
#' @param bckg.prctls.vars Name(s) of continuous background or contextual variable(s) to compute
#'                         the percentiles for. The results will be computed by all groups specified
#'                         by the splitting variables. See details.
#' @param PV.root.prctls   The root name(s) for the set(s) of plausible values. See details.
#' @param prctls           Vector of integers specifying the percentiles to be computed, the default
#'                         is \code{c(5, 25, 50, 75, 95)}. See examples.
#' @param weight.var       The name of the variable containing the weights. If no name of a weight
#'                         variable is provide, the function will automatically select the default
#'                         weight variable for the provided data, depending on the respondent type.
#' @param include.missing  Logical, shall the missing values of the splitting variables be included
#'                         as categories to split by and all statistics produced for them? The
#'                         default (\code{FALSE}) takes all cases on the splitting variables
#'                         without missing values before computing any statistics. See details.
#' @param shortcut         Logical, shall the "shortcut" method for IEA TIMSS, TIMSS Advanced,
#'                         TIMSS Numeracy, eTIMSS, PIRLS, ePIRLS, PIRLS Literacy and RLII be
#'                         applied when using PVs? The default (\code{FALSE}) applies the "full"
#'                         design when computing the variance components and the standard errors of
#'                         the PV estimates.
#' @param output.file      Full path to the output file including the file name. If omitted, a file
#'                         with a default file name "Analysis.xlsx" will be written to the working
#'                         directory (\code{getwd()}).
#' @param open.output      Logical, shall the output be open after it has been written? The default
#'                         (\code{TRUE}) opens the output in the default spreadsheet program
#'                         installed on the computer.
#'
#' @details
#' Either \code{data.file} or \code{data.object} shall be provided as source of data. If both of them are provided, the function will stop with an error message.
#'
#' The function computes percentiles of variables (background/contextual or sets of plausible values) by groups defined by one or more categorical variables (splitting variables). Multiple splitting variables can be added, the function will compute the percentages for all formed groups and their percentiles on the continuous variables. If no splitting variables are added, the results will be computed only by country.
#'
#' Multiple continuous background variables can be provided to compute the specified percentiles for them. Please note that in this case the results will slightly differ compared to using each of the same background continuous variables in separate analyses. This is because the cases with the missing values on \code{bckg.prctls.vars} are removed in advance and the more variables are provided to \code{bckg.prctls.vars}, the more cases are likely to be removed.
#'
#' Computation of percentiles involving plausible values requires providing a root of the plausible values names in \code{PV.root.prctls}. All studies (except CivED, TEDS-M, SITES, TALIS and TALIS Starting Strong Survey) have a set of PVs per construct (e.g. in TIMSS five for overall mathematics, five for algebra, five for geometry, etc.). In some studies (say TIMSS and PIRLS) the names of the PVs in a set always start with character string and end with sequential number of the PV. For example, the names of the set of PVs for overall mathematics in TIMSS are BSMMAT01, BSMMAT02, BSMMAT03, BSMMAT04 and BSMMAT05. The root of the PVs for this set to be added to \code{PV.root.prctls} will be "BSMMAT". The function will automatically find all the variables in this set of PVs and include them in the analysis. In other studies like OECD PISA and IEA ICCS and ICILS the sequential number of each PV is included in the middle of the name. For example, in ICCS the names of the set of PVs are PV1CIV, PV2CIV, PV3CIV, PV4CIV and PV5CIV. The root PV name has to be specified in \code{PV.root.prctls} as "PV#CIV". More than one set of PVs can be added. Note, however, that providing continuous variable(s) for the \code{bckg.prctls.vars} argument and root PV for the \code{PV.root.prctls} argument will affect the results for the PVs because the cases with missing on \code{bckg.prctls.vars} will be removed and this will also affect the results from the PVs. On the other hand, using more than one set of PVs at the same time should not affect the results on any PV estimates because PVs shall not have any missing values.
#'
#' If \code{include.missing = FALSE} (default), all cases with missing values on the splitting variables will be removed and only cases with valid values will be retained in the statistics. Note that the data from the studies can be exported in two different ways using the \code{lsa.convert.data}: (1) setting all user-defined missing values to \code{NA}; and (2) importing all user-defined missing values as valid ones and adding their codes in an additional attribute to each variable. If the \code{include.missing} in \code{lsa.prctls} is set to \code{FALSE} (default) and the data used is exported using option (2), the output will remove all values from the variable matching the values in its \code{missings} attribute. Otherwise, it will include them as valid values and compute statistics for them.
#'
#' The \code{shortcut} argument is valid only for TIMSS, TIMSS Advanced, TIMSS Numeracy, PIRLS, ePIRLS, PIRLS Literacy and RLII. Previously, in computing the standard errors, these studies were using 75 replicates because one of the schools in the 75 JK zones had its weights doubled and the other one has been taken out. Since TIMSS 2015 and PIRLS 2016 the studies use 150 replicates and in each JK zone once a school has its weights doubled and once taken out, i.e. the computations are done twice for each zone. For more details see Foy & LaRoche (2016) and Foy & LaRoche (2017). If replication of the tables and figures is needed, the \code{shortcut} argument has to be changed to \code{TRUE}.
#'
#' @return
#' A MS Excel (\code{.xlsx}) file (which can be opened in any spreadsheet program), as specified with the full path in the \code{output.file}. If the argument is missing, an Excel file with the generic file name "Analysis.xlsx" will be saved in the working directory (\code{getwd()}). The workbook contains three spreadsheets. The first one ("Estimates") contains a table with the results by country and the final part of the table contains averaged results from all countries' statistics. The following columns can be found in the table, depending on the specification of the analysis:
#'
#' \itemize{
#'   \item \verb{<}Country ID\verb{>} - a column containing the names of the countries in the file for which statistics are computed. The exact column header will depend on the country identifier used in the particular study.
#'   \item \verb{<}Split variable 1\verb{>}, \verb{<}Split variable 2\verb{>}... - columns containing the categories by which the statistics were split by. The exact names will depend on the variables in \code{split.vars}.
#'   \item n_Cases - the number of cases in the sample used to compute the statistics.
#'   \item Sum_\verb{<}Weight variable\verb{>} - the estimated population number of elements per group after applying the weights. The actual name of the weight variable will depend on the weight variable used in the analysis.
#'   \item Sum_\verb{<}Weight variable\verb{>}\verb{_}SE - the standard error of the the estimated population number of elements per group. The actual name of the weight variable will depend on the weight variable used in the analysis.
#'   \item Percentages_\verb{<}Last split variable\verb{>} - the percentages of respondents (population estimates) per groups defined by the splitting variables in \code{split.vars}. The percentages will be for the last splitting variable which defines the final groups.
#'   \item Percentages_\verb{<}Last split variable\verb{>}\verb{_}SE - the standard errors of the percentages from above.
#'   \item Prctl_\verb{<}Percentile value\verb{>}\verb{_}\verb{<}Background variable\verb{>} - the percentile of the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.prctls.vars}. There will be one column for each percentile estimate for each variable specified in \code{bckg.prctls.vars}.
#'   \item Prctl_\verb{<}Percentile value\verb{>}\verb{_}\verb{<}Background variable\verb{>}\verb{_}SE - the standard error of the percentile of the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.prctls.vars}. There will be one column with the SE per percentile estimate for each variable specified in \code{bckg.prctls.vars}.
#'   \item Percent_Missings_\verb{<}Background variable\verb{>} - the percentage of missing values for the \verb{<}Background variable\verb{>} specified in \code{bckg.prctls.vars}. There will be one column with the percentage of missing values for each variable specified in \code{bckg.prctls.vars}.
#'   \item Prctl_\verb{<}Percentile value\verb{>}\verb{_}\verb{<}root PV\verb{>} - the percentile of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.prctls}. There will be one column per percentile value estimate for each set of PVs specified in \code{PV.root.prctls}.
#'   \item Prctl_\verb{<}Percentile value\verb{>}\verb{_}\verb{<}root PV\verb{>}\verb{_}SE - the standard error per percentile per set of PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.prctls}. There will be one column with the standard error of estimate per percentile per set of PVs specified in \code{PV.root.prctls}.
#'   \item Prctl_\verb{<}Percentile value\verb{>}\verb{_}\verb{<}root PV\verb{>}\verb{_}SVR - the sampling variance component per percentile per set of PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.prctls}. There will be one column with the sampling variance component percentile estimate for each set of PVs specified in \code{PV.root.prctls}.
#'   \item Prctl_\verb{<}Percentile value\verb{>}\verb{_}\verb{<}root PV\verb{>}\verb{_}MVR - the measurement variance component per percentiles per set of PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.prctls}. There will be one column with the measurement variance component per percentile per set of PVs specified in \code{PV.root.prctls}.
#'   \item Percent_Missings_\verb{<}root PV\verb{>} - the percentage of missing values for the \verb{<}root PV\verb{>} specified in \code{PV.root.prctls}. There will be one column with the percentage of missing values for each set of PVs specified in \code{PV.root.prctls}.
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
#' # Compute the 5th, 25th and 50th percentiles of the complex background scale "Students like
#' # learning mathematics" by student sex and frequency of using computer or tablet at home using
#' # TIMSS 2015 grade 8 data loaded in memory, without shortcut, exclude the cases with missing
#' # values in the splitting variables, and use the default (TOTWGT) weights
#' \dontrun{
#' lsa.pcts.prctls(data.object = T15_G8_student_data, split.vars = c("BSBG01", "BSBG13A"),
#' bckg.prctls.vars = "BSBGSLM", prctls = c(5, 25, 50),
#' output.file = "C:/temp/test.xlsx", open.output = TRUE)
#' }
#'
#' # Repeat the analysis from above, this time with shortcut, include the cases with missing
#' # values in the splitting variables, and use the senate weights
#' \dontrun{
#' lsa.pcts.prctls(data.object = T15_G8_student_data, split.vars = c("BSBG01", "BSBG13A"),
#' bckg.prctls.vars = "BSBGSLM", prctls = c(5, 25, 50), weight.var = "SENWGT",
#' include.missing = TRUE, shortcut = TRUE, output.file = "C:/temp/test.xlsx",
#' open.output = TRUE)
#' }
#'
#' # Repeat the analysis from above, adding a second continuous variable to compute the
#' # percentiles for, the "Students Like Learning Science" complex scale
#' \dontrun{
#' lsa.pcts.prctls(data.object = T15_G8_student_data, split.vars = c("BSBG01", "BSBG13A"),
#' bckg.prctls.vars = c("BSBGSLM", "BSBGSLS"), prctls = c(5, 25, 50), weight.var = "SENWGT",
#' include.missing = FALSE, shortcut = TRUE,
#' output.file = "C:/temp/test.xlsx", open.output = TRUE)
#' }
#'
#' # Compute the 5th, 25th and 50th percentiles for the student overall reading achievement
#' # scores (i.e. using a set of PVs), using PIRLS 2016 student data file, split the output
#' # by student sex, use the full design, include the missing values od the splitting variable
#' # (i.e. student sex), and do not open the output after the computations are finished
#' \dontrun{
#' lsa.pcts.prctls(data.file = "C:/Data/PIRLS_2016_Student_Miss_to_NA.RData",
#' split.vars = "ASBG01", PV.root.prctls = "ASRREA", prctls = c(5, 25, 50),
#' include.missing = TRUE, output.file = "C:/temp/test.xlsx", open.output = FALSE)
#' }
#'
#' @references
#' LaRoche, S., Joncas, M., & Foy, P. (2016). Sample Design in TIMSS 2015. In M. O. Martin, I. V. S. Mullis, & M. Hooper (Eds.), \emph{Methods and Procedures in TIMSS 2015} (pp. 3.1-3.37). Chestnut Hill, MA: TIMSS & PIRLS International Study Center.
#' LaRoche, S., Joncas, M., & Foy, P. (2017). Sample Design in PIRLS 2016. In M. O. Martin, I. V. S. Mullis, & M. Hooper (Eds.), \emph{Methods and Procedures in PIRLS 2016} (pp. 3.1-3.34). Chestnut Hill, MA: Lynch School of Education, Boston College.
#'
#' @seealso \code{\link{lsa.convert.data}}
#' @export


lsa.prctls <- function(data.file, data.object, split.vars, bckg.prctls.vars, PV.root.prctls, prctls = c(5, 25, 50, 75, 95), weight.var, include.missing = FALSE, shortcut = FALSE, output.file, open.output = TRUE) {
  
  tmp.options <- options(scipen = 999, digits = 22)
  on.exit(expr = options(tmp.options), add = TRUE)
  
  prctls <- sort(prctls/100)
  
  if(!missing(data.file) == TRUE && !missing(data.object) == TRUE) {
    stop('Either "data.file" or "data.object" has to be provided, but not both. All operations stop here. Check your input.\n\n', call. = FALSE)
  } else if(!missing(data.file)) {
    if(file.exists(data.file) == FALSE) {
      stop('The file specified in the "data.file" argument does not exist. All operations stop here. Check your input.\n\n', call. = FALSE)
    }
    
    if(missing(bckg.prctls.vars) & missing(PV.root.prctls)) {
      stop('No background variables in "bckg.prctls.vars" or PV roots in "PV.root.prctls" are provided. All operations stop here. Check your input.\n\n', call. = FALSE)
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
  
  vars.list <- get.analysis.and.design.vars(data)
  
  if(!is.null(vars.list[["bckg.prctls.vars"]]) && any(sapply(X = data[ , mget(vars.list[["bckg.prctls.vars"]])], class) != "numeric")) {
    stop('One or more variables passed to the "bckg.prctls.vars" is not numeric. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  
  action.args.list <- get.action.arguments()
  
  file.attributes <- get.file.attributes(imported.object = data)
  
  tryCatch({
    
    if(file.attributes[["lsa.study"]] %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "TIMSS Advanced", "TiPi") & missing(shortcut)) {
      action.args.list[["shortcut"]] <- FALSE
    }
    
    data <- produce.analysis.data.table(data.object = data, object.variables = vars.list, action.arguments = action.args.list, imported.file.attributes = file.attributes)
    
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
        bckg.prctls.vars.all.NA <- names(Filter(function(i) {all(is.na(i))}, data))
        if(length(bckg.prctls.vars.all.NA) > 0) {
          data1 <- copy(data)
          data1[ , (bckg.prctls.vars.all.NA) := 0]
          data1 <- na.omit(object = data1)
        } else {
          data1 <- na.omit(object = data)
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
        
        if(!is.null(vars.list[["bckg.prctls.vars"]])) {
          bckg.prctls <- na.omit(data1[ , Map(f = wgt.prctl, variable = mget(vars.list[["bckg.prctls.vars"]]), weight = mget(rep(all.weights, each = length(prctls)*length(vars.list[["bckg.prctls.vars"]]))), prctls.values = prctls), by = eval(key.vars)])
          setnames(bckg.prctls, c(key.vars, paste0("V", 1:(length(all.weights)*length(prctls)*length(bckg.prctls.vars)))))
          
          bckg.vars.pct.miss <- compute.cont.vars.pct.miss(vars.vector = vars.list[["bckg.prctls.vars"]], data.object = data, weight.var = all.weights, keys = key.vars)
          
          bckg.vars.pct.miss <- na.omit(object = bckg.vars.pct.miss, cols = key.vars)
        }
        
        if(!is.null(vars.list[["PV.root.prctls"]])) {
          PV.prctls <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
            lapply(X = i, FUN = function(j) {
              data1[ , Map(f = wgt.prctl, variable = mget(j), weight = mget(rep(all.weights, each = length(prctls))), prctls.values = prctls), by = eval(key.vars)]
            })
          })
          
          lapply(X = PV.prctls, FUN = function(i) {
            lapply(X = i, FUN = function(j) {
              setnames(j, c(key.vars, paste0("V", 1:(length(all.weights)*length(prctls)))))
            })
          })
          
          PVs.pct.miss <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
            compute.cont.vars.pct.miss(vars.vector = i, data.object = na.omit(object = data, cols = key.vars), weight.var = all.weights, keys = key.vars)
          })
          
        }
        
      } else if (include.missing == TRUE) {
        
        bckg.prctls.vars.all.NA <- names(Filter(function(i) {all(is.na(i))}, data))
        if(length(bckg.prctls.vars.all.NA) > 0) {
          data1 <- copy(data)
          data1[ , (bckg.prctls.vars.all.NA) := 0]
          data1 <- na.omit(object = data1, cols = vars.list[["bckg.prctls.vars"]][!vars.list[["bckg.prctls.vars"]] %in% bckg.prctls.vars.all.NA])
        } else {
          data1 <- na.omit(object = data, cols = unlist(vars.list["bckg.prctls.vars"]))
        }
        
        if(!is.null(vars.list[["pcts.var"]])) {
          
          percentages <- data1[ , c(.(na.omit(unique(get(vars.list[["pcts.var"]])))), Map(f = wgt.pct, variable = .(get(vars.list[["pcts.var"]])), weight = mget(all.weights))), by = eval(vars.list[["group.vars"]])]
          
          number.of.cases <- data1[eval(parse(text = vars.list[["weight.var"]])) > 0, .(n_Cases = .N), by = key.vars]
          sum.of.weights <- data1[ , lapply(.SD, sum), by = key.vars, .SDcols = all.weights]
          
        } else {
          if(!is.null(vars.list[["bckg.prctls.vars"]])) {
            
            percentages <- data1[ , c(.(na.omit(unique(get(key.vars)))), Map(f = wgt.pct, variable = .(get(key.vars)), weight = mget(all.weights)))]
            
            number.of.cases <- data1[ , .(n_Cases = .N), by = key.vars]
            sum.of.weights <- data1[ , lapply(.SD, sum), by = key.vars, .SDcols = all.weights]
          } else {
            percentages <- data[ , c(.(na.omit(unique(get(key.vars)))), Map(f = wgt.pct, variable = .(get(key.vars)), weight = mget(all.weights)))]
            number.of.cases <- data[ , .(n_Cases = .N), by = key.vars]
            sum.of.weights <- data[ , lapply(.SD, sum), by = key.vars, .SDcols = all.weights]
          }
        }
        
        if(!is.null(vars.list[["bckg.prctls.vars"]])) {
          
          bckg.prctls <- na.omit(data1[ , Map(f = wgt.prctl, variable = mget(vars.list[["bckg.prctls.vars"]]), weight = mget(rep(all.weights, each = length(prctls)*length(vars.list[["bckg.prctls.vars"]]))), prctls.values = prctls), by = eval(key.vars)])
          
          setnames(bckg.prctls, c(key.vars, paste0("V", 1:(length(all.weights)*length(prctls)*length(bckg.prctls.vars)))))
          
          bckg.vars.pct.miss <- compute.cont.vars.pct.miss(vars.vector = vars.list[["bckg.prctls.vars"]], data.object = data, weight.var = all.weights, keys = key.vars)
        }
        
        if(!is.null(vars.list[["PV.root.prctls"]])) {
          
          PV.prctls <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
            lapply(X = i, FUN = function(j) {
              data[ , Map(f = wgt.prctl, variable = mget(j), weight = mget(rep(all.weights, each = length(prctls))), prctls.values = prctls), by = eval(key.vars)]
            })
          })
          
          lapply(X = PV.prctls, FUN = function(i) {
            lapply(X = i, FUN = function(j) {
              setnames(j, c(key.vars, paste0("V", 1:(length(all.weights)*length(prctls)))))
            })
          })
          
          PVs.pct.miss <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
            compute.cont.vars.pct.miss(vars.vector = i, data.object = data, weight.var = all.weights, keys = key.vars)
          })
        }
        
      }
      
      percentages <- list(percentages)
      sum.of.weights <- list(sum.of.weights)
      if(!is.null(vars.list[["bckg.prctls.vars"]])) {
        bckg.prctls <- list(bckg.prctls)
      }
      
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
      
      if(!is.null(vars.list[["bckg.prctls.vars"]])) {
        
        reshape.list.statistics.bckg(estimate.object = bckg.prctls, estimate.name = "Prctls_", data.key.variables = key.vars, new.names.vector = vars.list[["bckg.prctls.vars"]], bckg.vars.vector = vars.list[["bckg.prctls.vars"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut, multiply.columns = prctls)
        
        bckg.prctls <- Reduce(function(...) merge(...), bckg.prctls)
        
        if(!is.null(bckg.prctls.vars.all.NA) & length(bckg.prctls.vars.all.NA) > 0) {
          prctls.cols <- grep(pattern = paste(bckg.prctls.vars.all.NA, collapse = "|"), x = colnames(bckg.prctls), value = TRUE)
          bckg.prctls[ , (prctls.cols) := NaN]
        }
        
      }
      
      if(!is.null(vars.list[["PV.root.prctls"]])) {
        
        reshape.list.statistics.PV(estimate.object = PV.prctls, estimate.name = "Prctls_", PV.vars.vector = vars.list[["PV.names"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut, multiply.columns = prctls)
        
        PV.prctls <- lapply(X = PV.prctls, FUN = function(i) {
          Reduce(function(...) merge(...), i)
        })
        
        aggregate.PV.estimates(estimate.object = PV.prctls, estimate.name = "Prctls_", root.PV = vars.list[["PV.root.prctls"]], PV.vars.vector = vars.list[["PV.names"]], data.key.variables = key.vars, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
        
        PV.prctls <- lapply(X = PV.prctls, FUN = function(i) {
          tmp <- split(x = i, by = "Percentiles")
          
          tmp <- lapply(X = tmp, FUN = function(j) {
            
            if(file.attributes[["lsa.study"]] %in% c("PISA", "PISA for Development", "ICCS", "ICILS")) {
              PV.root.column <- gsub(pattern = "\\#", replacement = "N", x = PV.root.prctls)
              prctls.column <- grep(pattern = paste(paste0("Prctls_", PV.root.column, "$"), collapse = "|"), x = colnames(j), value = TRUE)
              prctls.SE.column <- grep(pattern = paste(paste0("Prctls_", PV.root.column, "_SE$"), collapse = "|"), x = colnames(j), value = TRUE)
              prctls.SVR.column <- grep(pattern = paste(paste0("Prctls_", PV.root.column, "_SVR$"), collapse = "|"), x = colnames(j), value = TRUE)
              prctls.MVR.column <- grep(pattern = paste(paste0("Prctls_", PV.root.column, "_MVR$"), collapse = "|"), x = colnames(j), value = TRUE)
            } else {
              PV.root.column <- gsub(pattern = "Prctls_|_SE", replacement = "", x = colnames(i))
              PV.root.column <- unique(PV.root.column)[which(unique(PV.root.column) %in% PV.root.prctls)]
              prctls.column <- grep(pattern = paste(paste0("Prctls_", PV.root.prctls, "$"), collapse = "|"), x = colnames(j), value = TRUE)
              prctls.SE.column <- grep(pattern = paste(paste0("Prctls_", PV.root.prctls, "_SE$"), collapse = "|"), x = colnames(j), value = TRUE)
              prctls.SVR.column <- grep(pattern = paste(paste0("Prctls_", PV.root.prctls, "_SVR$"), collapse = "|"), x = colnames(j), value = TRUE)
              prctls.MVR.column <- grep(pattern = paste(paste0("Prctls_", PV.root.prctls, "_MVR$"), collapse = "|"), x = colnames(j), value = TRUE)
            }
            
            setnames(x = j, old = prctls.column, new = paste0(j[ , Percentiles][1], "_", PV.root.column))
            setnames(x = j, old = prctls.SE.column, new = paste0(j[ , Percentiles][1], "_", PV.root.column, "_SE"))
            setnames(x = j, old = prctls.SVR.column, new = paste0(j[ , Percentiles][1], "_", PV.root.column, "_SVR"))
            setnames(x = j, old = prctls.MVR.column, new = paste0(j[ , Percentiles][1], "_", PV.root.column, "_MVR"))
            j[ , Percentiles := NULL]
          })
        })
        
        PV.prctls <- lapply(X = PV.prctls, FUN = function(i) {
          Reduce(function(...) merge(...), i)
        })
        
        PV.prctls <- Reduce(function(...) merge(..., all = TRUE), PV.prctls)
        
        if(file.attributes[["lsa.study"]] %in% c("PISA", "PISA for Development", "ICCS", "ICILS")) {
          
          lapply(X = PVs.pct.miss, FUN = function(i) {
            pct.miss.columns <- grep(pattern = paste0("Percent_Missing_", vars.list[["PV.root.prctls"]], collapse = "|"), x = names(i), value = TRUE)
            i[ , avg.PVs.pct.miss := rowSums(.SD)/length(pct.miss.columns), .SDcols = pct.miss.columns]
          })
          
          lapply(X = PVs.pct.miss, FUN = function(i) {
            PV.root.prctls.in.scope <- grep(pattern = paste(unlist(vars.list[["PV.names"]]), sep = "", collapse = "|"), x = colnames(i), value = TRUE)
            PV.root.prctls.in.scope <- gsub(pattern = "Percent_Missing_", replacement = "", x = PV.root.prctls.in.scope)
            PV.root.prctls.in.scope <- unique(gsub(pattern = "[[:digit:]]+", replacement = "N", x = PV.root.prctls.in.scope))
            setnames(x = i, old = "avg.PVs.pct.miss", new = paste0("Percent_Missing_", PV.root.prctls.in.scope))
            i[ , grep(pattern = paste0("Percent_Missing_", paste(unlist(vars.list[["PV.names"]]), sep = "", collapse = "|"), collapse = "|"), x = names(i), value = TRUE) := NULL]
          })
        } else {
          lapply(X = PVs.pct.miss, FUN = function(i) {
            PV.root.prctls.in.scope <- intersect(unique(gsub(pattern = "Percent_Missing_|[[:digit:]]+$", replacement = "", x = colnames(i))), unlist(vars.list[["PV.root.prctls"]]))
            PV.root.prctls.in.scope <- grep(pattern = paste0("Percent_Missing_", PV.root.prctls.in.scope <- intersect(unique(gsub(pattern = "Percent_Missing_|[[:digit:]]+$", replacement = "", x = colnames(i))), unlist(vars.list[["PV.root.prctls"]])), "[[:digit:]]+"), x = names(i), value = TRUE)
            i[ , avg.PVs.pct.miss := rowSums(.SD)/length(PV.root.prctls.in.scope), .SDcols = PV.root.prctls.in.scope]
          })
          lapply(X = PVs.pct.miss, FUN = function(i) {
            PV.root.prctls.in.scope <- intersect(unique(gsub(pattern = "Percent_Missing_|[[:digit:]]+$", replacement = "", x = colnames(i))), unlist(vars.list[["PV.root.prctls"]]))
            setnames(x = i, old = "avg.PVs.pct.miss", new = paste0("Percent_Missing_", PV.root.prctls.in.scope))
            i[ , grep(pattern = paste0("Percent_Missing_", PV.root.prctls.in.scope, "[[:digit:]]+"), x = names(i), value = TRUE) := NULL]
          })
        }
        PVs.pct.miss <- Reduce(function(...) merge(..., all = TRUE), PVs.pct.miss)
      }
      
      country.analysis.info <- produce.analysis.info(cnt.ID = unique(data[ , get(key.vars)]), data = used.data, study = file.attributes[["lsa.study"]], cycle = file.attributes[["lsa.cycle"]], weight.variable = vars.list[["weight.var"]], rep.design = DESIGN, used.shortcut = shortcut, number.of.reps = rep.wgts.names, in.time = cnt.start.time)
      
      analysis.info[[country.analysis.info[ , COUNTRY]]] <<- country.analysis.info
        
      if(!is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.prctls.vars"]]) && is.null(vars.list[["PV.root.prctls"]])) {
        merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.prctls, bckg.vars.pct.miss))
      } else if(!is.null(vars.list[["split.vars"]]) && is.null(vars.list[["bckg.prctls.vars"]]) && !is.null(vars.list[["PV.root.prctls"]])){
        merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, PV.prctls, PVs.pct.miss))
      } else if(!is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.prctls.vars"]]) && !is.null(vars.list[["PV.root.prctls"]])) {
        merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.prctls, PV.prctls, bckg.vars.pct.miss, PVs.pct.miss))
      } else if(is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.prctls.vars"]]) && is.null(vars.list[["PV.root.prctls"]])) {
        merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.prctls, bckg.vars.pct.miss))
      } else if(is.null(vars.list[["split.vars"]]) && is.null(vars.list[["bckg.prctls.vars"]]) && !is.null(vars.list[["PV.root.prctls"]])) {
        merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, PV.prctls, PVs.pct.miss))
      } else if(is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.prctls.vars"]]) && !is.null(vars.list[["PV.root.prctls"]])) {
        merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.prctls, bckg.vars.pct.miss, PV.prctls, PVs.pct.miss))
      }
      
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
    
    estimates <- rbindlist(lapply(X = data, FUN = compute.all.stats))

    estimates[ , colnames(estimates)[1] := as.character(estimates[ , get(colnames(estimates)[1])])]
    setkeyv(x = estimates, cols = key.vars)

    total.exec.time <- rbindlist(analysis.info)[ , DURATION]
    total.exec.time.millisec <- sum(as.numeric(str_extract(string = total.exec.time, pattern = "[[:digit:]]{3}$")))/1000
    total.exec.time <- sum(as.ITime(total.exec.time), total.exec.time.millisec)

    if(length(unique(estimates[ , get(key.vars)])) > 1) {
      message("\nAll ", length(unique(estimates[ , get(key.vars)])), " countries with valid data processed in ", format(as.POSIXct("0001-01-01 00:00:00") + total.exec.time - 1, "%H:%M:%OS3"))
    } else {
      message("")
    }

    ptm.add.table.average <- proc.time()
    estimates <- compute.table.average(output.obj = estimates, object.variables = vars.list, data.key.variables = key.vars, data.properties = file.attributes)
    message('"Table Average" added to the estimates in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.add.table.average}[[3]], "%H:%M:%OS3"), "\n")
    
    export.results(output.object = estimates, analysis.type = action.args.list[["executed.analysis.function"]], analysis.info.obj = rbindlist(l = analysis.info), destination.file = output.file, open.exported.file = open.output)

    if(exists("removed.countries.where.any.split.var.is.all.NA") && length(removed.countries.where.any.split.var.is.all.NA) > 0) {
      warning('Some of the countries had one or more splitting variables which contains only missing values. These countries are: "', paste(removed.countries.where.any.split.var.is.all.NA, collapse = '", "'), '".', call. = FALSE)
    }

  }, interrupt = function(f) {
    message("\nInterrupted by the user. Computations are not finished and output file is not produced.\n")
  })
  
  vars.list.analysis.vars <- grep(pattern = "split.vars|bckg.prctls.vars", x = names(vars.list), value = TRUE)
  vars.list.analysis.vars <- unlist(vars.list[vars.list.analysis.vars])
  vars.list.analysis.vars <- grep(pattern = paste(unique(unlist(studies.all.design.variables)), collapse = "|"), x = vars.list.analysis.vars, value = TRUE)
  
  if(length(vars.list.analysis.vars) > 0) {
    warning('Some of the variables specified as analysis variables (in "split.vars" and/or "bckg.prctls.vars") are design variables (sampling variables or PVs). This kind of variables shall not be used for analysis. Check your input.', call. = FALSE)
  }
  
}
