#' @title Compute binary logistic regression coefficients specified groups
#'
#' @description \code{lsa.bin.log.reg} computes binary logistic regression coefficients within groups defined by one or more variables.
#'
#' @param data.file            The file containing \code{lsa.data} object. Either this or
#'                             \code{data.object} shall be specified, but not both. See details.
#' @param data.object          The object in the memory containing \code{lsa.data} object. Either
#'                             this or \code{data.file} shall be specified, but not both. See
#'                             details.
#' @param split.vars           Categorical variable(s) to split the results by. If no split
#'                             variables are provided, the results will be for the overall
#'                             countries' populations. If one or more variables are provided, the
#'                             results will be split by all but the last variable and the
#'                             percentages of respondents will be computed by the unique values of
#'                             the last splitting variable.
#' @param bin.dep.var          Name of a binary (i.e. just two distinct values) background or
#'                             contextual variable used as a dependent variable in the model. See
#'                             details.
#' @param bckg.indep.cont.vars Names of continuous independent background or contextual variables
#'                             used as predictors in the model. See details.
#' @param bckg.indep.cat.vars  Names of categorical independent background or contextual variables
#'                             used as predictors in the model to compute contrasts for (see
#'                             \code{bckg.cat.contrasts} and \code{bckg.ref.cats}). See details.
#' @param bckg.cat.contrasts   String vector with the same length as the length of
#'                             \code{bckg.indep.cat.vars} specifying the type of contrasts to
#'                             compute in case \code{bckg.indep.cat.vars} are provided. See
#'                             details.
#' @param bckg.ref.cats        Vector of integers with the same length as the length of
#'                             \code{bckg.indep.cat.vars} and \code{bckg.cat.contrasts} specifying
#'                             the reference categories for the contrasts to compute in case
#'                             \code{bckg.indep.cat.vars} are provided. See details.
#' @param PV.root.indep        The root names for a set of plausible values used as a independent
#'                             variables in the model. See details.
#' @param interactions         Interaction terms - a list containing vectors of length of two.
#'                             See details.
#' @param standardize          Shall the dependent and independent variables be standardized to
#'                             produce beta coefficients? The default is \code{FALSE}. See details.
#' @param weight.var           The name of the variable containing the weights. If no name of a
#'                             weight variable is provide, the function will automatically select
#'                             the default weight variable for the provided data, depending on the
#'                             respondent type.
#' @param norm.weight          Shall the weights be normalized before applying them, default is
#'                             \code{FALSE}. See details.
#' @param include.missing      Logical, shall the missing values of the splitting variables be
#'                             included as categories to split by and all statistics produced for
#'                             them? The default (\code{FALSE}) takes all cases on the splitting
#'                             variables without missing values before computing any statistics.
#'                             See details.
#' @param shortcut             Logical, shall the "shortcut" method for IEA TIMSS, TIMSS Advanced,
#'                             TIMSS Numeracy, eTIMSS PSI, PIRLS, ePIRLS, PIRLS Literacy and RLII
#'                             be applied? The default (\code{FALSE}) applies the "full" design
#'                             when computing the variance components and the standard errors of
#'                             the estimates.
#' @param save.output          Logical, shall the output be saved in MS Excel file (default) or
#'                             not (printed to the console or assigned to an object).
#' @param output.file          If \code{save.output = TRUE} (default), full path to the output file
#'                             including the file name. If omitted, a file with a default file name
#'                             "Analysis.xlsx" will be written to the working directory
#'                             (\code{getwd()}). Ignored if \code{save.output = FALSE}.
#' @param open.output          Logical, shall the output be open after it has been written? The
#'                             default (\code{TRUE}) opens the output in the default spreadsheet
#'                             program installed on the computer. Ignored if
#'                             \code{save.output = FALSE}.
#'
#' @details
#' Either \code{data.file} or \code{data.object} shall be provided as source of data. If both of them are provided, the function will stop with an error message.
#' The function computes binary logistic regression coefficients by the categories of the splitting variables. The percentages of respondents in each group are computed within the groups specified by the last splitting variable. If no splitting variables are added, the results will be computed only by country.
#'
#' If \code{standardize = TRUE}, the variables will be standardized before computing any statistics to provide beta regression coefficients.
#'
#' A binary (i.e. dichotomous) background/contextual variable must be provided to \code{bin.dep.var} (numeric or factor). If more than two categories exist in the variable, the function will exit with an error. The function automatically recodes the two categories of the \code{bin.dep.var} to 0 and 1 if they are not as such (e.g. as 1 and 2 as in factors). If the variable of interest has more than two distinct values (can use the \code{lsa.var.dict} to see them), they can be collapsed using the \code{lsa.recode.vars}.
#'
#' Background/contextual variables passed to \code{bckg.indep.cont.vars} will be treated as numeric variables in the model. Variables with discrete number of categories (i.e. factors) passed to \code{bckg.indep.cat.vars} will be used to compute contrasts. In this case the type of contrast have to be passed to \code{bckg.cat.contrasts} and the number of the reference categories for each of the \code{bckg.indep.cat.vars}. The number of types of contrasts and the reference categories must be the same as the number of \code{bckg.indep.cat.vars}. The currently supported contrast coding schemes are:
#'
#' \itemize{
#'   \item \code{dummy} (also called "indicator" in logistic regression) - the odds ratios show what is the probability for a positive (i.e. 1) outcome in the binary dependent variable compared to the negative outcome (i.e. 0) per category of a variable in the \code{bckg.indep.cat.cats} compared to the reference category of that dummy coded variable. The intercept shows the log of the odds for the reference category when all other levels are 0.
#'   \item \code{deviation} (also called "effect" in logistic regression) - comparing the effect of each category (except for the reference) of the deviation coded variable to the overall effect (which is the intercept).
#'   \item \code{simple} - the same as for the \code{dummy} contrast coding, except for the intercept which in this case is the overall effect.
#' }
#' Note that when using \code{standardize = TRUE}, the contrast coding of \code{bckg.indep.cat.vars} is not standardized. Thus, the regression coefficients may not be comparable to other software solutions for analyzing large-scale assessment data which rely on, for example, SPSS or SAS where the contrast coding of categorical variables (e.g. dummy coding) takes place by default. However, the model statistics will be identical.
#'
#' Multiple continuous or categorical background variables and/or sets of plausible values can be provided to compute regression coefficients for. Please note that in this case the results will slightly differ compared to using each pair of the same background continuous variables or PVs in separate analysis. This is because the cases with the missing values are removed in advance and the more variables are provided, the more cases are likely to be removed. That is, the function support only listwisie deletion.
#'
#' Computation of regression coefficients involving plausible values requires providing a root of the plausible values names in \code{PV.root.dep} and/or \code{PV.root.indep}. All studies (except CivED, TEDS-M, SITES, TALIS and TALIS Starting Strong Survey) have a set of PVs per construct (e.g. in TIMSS five for overall mathematics, five for algebra, five for geometry, etc.). In some studies (say TIMSS and PIRLS) the names of the PVs in a set always start with character string and end with sequential number of the PV. For example, the names of the set of PVs for overall mathematics in TIMSS are BSMMAT01, BSMMAT02, BSMMAT03, BSMMAT04 and BSMMAT05. The root of the PVs for this set to be added to \code{PV.root.dep} or \code{PV.root.indep} will be "BSMMAT". The function will automatically find all the variables in this set of PVs and include them in the analysis. In other studies like OECD PISA and IEA ICCS and ICILS the sequential number of each PV is included in the middle of the name. For example, in ICCS the names of the set of PVs are PV1CIV, PV2CIV, PV3CIV, PV4CIV and PV5CIV. The root PV name has to be specified in \code{PV.root.dep} or \code{PV.root.indep} as "PV#CIV". More than one set of PVs can be added in \code{PV.root.indep}.
#'
#' The function can also compute two-way interaction effects between independent variables by passing a list to the \code{interactions} argument. The list must contain vectors of length two and all variables in these vectors **must also be passed as independent variables** (see the examples). Note the following:
#' \itemize{
#' \item When an interaction is between two independent background continuous variables (i.e. both are passed to \code{bckg.indep.cont.vars}), the interaction effect will be computed between them as they are.
#' \item When the interaction is between two categorical variables (i.e. both are passed to \code{bckg.indep.cat.vars}), the interaction effect will be computed between each possible pair of categories of the two variables, except for the reference categories.
#' \item When the interaction is between one continuous (i.e. passed to \code{bckg.indep.cont.vars}) and one categorical (i.e. passed to \code{bckg.indep.cat.vars}), the interaction effect will be computed between the continuous variable and each category of the categorical variable, except for the reference category.
#' \item When the interaction is between a continuous variable (i.e. passed to \code{bckg.indep.cont.vars}) and a set of PVs (i.e. passed to \code{PV.root.indep}), the interaction effect is computed between the continuous variable and each PV in the set and the results are aggregated.
#' \item When the interaction is between a categorical variable  (i.e. passed to \code{bckg.indep.cat.vars}) and a set of PVs (i.e. passed to \code{PV.root.indep}), the interaction effect is computed between each category of the categorical variable (except the reference category) and each PV in the set. The results are aggregated for each of the categories of the categorical variables and the set of PVs.
#' \item When the interaction is between two sets of PVs (i.e. passed to \code{PV.root.indep}), the interaction effect is computed between the first PV in the first set and the first PV in the second set, the second PV in the first set and the second PV in the second set, and so on. The results are then aggregated.
#' }
#'
#' If \code{norm.weight = TRUE}, the weights will be normalized before used in the model. This may be necessary in some countries in some studies extreme weights for some of the cases may result in inflated estimates due to model perfect separation. The consequence of normalizing weights is that the number of elements in the population will sum to the number of cases in the sample. Use with caution.
#'
#' If \code{include.missing = FALSE} (default), all cases with missing values on the splitting variables will be removed and only cases with valid values will be retained in the statistics. Note that the data from the studies can be exported in two different ways: (1) setting all user-defined missing values to \code{NA}; and (2) importing all user-defined missing values as valid ones and adding their codes in an additional attribute to each variable. If the \code{include.missing} is set to \code{FALSE} (default) and the data used is exported using option (2), the output will remove all values from the variable matching the values in its \code{missings} attribute. Otherwise, it will include them as valid values and compute statistics for them.
#'
#' The \code{shortcut} argument is valid only for TIMSS, eTIMSS PSI, TIMSS Advanced, TIMSS Numeracy, PIRLS, ePIRLS, PIRLS Literacy and RLII. Previously, in computing the standard errors, these studies were using 75 replicates because one of the schools in the 75 JK zones had its weights doubled and the other one has been taken out. Since TIMSS 2015 and PIRLS 2016 the studies use 150 replicates and in each JK zone once a school has its weights doubled and once taken out, i.e. the computations are done twice for each zone. For more details see Foy & LaRoche (2016) and Foy & LaRoche (2017). If replication of the tables and figures is needed, the \code{shortcut} argument has to be changed to \code{TRUE}.
#' The function provides two-tailed \emph{t}-test and \emph{p}-values for the regression coefficients.
#'
#' @return
#' If \code{save.output = FALSE}, a list containing the estimates and analysis information. If \code{save.output = TRUE} (default), an MS Excel (\code{.xlsx}) file (which can be opened in any spreadsheet program), as specified with the full path in the \code{output.file}. If the argument is missing, an Excel file with the generic file name "Analysis.xlsx" will be saved in the working directory (\code{getwd()}). The workbook contains four spreadsheets. The first one ("Estimates") contains a table with the results by country and the final part of the table contains averaged results from all countries' statistics. The following columns can be found in the table, depending on the specification of the analysis:
#'
#' \itemize{
#'   \item \verb{<}Country ID\verb{>} - a column containing the names of the countries in the file for which statistics are computed. The exact column header will depend on the country identifier used in the particular study.
#'   \item \verb{<}Split variable 1\verb{>}, \verb{<}Split variable 2\verb{>}... - columns containing the categories by which the statistics were split by. The exact names will depend on the variables in \code{split.vars}.
#'   \item n_Cases - the number of cases in the sample used to compute the statistics.
#'   \item Sum_\verb{<}Weight variable\verb{>} - the estimated population number of elements per group after applying the weights. The actual name of the weight variable will depend on the weight variable used in the analysis.
#'   \item Sum_\verb{<}Weight variable\verb{>}\verb{_}SE - the standard error of the the estimated population number of elements per group. The actual name of the weight variable will depend on the weight variable used in the analysis.
#'   \item Percentages_\verb{<}Last split variable\verb{>} - the percentages of respondents (population estimates) per groups defined by the splitting variables in \code{split.vars}. The percentages will be for the last splitting variable which defines the final groups.
#'   \item Percentages_\verb{<}Last split variable\verb{>}\verb{_}SE - the standard errors of the percentages from above.
#'   \item Variable - the variable names (background/contextual or PV root names, or contrast coded variable names).
#'   \item Coefficients - the logistic regression coefficients (intercept and slopes).
#'   \item Coefficients_SE - the standard error of the logistic regression coefficients (intercepts and slopes) for each independent variable (background/contextual or PV root names, or contrast coded variable names) in the model.
#'   \item Coefficients_SVR - the sampling variance component for the logistic regression coefficients if root PVs are specified either as dependent or independent variables.
#'   \item Coefficients_\verb{<}root PV\verb{>}\verb{_}MVR - the measurement variance component for the logistic regression coefficients if root PVs are specified either as dependent or independent variables.
#'   \item Wald_Statistic - Wald (\emph{z}) statistic for each coefficient.
#'   \item p_value - the \emph{p}-value for the regression coefficients.
#'   \item Odds_Ratio - the odds ratios of the logistic regression.
#'   \item Odds_Ratio_SE - the standard errors for the odds ratios of the logistic regression.
#'   \item Wald_L95CI - the lower 95% model-based confidence intervals for the logistic regression coefficients.
#'   \item Wald_U95CI - the upper 95% model-based confidence intervals for the logistic regression coefficients.
#'   \item Odds_L95CI - the lower 95% model-based confidence intervals for the odds ratios.
#'   \item Odds_U95CI - the upper 95% model-based confidence intervals for the odds ratios.
#' }
#'
#' When interaction terms are included, the cells with the interactions in the \code{Variables} column will contain the names of the two variables in each of the interaction terms, divided by colon, e.g. \code{ASBGSSB:ASBGHRL}.
#'
#' The second sheet contains the model statistics:
#' \itemize{
#'   \item \verb{<}Country ID\verb{>} - a column containing the names of the countries in the file for which statistics are computed. The exact column header will depend on the country identifier used in the particular study.
#'   \item \verb{<}Split variable 1\verb{>}, \verb{<}Split variable 2\verb{>}... - columns containing the categories by which the statistics were split by. The exact names will depend on the variables in \code{split.vars}.
#'   \item Statistic - a column containing the Null Deviance (-2LL, no predictors in the model, just constant, also called "baseline"), Deviance (-2LL, after adding predictors, residual deviance, also called "new"), DF Null (degrees of freedom for the null deviance), DF Residual (degrees of freedom for the residual deviance), Akaike Information Criteria (AIC), Bayesian information criterion (BIC), model Chi-Square, different R-Squared statistics (Hosmer & Lemeshow - HS, Cox & Snell - CS, and Nagelkerke - N).
#'   \item Estimate - the numerical estimates for each of the above.
#'   \item Estimate_SE - the standard errors of the estimates from above.
#'   \item Estimate_SVR - the sampling variance component if PVs were included in the model.
#'   \item Estimate_MVR - the measurement variance component if PVs were included in the model.
#' }
#' The third sheet contains some additional information related to the analysis per country in columns:
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
#' # Compute logistic regression predicting the log of the odds the students will respond
#' # "Agree a lot" when asked if teachers are fair (dependent variable, categorical), as a function
#' # of their own sense of school belonging (independent variable, continuous) using PIRLS 2016
#' # student data. Because the dependent variable has four categories, it needs to be recoded first
#' # into a dichotomous (using the \code{lsa.recode.vars}).
#' \dontrun{
#' lsa.recode.vars(data.file = "C:/temp/test.RData", src.variables = "ASBG12D",
#' old.new = "1=2;2=2;3=1;4=1;5=3", new.variables = "ASBG12Dr",
#' new.labels = c("Disagree", "Agree", "Omitted or invalid"),
#' missings.attr = "Omitted or invalid",
#' variable.labels = "GEN/AGREE/TEACHERS ARE FAIR - RECODED",
#' out.file = "C:/temp/test.RData")
#'
#' lsa.bin.log.reg(data.file = "C:/temp/test.RData", split.vars = "ASBG01",
#' bin.dep.var = "ASBG12Dr", bckg.indep.cont.vars = "ASBGSSB")
#' }
#'
#' # Perform the same analysis from above, this time use the overall student reading achievement
#' # as a predictor.
#' \dontrun{
#' lsa.bin.log.reg(data.object = test, split.vars = "ASBG01",
#' bin.dep.var = "ASBG12Dr", PV.root.indep = "ASRREA")
#' }
#'
#' # Compute linear regression with interaction terms using PIRLS 2016 student data.
#' \dontrun{
#' lsa.bin.log.reg(data.file = "C:/temp/test.RData", bin.dep.var = "ASBG05B",
#' bckg.indep.cont.vars = "ASBGSSB", bckg.indep.cat.vars = c("ASBG01", "ASBG12B"),
#' PV.root.indep = c("ASRREA", "ASRLIT"),
#' interactions = list(c("ASBG12B", "ASBGSSB"), c("ASBG01", "ASRLIT")))
#' }
#'
#' @references
#' LaRoche, S., Joncas, M., & Foy, P. (2016). Sample Design in TIMSS 2015. In M. O. Martin, I. V. S. Mullis, & M. Hooper (Eds.), \emph{Methods and Procedures in TIMSS 2015} (pp. 3.1-3.37). Chestnut Hill, MA: TIMSS & PIRLS International Study Center.
#'
#' LaRoche, S., Joncas, M., & Foy, P. (2017). Sample Design in PIRLS 2016. In M. O. Martin, I. V. S. Mullis, & M. Hooper (Eds.), \emph{Methods and Procedures in PIRLS 2016} (pp. 3.1-3.34). Chestnut Hill, MA: Lynch School of Education, Boston College.
#'
#' UCLA: Statistical Consulting Group. 2020. "R LIBRARY CONTRAST CODING SYSTEMS FOR CATEGORICAL VARIABLES." \emph{IDRE Stats - Statistical Consulting Web Resources}. Retrieved June 16, 2020 (https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/).
#'
#' Hilbe, J. M. (2015). \emph{Practical Guide to Logistic Regression}. CRC Press.
#'
#'
#' @seealso \code{\link{lsa.convert.data}}, , \code{\link{lsa.vars.dict}}, \code{\link{lsa.recode.vars}}, \code{\link{lsa.lin.reg}}
#' @export
lsa.bin.log.reg <- function(data.file, data.object, split.vars, bin.dep.var, bckg.indep.cont.vars, bckg.indep.cat.vars, bckg.cat.contrasts, bckg.ref.cats, PV.root.indep, interactions, standardize = FALSE, weight.var, norm.weight = FALSE, include.missing = FALSE, shortcut = FALSE, save.output = TRUE, output.file, open.output = TRUE) {
  tmp.options <- options(scipen = 999, digits = 22)
  on.exit(expr = options(tmp.options), add = TRUE)
  warnings.collector <- list()
  if(missing("bckg.indep.cont.vars") & missing("bckg.indep.cat.vars") & missing("PV.root.indep")) {
    stop('No independent variables ("bckg.indep.cont.vars", "bckg.indep.cat.vars" or "PV.root.indep") were passed to the call. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(!missing(bin.dep.var) && length(bin.dep.var) > 1) {
    stop('Only one binary dependent variable can be passed at a time. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(!missing(bckg.indep.cat.vars) && !missing(bckg.ref.cats) && length(bckg.indep.cat.vars) != length(bckg.ref.cats)) {
    stop('"bckg.indep.cat.vars" and "bckg.ref.cats" must have equal length. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(!missing(bckg.indep.cat.vars) && !missing(bckg.cat.contrasts) && length(bckg.indep.cat.vars) != length(bckg.cat.contrasts)) {
    stop('"bckg.indep.cat.vars" and "bckg.cat.contrasts" must have equal length. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(!missing(bckg.ref.cats) && !is.numeric(bckg.ref.cats)) {
    stop('The reference category passed to "bckg.ref.cats" must be a numeric value. All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(!missing(interactions)) {
    indeps.passed <- as.list(sys.call())
    indeps.passed <- indeps.passed[c("bckg.indep.cont.vars", "bckg.indep.cat.vars", "PV.root.indep")]
    indeps.passed <- lapply(X = indeps.passed, FUN = eval)
    indeps.passed <- unname(sort(unlist(indeps.passed[!is.na(names(indeps.passed))])))
    if(!all(sort(unlist(unique(interactions))) %in% indeps.passed)) {
      stop('The variables passed to "interactions" must be present in "bckg.indep.cont.vars", "bckg.indep.cat.vars" and "PV.root.indep". Check your input.\n\n', call. = FALSE)
    }
    if(!is.list(interactions)) {
      stop('The "interactions" argument is not a list. Check your input.\n\n', call. = FALSE)
    }
    if(!all(sapply(X = interactions, length) == 2)) {
      stop('Two-way interactions are supported only. The individual vectors of variable names in the "interactions" argument must be pairs of variable names. Check your input.\n\n', call. = FALSE)
    }
  }
  if(!missing(bckg.indep.cat.vars) & missing(bckg.cat.contrasts)) {
    bckg.cat.contrasts <- rep(x = "dummy", times = length(bckg.indep.cat.vars))
    warnings.collector[["contrast.cat.set.default"]] <- 'Independent categorical background variable(s) were passed to "bckg.indep.cat.vars", but no contrast coding schemes were provided for the "bckg.cat.contrasts" argument. "dummy" coding was set as default for all variables passed to "bckg.indep.cat.vars".'
  }
  if(!missing(bckg.indep.cat.vars) && any(!bckg.cat.contrasts %in% c("dummy", "simple", "deviation"))) {
    stop('An unsupported contrast coding scheme was passed to the "bckg.indep.cat.vars". All operations stop here. Check your input.\n\n', call. = FALSE)
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
  if(!missing(bckg.indep.cat.vars) & missing(bckg.ref.cats)) {
    bckg.ref.cats <- sapply(X = data[ , mget(vars.list[["bckg.indep.cat.vars"]])], FUN = function(i) {
      min(na.omit(as.numeric(i)))
    })
    warnings.collector[["ref.cat.set.default"]] <- 'Independent categorical background variable(s) were passed to "bckg.indep.cat.vars", but no reference categories were provided for the "bckg.ref.cats" argument. Default reference categories were set: the minimum value(s) available in the data for categorical independent variable(s).'
  }
  action.args.list <- get.action.arguments()
  file.attributes <- get.file.attributes(imported.object = data)
  vars.list.analysis.vars <- grep(pattern = "split.vars|bckg.dep.var|bckg.indep.cont.vars|bckg.indep.cat.vars", x = names(vars.list), value = TRUE)
  vars.list.analysis.vars <- unlist(vars.list[vars.list.analysis.vars])
  vars.list.analysis.vars <- grep(pattern = paste(unique(unlist(studies.all.design.variables)), collapse = "|"), x = vars.list.analysis.vars, value = TRUE)
  if(length(vars.list.analysis.vars) > 0) {
    warnings.collector[["vars.list.analysis.vars"]] <- 'Some of the variables specified as analysis variables (in "split.vars" and/or background variables - dependent or independent) are design variables (sampling variables or PVs). This kind of variables shall not be used for analysis. Check your input.'
  }
  tryCatch({
    if(file.attributes[["lsa.study"]] %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "TiPi") & missing(shortcut)) {
      action.args.list[["shortcut"]] <- FALSE
    }
    data <- produce.analysis.data.table(data.object = data, object.variables = vars.list[names(vars.list) != "interactions"], action.arguments = action.args.list, imported.file.attributes = file.attributes)
    if(exists("removed.countries.where.any.split.var.is.all.NA") && length(removed.countries.where.any.split.var.is.all.NA) > 0) {
      warnings.collector[["removed.countries.where.any.split.var.is.all.NA"]] <- paste0('Some of the countries had one or more splitting variables which contains only missing values. These countries are: ', paste(removed.countries.where.any.split.var.is.all.NA, collapse = ', '), '.')
    }
    countries.with.all.NA.vars <- sapply(X = data, FUN = function(i) {
      any(sapply(X = i[ , mget(unname(unlist(vars.list[c("bin.dep.var", "bckg.indep.cont.vars", "bckg.indep.cat.vars", "PV.names")])))], FUN = function(j) {
        all(is.na(j))
      }) == TRUE)
    })
    countries.with.all.NA.vars <- names(Filter(isTRUE, countries.with.all.NA.vars))
    if(length(countries.with.all.NA.vars) > 0) {
      warnings.collector[["countries.with.all.NA.vars"]] <- paste0('One or more countries in the data have one or more variables in the regression model which have only missing values and have been removed: ', paste(countries.with.all.NA.vars, collapse = ", "), ".")
      if(length(countries.with.all.NA.vars) == length(names(data))) {
        stop('One or more variables in the model has missing values in all countries. All operations stop here. Check the data for all variables.\n\n', call. = FALSE)
      } else {
        data[countries.with.all.NA.vars] <- NULL
      }
    }
    if(!missing(bckg.indep.cat.vars)) {
      countries.with.constant.cat.vars <- names(Filter(isTRUE, lapply(X = data, FUN = function(i) {
        any(Filter(isTRUE, lapply(X = i[ , mget(unname(unlist(vars.list["bckg.indep.cat.vars"])))], FUN = function(j) {
          length(unique(j)) < 2
        })) == TRUE)
      })))
      if(length(countries.with.constant.cat.vars) > 0) {
        warnings.collector[["countries.with.constant.cat.vars"]] <- paste0('One or more countries in the data have one or more variables in "bckg.indep.cat.vars" which are constant and have been removed: ', paste(countries.with.all.NA.vars, collapse = ", "), ".")
        data[countries.with.constant.cat.vars] <- NULL
      }
    }
    max.two.cats <- sapply(X = data, FUN = function(i) {
      length(unique(na.omit(i[ , get(bin.dep.var)])))
    })
    if(na.omit(unique(max.two.cats)) != 2) {
      stop('The variable passed to "bin.dep.var" is not binary. All operations stop here. Check your input.\n\n', call. = FALSE)
    }
    lapply(X = data, FUN = function(i) {
      i[get(bin.dep.var) == min(get(bin.dep.var), na.rm = TRUE), (bin.dep.var) := 0]
      i[get(bin.dep.var) == max(get(bin.dep.var), na.rm = TRUE), (bin.dep.var) := 1]
    })
    if(!is.null(vars.list[["split.vars"]])) {
      data <- lapply(X = data, FUN = function(i) {
        rows.to.remove <- lapply(X = vars.list[["bckg.indep.cat.vars"]], FUN = function(j) {
          tmp <- dcast(i, formula(paste0(vars.list[["split.vars"]][length(vars.list[["split.vars"]])], " ~ ", j)), value.var = j, fun.aggregate = length)
          tmp1 <- tmp[ , mget(colnames(tmp)[2:length(colnames(tmp))])]
          tmp[ , JUSTONEVALID := apply(tmp1, 1, function(j) {
            if(sum(j > 0) == 1) {
              FALSE
            } else {
              TRUE
            }
          })]
          tmp[JUSTONEVALID == FALSE, get(vars.list[["split.vars"]][length(vars.list[["split.vars"]])])]
        })
        i[!get(vars.list[["split.vars"]][length(vars.list[["split.vars"]])]) %in% unlist(rows.to.remove), ]
      })
    }
    data <- lapply(X = data, FUN = function(i) {
      i <- na.omit(object = i, cols = unlist(vars.list[c("bin.dep.var", "bckg.indep.cont.vars", "bckg.indep.cat.vars", "bckg.cat.contrasts", "bckg.ref.cats")]))
      i[get(vars.list[["weight.var"]]) > 0, ]
    })
    if(standardize == TRUE) {
      data <- lapply(X = data, FUN = function(i) {
        all.model.vars <- unlist(x = Filter(Negate(is.null), vars.list[c("bckg.indep.cont.vars", "PV.names")]), use.names = FALSE)
        i[ , (all.model.vars) := lapply(.SD, scale), .SDcols = all.model.vars]
      })
    }
    if(!is.null(vars.list[["bckg.indep.cat.vars"]])) {
      bckg.cat.vars.new.names <- unlist(Map(f = function(input1, input2) {
        if(input2 == "dummy") {
          paste0(input1, "_DY")
        } else if(input2 == "deviation") {
          paste0(input1, "_DN")
        } else if(input2 == "simple") {
          paste0(input1, "_SC")
        }
      }, input1 = as.list(vars.list[["bckg.indep.cat.vars"]]), input2 = as.list(bckg.cat.contrasts)))
      contrast.columns <- copy(lapply(X = data, FUN = function(i) {
        i[ , mget(vars.list[["bckg.indep.cat.vars"]])]
      }))
      contrast.columns <- lapply(X = contrast.columns, FUN = function(i) {
        i[ , (bckg.cat.vars.new.names) := lapply(.SD, factor), .SDcols = vars.list[["bckg.indep.cat.vars"]]]
        tmp.contr.cols <- Map(f = function(input1, input2, input3) {
          if(input2 == "dummy") {
            contrasts(input1) <- contr.treatment(n = length(levels(input1)), base = input3)
          } else if(input2 == "deviation") {
            input1 <- factor(x = input1, levels = c(levels(input1)[!levels(input1) == input3], input3))
            deviation.contrasts <- contr.sum(n = length(levels(input1)))
            dimnames(deviation.contrasts) <- list(levels(input1), grep(pattern = input3, x = levels(input1), value = TRUE, invert = TRUE))
            contrasts(input1) <- deviation.contrasts
          } else if(input2 == "simple") {
            input1 <- factor(x = input1, levels = c(levels(input1)[levels(input1) == input3], levels(input1)[!levels(input1) == input3]))
            contr.treatment.matrix <- contr.treatment(n = length(levels(input1)))
            effect.contrasts.matrix <- matrix(rep(x = 1/4, times = length(levels(input1))*(length(levels(input1)) - 1)), ncol = (length(levels(input1)) - 1))
            contr.treatment.matrix <- contr.treatment.matrix - effect.contrasts.matrix
            dimnames(contr.treatment.matrix) <- list(levels(input1), grep(pattern = input3, x = levels(input1), value = TRUE, invert = TRUE))
            contrasts(input1) <- contr.treatment.matrix
          }
          return(data.table(input1))
        }, input1 = i[ , mget(bckg.cat.vars.new.names)], input2 = as.list(bckg.cat.contrasts), input3 = as.list(bckg.ref.cats))
        tmp.contr.cols <- do.call(cbind, tmp.contr.cols)
        setnames(x = tmp.contr.cols, bckg.cat.vars.new.names)
      })
      data <- Map(f = cbind, data, contrast.columns)
    }
    vars.list[["pcts.var"]] <- tmp.pcts.var
    vars.list[["group.vars"]] <- tmp.group.vars
    analysis.info <- list()
    model.stats <- list()
    number.of.countries <- length(names(data))
    if(number.of.countries == 1) {
      message("\nValid data from one country have been found. Some computations can be rather intensive. Please be patient.\n")
    } else if(number.of.countries > 1) {
      message("\nValid data from ", number.of.countries, " countries have been found. Some computations can be rather intensive. Please be patient.\n")
    }
    counter <- 0
    compute.all.stats <- function(data) {
      independent.variables <- grep(pattern = ".indep", x = names(vars.list), value = TRUE)
      if("PV.root.indep" %in% independent.variables) {
        independent.variables.PV <- lapply(X = vars.list[["PV.root.indep"]], FUN = function(i) {
          as.list(grep(pattern = i, x = unlist(vars.list[["PV.names"]]), value = TRUE))
        })
      }
      if(any(c("bckg.indep.cont.vars", "bckg.indep.cat.vars") %in% independent.variables)) {
        if(exists("bckg.cat.vars.new.names")) {
          independent.variables.bckg <- paste(unlist(c(vars.list[["bckg.indep.cont.vars"]], bckg.cat.vars.new.names)), collapse = " + ")
        } else {
          independent.variables.bckg <- paste(unlist(vars.list[["bckg.indep.cont.vars"]]), collapse = " + ")
        }
      }
      if(exists("independent.variables.PV") & exists("independent.variables.bckg")) {
        independent.variables <- do.call(cbind, independent.variables.PV)
        independent.variables <- cbind(independent.variables, independent.variables.bckg)
        independent.variables <- as.list(apply(X = independent.variables, MARGIN = 1, FUN = function(i) {
          paste(i, collapse = " + ")
        }))
      } else if(exists("independent.variables.PV") & !exists("independent.variables.bckg")) {
        independent.variables <- lapply(X = vars.list[["PV.root.indep"]], FUN = function(i) {
          as.list(grep(pattern = i, x = unlist(vars.list[["PV.names"]]), value = TRUE))
        })
        independent.variables <- do.call(cbind, independent.variables)
        independent.variables <- as.list(apply(X = independent.variables, MARGIN = 1, FUN = function(i) {
          paste(i, collapse = " + ")
        }))
      } else if(!exists("independent.variables.PV") & exists("independent.variables.bckg")) {
        if(exists("bckg.cat.vars.new.names")) {
          independent.variables <- paste(unlist(Filter(Negate(is.null), c(vars.list["bckg.indep.cont.vars"], bckg.cat.vars.new.names))), collapse = " + ")
        } else {
          independent.variables <- paste(unlist(Filter(Negate(is.null), vars.list["bckg.indep.cont.vars"])), collapse = " + ")
        }
      }
      if(is.character(independent.variables)) {
        regression.formula <- paste(c(bin.dep.var, independent.variables), collapse = " ~ ")
      } else if(is.list(independent.variables)) {
        regression.formula <- Map(f = paste, bin.dep.var, independent.variables, sep = " ~ ")
      }
      if("interactions" %in% names(vars.list)) {
        interaction.terms <- lapply(X = vars.list[["interactions"]], FUN = function(i) {
          if(exists("bckg.cat.vars.new.names")) {
            cat.interaction.vars <- grep(pattern = paste(i, collapse = "|"), x = bckg.cat.vars.new.names, value = TRUE)
          }
          if(!is.null(vars.list[["bckg.indep.cont.vars"]])) {
            cont.interaction.vars <- grep(pattern = paste(i, collapse = "|"), x = bckg.indep.cont.vars, value = TRUE)
          }
          if(exists("independent.variables.PV")) {
            PV.interaction.vars <- lapply(X = independent.variables.PV, FUN = function(j) {
              if(file.attributes[["lsa.study"]] %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "TiPi")) {
                grep(pattern = paste(i, collapse = "|"), x = unlist(j), value = TRUE)
              } else if(file.attributes[["lsa.study"]] %in% c("PISA", "PISA for Development", "ICCS", "ICILS")) {
                grep(pattern = paste(gsub(pattern = "#", replacement = "[[:digit:]]+", x = i), collapse = "|"), x = unlist(j), value = TRUE)
              }
            })
          }
          which.interact.terms.exist <- names(which(sapply(X = c("cat.interaction.vars", "PV.interaction.vars", "cont.interaction.vars"), FUN = function(i) {exists(i)}) == TRUE))
          Filter(length, mget(which.interact.terms.exist))
        })
        interaction.terms <- lapply(interaction.terms, function(i) {
          lapply(i, function(j) {
            Filter(length, j)
          })
        })
        interaction.terms <- lapply(interaction.terms, function(i) {
          Filter(length, i)
        })
        interaction.terms <- lapply(X = interaction.terms, FUN = function(i) {
          if(length(i) == 1 && names(i) == "cat.interaction.vars") {
            paste(unlist(i), collapse = ":")
          } else if(length(i) == 1 && names(i) == "cont.interaction.vars") {
            paste(unlist(i), collapse = ":")
          } else if(length(i) == 2 && identical(names(i), c("cat.interaction.vars", "cont.interaction.vars"))) {
            paste(c(i[[1]], i[[2]]), collapse = ":")
          } else if(length(i) == 2 && identical(names(i), c("cont.interaction.vars", "cat.interaction.vars"))) {
            paste(c(i[[1]], i[[2]]), collapse = ":")
          } else if(length(i) == 2 && identical(names(i), c("cat.interaction.vars", "PV.interaction.vars"))) {
            lapply(X = unlist(i[["PV.interaction.vars"]]), FUN = function(j) {
              paste(c(i[["cat.interaction.vars"]], j), collapse = ":")
            })
          } else if(length(i) == 2 && identical(names(i), c("PV.interaction.vars", "cat.interaction.vars"))) {
            lapply(X = unlist(i[["PV.interaction.vars"]]), FUN = function(j) {
              paste(c(i[["cat.interaction.vars"]], j), collapse = ":")
            })
          } else if(length(i) == 2 && identical(names(i), c("cont.interaction.vars", "PV.interaction.vars"))) {
            lapply(X = unlist(i[["PV.interaction.vars"]]), FUN = function(j) {
              paste(c(i[["cont.interaction.vars"]], j), collapse = ":")
            })
          } else if(length(i) == 2 && identical(names(i), c("PV.interaction.vars", "cont.interaction.vars"))) {
            lapply(X = unlist(i[["PV.interaction.vars"]]), FUN = function(j) {
              paste(c(i[["cont.interaction.vars"]], j), collapse = ":")
            })
          } else if(length(i) == 1 && identical(names(i), "PV.interaction.vars")) {
            unlist(lapply(X = i, FUN = function(j) {
              as.list(apply(X = data.table(j[[1]], j[[2]]), MARGIN = 1, FUN = function(k) {
                paste(k, collapse = ":")
              }))
            }), recursive = FALSE)
          }
        })
        interaction.terms <- lapply(X = interaction.terms, FUN = function(i) {
          data.table(i)
        })
        interaction.terms <- Reduce(cbind, interaction.terms)
        interaction.terms <- apply(X = interaction.terms, MARGIN = 1, FUN = function(i) {
          paste(i, collapse = " + ")
        }, simplify = FALSE)
        if(is.list(regression.formula)) {
          regression.formula <- as.data.table(cbind(regression.formula, interaction.terms))
          regression.formula <- apply(X = regression.formula, MARGIN = 1, FUN = function(i) {
            paste(i, collapse = " + ")
          }, simplify = FALSE)
        } else {
          regression.formula <- as.data.table(cbind(as.list(regression.formula), interaction.terms))
          regression.formula <- apply(X = regression.formula, MARGIN = 1, FUN = function(i) {
            paste(i, collapse = " + ")
          })
        }
      }
      rep.wgts.names <- paste(c("REPWGT", unlist(lapply(X = design.weight.variables[grep("rep.wgts", names(design.weight.variables), value = TRUE)], FUN = function(i) {
        unique(gsub(pattern = "[[:digit:]]*$", replacement = "", x = i))
      }))), collapse = "|^")
      rep.wgts.names <- grep(pattern = rep.wgts.names, x = names(data), value = TRUE)
      all.weights <- c(vars.list[["weight.var"]], rep.wgts.names)
      if(norm.weight == TRUE) {
        data[ , (all.weights) := lapply(.SD, function(i) {
          length(i) * i / sum(i)
        }), .SDcols = all.weights]
      }
      cnt.start.time <- format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS3")
      if(include.missing == FALSE) {
        data1 <- na.omit(object = copy(data), cols = key.vars)
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
        data1 <- copy(data)
        if(!is.null(vars.list[["pcts.var"]])) {
          percentages <- data1[ , c(.(na.omit(unique(get(vars.list[["pcts.var"]])))), Map(f = wgt.pct, variable = .(get(vars.list[["pcts.var"]])), weight = mget(all.weights))), by = eval(vars.list[["group.vars"]])]
          number.of.cases <- data1[eval(parse(text = vars.list[["weight.var"]])) > 0, .(n_Cases = .N), by = key.vars]
          sum.of.weights <- data1[ , lapply(.SD, sum), by = key.vars, .SDcols = all.weights]
        } else {
          percentages <- data[ , c(.(na.omit(unique(get(key.vars)))), Map(f = wgt.pct, variable = .(get(key.vars)), weight = mget(all.weights)))]
          number.of.cases <- data[ , .(n_Cases = .N), by = key.vars]
          sum.of.weights <- data[ , lapply(.SD, sum), by = key.vars, .SDcols = all.weights]
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
      if(!is.null(vars.list[["PV.root.indep"]])) {
        PV.names.to.split.by <- transpose(vars.list[["PV.names"]])
        PV.names.to.keep <- lapply(X = PV.names.to.split.by, FUN = function(i) {
          grep(pattern = paste(c(key.vars, i, vars.list[["bin.dep.var"]], vars.list[["bckg.indep.cont.vars"]], vars.list[["bckg.indep.cat.vars"]], all.weights, vars.list[["jk.zones"]], vars.list[["rep.ind"]]), collapse = "|"), x = colnames(data1), value = TRUE)
        })
        data1 <- lapply(X = PV.names.to.keep, FUN = function(i) {
          data1[ , mget(i)]
        })
      }
      if(is.null(vars.list[["PV.root.indep"]])) {
        if(exists("bckg.cat.vars.new.names")) {
          bckg.regression <- list(compute.logistic.regression.all.repwgt(data.object = data1, vars.vector = c(vars.list[["bin.dep.var"]], vars.list[["bckg.indep.cont.vars"]], bckg.cat.vars.new.names), weight.var = all.weights, keys = key.vars, reg.formula = regression.formula))
        } else {
          bckg.regression <- list(compute.logistic.regression.all.repwgt(data.object = data1, vars.vector = c(vars.list[["bin.dep.var"]], vars.list[["bckg.indep.cont.vars"]]), weight.var = all.weights, keys = key.vars, reg.formula = regression.formula))
        }
        lapply(X = bckg.regression, FUN = function(i) {
          setnames(x = i, old = "V1", new = "Variable")
        })
      } else if(!is.null(vars.list[["PV.root.indep"]])) {
        PV.regression <- list(lapply(X = seq_along(data1), FUN = function(i) {
          compute.logistic.regression.all.repwgt(data.object = data1[[i]], vars.vector = grep(pattern = paste(c(vars.list[["PV.root.indep"]], vars.list[["bin.dep.var"]], vars.list[["bckg.indep.cont.vars"]], vars.list[["bckg.indep.cat.vars"]]), collapse = "|"), x = colnames(data1[[i]]), value = TRUE), weight.var = all.weights, keys = key.vars, reg.formula = regression.formula[[i]])
        }))
        PV.regression["odds.ratios"] <- lapply(X = PV.regression, FUN = function(i) {
          lapply(X = i, function(j) {
            j <- j[V1 %in% grep(pattern = "_odds$", x = V1, value = TRUE)]
            j[ , V1 := gsub(pattern = "_odds$", replacement = "", x = V1)]
          })
        })
        PV.regression[1] <- lapply(X = PV.regression[1], FUN = function(i) {
          lapply(X = i, function(j) {
            j[!V1 %in% grep(pattern = "_odds$", x = V1, value = TRUE), ]
          })
        })
        PV.regression <- lapply(X = PV.regression, FUN = function(i) {
          lapply(X = i, FUN = function(j) {
            j[ , V1 := as.character(V1)]
            PV.values.names <- grep(pattern = paste(vars.list[["PV.root.indep"]], collapse = "|"), x = j[ , V1], value = TRUE)
            new.V1.values <- unname(lapply(X = j[ , V1], FUN = function(k) {
              if(grepl(pattern = "\\:", x = k) == TRUE) {
                k <- unlist(strsplit(x = k, split = ":", fixed = TRUE))
                k <- sapply(X = k, FUN = function(l) {
                  if(l %in% unlist(vars.list[["PV.names"]]) && grepl(pattern = "[[:digit:]]+$", x = l) == TRUE) {
                    gsub(pattern = "[[:digit:]]+", replacement = "", x = l)
                  } else if(l %in% unlist(vars.list[["PV.names"]]) && grepl(pattern = "[[:alpha:]]+[[:digit:]]+[[:alpha:]]+", x = l) == TRUE) {
                    gsub(pattern = "[[:digit:]]+", replacement = "N", x = l)
                  } else if(!l %in% unlist(vars.list[["PV.names"]])) {
                    l
                  }
                })
              } else {
                ifelse(test = k %in% PV.values.names, yes = gsub(pattern = "[[:digit:]]+$", replacement = "", x = k), no = k)
              }
            }))
            new.V1.values <- lapply(X = new.V1.values, FUN = function(k) {
              paste(k, collapse = ":")
            })
            j[ , V1 := new.V1.values]
            if(exists("bckg.cat.vars.new.names")) {
              new.cat.indep.vars.vals <- unique(grep(pattern = paste(bckg.cat.vars.new.names, collapse = "|"), x = j[ , V1], value = TRUE))
              if(file.attributes[["lsa.study"]] %in% c("PISA", "PISA for Development", "ICCS", "ICILS")) {
                PV.root.indep.names <- unique(gsub(pattern = "[[:digit:]]+", replacement = "N", x = grep(pattern = paste(vars.list[["PV.root.indep"]], collapse = "|"), x = j[ , V1], value = TRUE)))
                j[ , V1 := sapply(.SD, FUN = function(k) {
                  ifelse(test = grepl(pattern = paste(vars.list[["PV.root.indep"]], collapse = "|"), x = k), yes = gsub(pattern = "[[:digit:]]+", replacement = "N", x = k), no = k)
                }), .SDcols = "V1"]
                j[ , V1 := factor(x = V1, levels = c("(Intercept)", PV.root.indep.names, vars.list[["bckg.indep.cont.vars"]], new.cat.indep.vars.vals, grep(pattern = "\\:", x = j[ , V1], value = TRUE), "null.deviance", "deviance", "df.null", "df.residual", "aic", "bic", "chi.square", "r2hl", "r2cs", "r2n"), labels = c("(Intercept)", PV.root.indep.names, vars.list[["bckg.indep.cont.vars"]], new.cat.indep.vars.vals, grep(pattern = "\\:", x = j[ , V1], value = TRUE), "null.deviance", "deviance", "df.null", "df.residual", "aic", "bic", "chi.square", "r2hl", "r2cs", "r2n"))]
              } else {
                j[ , V1 := factor(x = V1, levels = c("(Intercept)", vars.list[["PV.root.indep"]], vars.list[["bckg.indep.cont.vars"]], new.cat.indep.vars.vals, grep(pattern = "\\:", x = j[ , V1], value = TRUE), "null.deviance", "deviance", "df.null", "df.residual", "aic", "bic", "chi.square", "r2hl", "r2cs", "r2n"), labels = c("(Intercept)", vars.list[["PV.root.indep"]], vars.list[["bckg.indep.cont.vars"]], new.cat.indep.vars.vals, grep(pattern = "\\:", x = j[ , V1], value = TRUE), "null.deviance", "deviance", "df.null", "df.residual", "aic", "bic", "chi.square", "r2hl", "r2cs", "r2n"))]
              }
            } else {
              if(file.attributes[["lsa.study"]] %in% c("PISA", "PISA for Development", "ICCS", "ICILS")) {
                PV.root.indep.names <- unique(gsub(pattern = "[[:digit:]]+", replacement = "N", x = grep(pattern = paste(vars.list[["PV.root.indep"]], collapse = "|"), x = j[ , V1], value = TRUE)))
                j[ , V1 := sapply(.SD, FUN = function(k) {
                  ifelse(test = grepl(pattern = paste(vars.list[["PV.root.indep"]], collapse = "|"), x = k), yes = gsub(pattern = "[[:digit:]]+", replacement = "N", x = k), no = k)
                }), .SDcols = "V1"]
                j[ , V1 := factor(x = V1, levels = c("(Intercept)", PV.root.indep.names, vars.list[["bckg.indep.cont.vars"]], grep(pattern = "\\:", x = j[ , V1], value = TRUE), "null.deviance", "deviance", "df.null", "df.residual", "aic", "bic", "chi.square", "r2hl", "r2cs", "r2n"), labels = c("(Intercept)", PV.root.indep.names, vars.list[["bckg.indep.cont.vars"]], grep(pattern = "\\:", x = j[ , V1], value = TRUE), "null.deviance", "deviance", "df.null", "df.residual", "aic", "bic", "chi.square", "r2hl", "r2cs", "r2n"))]
              } else {
                j[ , V1 := factor(x = V1, levels = c("(Intercept)", vars.list[["PV.root.indep"]], vars.list[["bckg.indep.cont.vars"]], grep(pattern = "\\:", x = j[ , V1], value = TRUE), "null.deviance", "deviance", "df.null", "df.residual", "aic", "bic", "chi.square", "r2hl", "r2cs", "r2n"), labels = c("(Intercept)", vars.list[["PV.root.indep"]], vars.list[["bckg.indep.cont.vars"]], grep(pattern = "\\:", x = j[ , V1], value = TRUE), "null.deviance", "deviance", "df.null", "df.residual", "aic", "bic", "chi.square", "r2hl", "r2cs", "r2n"))]
              }
            }
            setkeyv(x = j, cols = c(key.vars, "V1"))
          })
        })
        PV.regression <- lapply(X = PV.regression, FUN = function(i) {
          lapply(X = i, FUN = function(j) {
            setnames(x = j, old = c("V1", all.weights), new = c("Variable", paste0("V", 1:length(all.weights))))
          })
        })
      }
      if(is.null(vars.list[["PV.root.indep"]])) {
        reshape.list.statistics.bckg(estimate.object = bckg.regression, estimate.name = "Coefficients", data.key.variables = key.vars, new.names.vector = "", bckg.vars.vector = vars.list[["bckg.indep.vars"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
        bckg.regression <- bckg.regression[[1]]
        country.model.stats <- bckg.regression[Variable %in% c("null.deviance", "deviance", "df.null", "df.residual", "aic", "bic", "chi.square", "r2hl", "r2cs", "r2n"), ]
        setnames(x = country.model.stats, old = c("Variable", "Coefficients", "Coefficients_SE"), new = c("Statistic", "Estimate", "Estimate_SE"))
        bckg.regression <- bckg.regression[!Variable %in% c("null.deviance", "deviance", "df.null", "df.residual", "aic", "bic", "chi.square", "r2hl", "r2cs", "r2n"), ]
      } else if(!is.null(vars.list[["PV.root.indep"]])) {
        reshape.list.statistics.PV(estimate.object = PV.regression, estimate.name = "Coefficients", PV.vars.vector = "", weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
        lapply(X = PV.regression[["odds.ratios"]], FUN = function(i) {
          i[ , Variable := paste0(Variable, "_odds")]
        })
        PV.regression <- lapply(X = PV.regression, FUN = function(i) {
          rbindlist(l = i, idcol = "DDD")
        })
        PV.regression <- rbindlist(l = PV.regression)
        PV.regression <- split(x = PV.regression, by = "DDD")
        PV.regression <- list(lapply(X = PV.regression, FUN = function(i) {
          i[ , DDD := NULL]
        }))
        reset.coefficients.colnames <- function(input1, input2) {
          setnames(x = input1, old = grep(pattern = "^Coefficients$", x = colnames(input1), value = TRUE), new = paste0("Coefficients_", input2))
          setnames(x = input1, old = grep(pattern = "^Coefficients_SumSq$", x = colnames(input1), value = TRUE), new = paste0("Coefficients_", input2, "_SumSq"))
        }
        PV.regression <- lapply(X = PV.regression, FUN = function(i) {
          list(Map(f = reset.coefficients.colnames, input1 = i, input2 = as.list(paste(vars.list[["bin.dep.var"]], 1:length(vars.list[["PV.names"]][[1]]), sep = "0"))))[[1]]
        })
        PV.regression <- lapply(X = PV.regression, FUN = function(i) {
          Reduce(function(...) merge(...), i)
        })
        aggregate.PV.estimates(estimate.object = PV.regression, estimate.name = "Coefficients_", root.PV = vars.list[["bin.dep.var"]], PV.vars.vector = paste(vars.list[["bin.dep.var"]], 1:length(vars.list[["PV.names"]][[1]]), sep = "0"), data.key.variables = c(key.vars, "Variable"), study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
        if(file.attributes[["lsa.study"]] %in% c("PISA", "PISA for Development", "ICCS", "ICILS")) {
          lapply(X = PV.regression, FUN = function(i) {
            coefficient.cols <- grep(pattern = "^Coefficients_[[:graph:]]+$", x = colnames(i), value = TRUE)
            if(length(coefficient.cols) > 0) {
              main.coeff.col <- coefficient.cols[!coefficient.cols %in% grep(pattern = "_SE$|_SVR$|_MVR$", x = coefficient.cols, value = TRUE)]
              setnames(x = i, old = main.coeff.col, new = paste0("Coefficients_", vars.list[["bin.dep.var"]]))
              setnames(x = i, old = grep(pattern = "^Coefficients_[[:graph:]]+_SE$", x = colnames(i), value = TRUE), new = paste0("Coefficients_", vars.list[["bin.dep.var"]], "_SE"))
              setnames(x = i, old = grep(pattern = "^Coefficients_[[:graph:]]+_SVR$", x = colnames(i), value = TRUE), new = paste0("Coefficients_", vars.list[["bin.dep.var"]], "_SVR"))
              setnames(x = i, old = grep(pattern = "^Coefficients_[[:graph:]]+_MVR$", x = colnames(i), value = TRUE), new = paste0("Coefficients_", vars.list[["bin.dep.var"]], "_MVR"))
            } else {
              i
            }
          })
        }
        PV.regression <- PV.regression[[1]]
        coeff.colnames <- grep(pattern = "^Coefficients_", x = colnames(PV.regression), value = TRUE)
        country.model.stats <- PV.regression[Variable %in% c("null.deviance", "deviance", "df.null", "df.residual", "aic", "bic", "chi.square", "r2hl", "r2cs", "r2n"), ]
        colnames(country.model.stats) <- gsub(pattern = paste(paste0("_", unlist(vars.list)), collapse = "|"), replacement = "", x = colnames(country.model.stats))
        setnames(x = country.model.stats, old = c("Variable", "Coefficients", grep(pattern = "Coefficients_", x = colnames(country.model.stats), value = TRUE)), new = c("Statistic", "Estimate", gsub(pattern = "Coefficients_", replacement = "Estimate_", x = grep(pattern = "Coefficients_", x = colnames(country.model.stats), value = TRUE))))
        PV.regression <- PV.regression[!Variable %in% c("null.deviance", "deviance", "df.null", "df.residual", "aic", "bic", "chi.square", "r2hl", "r2cs", "r2n"), ]
        merged.PV.estimates <- PV.regression
        PV.regression <- NULL
      }
      country.model.stats[ , Statistic := factor(x = Statistic, levels = c("null.deviance", "deviance", "df.null", "df.residual", "aic", "bic", "chi.square", "r2hl", "r2cs", "r2n"), labels = c("Null Deviance (-2LL)", "Deviance (-2LL)", "DF Null", "DF Residual", "AIC", "BIC", "Chi-Square", "R-Squared (Hosmer & Lemeshow)", "R-Squared (Cox & Snell)", "R-Squared (Nagelkerke)"))]
      setkeyv(x = country.model.stats, cols = c(key.vars, "Statistic"))
      cnt.model.name <- unique(country.model.stats[ , get(key.vars[1])])
      model.stats[[cnt.model.name]] <<- country.model.stats
      country.analysis.info <- produce.analysis.info(cnt.ID = unique(data[ , get(key.vars[1])]), data = used.data, study = file.attributes[["lsa.study"]], cycle = file.attributes[["lsa.cycle"]], weight.variable = vars.list[["weight.var"]], rep.design = DESIGN, used.shortcut = shortcut, number.of.reps = rep.wgts.names, in.time = cnt.start.time)
      analysis.info[[country.analysis.info[ , COUNTRY]]] <<- country.analysis.info
      if("PV.root.indep" %in% names(vars.list) == FALSE) {
        merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.regression))
      } else if("PV.root.indep" %in% names(vars.list) == TRUE) {
        merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, merged.PV.estimates))
        colnames(merged.outputs) <- gsub(pattern = paste(paste0("Coefficients_", unlist(vars.list[["bin.dep.var"]])), collapse = "|"), replacement = "Coefficients", x = colnames(merged.outputs))
      }
      merged.outputs[ , Wald_Statistic := Coefficients/Coefficients_SE]
      merged.outputs[ , Wald_Statistic := lapply(.SD, function(i) {
        ifelse(test = is.infinite(i), yes = NA, no = i)
      }), .SDcols = "Wald_Statistic"]
      merged.outputs[ , p_value := 2 * pnorm(q = abs(Wald_Statistic), lower.tail = FALSE)]
      merged.outputs[ , (c("Wald_Statistic", "p_value")) := lapply(.SD, function(i) {
        ifelse(test = is.na(i), yes = NaN, no = i)
      }), .SDcols = c("Wald_Statistic", "p_value")]
      odds.ratios.estimates <- merged.outputs[Variable %in% grep(pattern = "_odds$", x = Variable, value = TRUE), mget(c(key.vars, "Variable", "Coefficients", "Coefficients_SE"))]
      odds.ratios.estimates[ , Variable := droplevels(Variable)]
      setnames(x = odds.ratios.estimates, old = c("Coefficients", "Coefficients_SE"), new = c("Odds_Ratio", "Odds_Ratio_SE"))
      odds.ratios.estimates[ , Variable := gsub(pattern = "_odds$", replacement = "", x = Variable)]
      if(is.factor(odds.ratios.estimates[ , Variable]) == FALSE) {
        odds.ratios.estimates[ , Variable := factor(x = Variable, levels = unique(Variable), labels = unique(Variable))]
      }
      setkeyv(x = odds.ratios.estimates, cols = c(key.vars, "Variable"))
      merged.outputs <- merged.outputs[!Variable %in% grep(pattern = "_odds$", x = Variable, value = TRUE), ]
      merged.outputs[ , Variable := droplevels(Variable)]
      setkeyv(x = merged.outputs, cols = c(key.vars, "Variable"))
      merged.outputs <- merge(x = merged.outputs, y = odds.ratios.estimates)
      merged.outputs[ , Wald_L95CI := Coefficients - qnorm(0.975) * Coefficients_SE]
      merged.outputs[ , Wald_U95CI := Coefficients + qnorm(0.975) * Coefficients_SE]
      merged.outputs[ , Odds_L95CI := exp(Wald_L95CI)]
      merged.outputs[ , Odds_U95CI := exp(Wald_U95CI)]
      odds.ratios.estimates <- NULL
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
    if(length(unique(estimates[ , get(key.vars[1])])) > 1) {
      message("\nAll ", length(unique(estimates[ , get(key.vars[1])])), " countries with valid data processed in ", format(as.POSIXct("0001-01-01 00:00:00") + total.exec.time, "%H:%M:%OS3"))
    } else {
      message("")
    }
    ptm.add.table.average <- proc.time()
    estimates <- compute.table.average(output.obj = estimates, object.variables = vars.list, data.key.variables = c(key.vars, "Variable"), data.properties = file.attributes)
    estimates[eval(parse(text = colnames(estimates)[1])) == "Table Average", Wald_Statistic := Coefficients/Coefficients_SE]
    estimates[eval(parse(text = colnames(estimates)[1])) == "Table Average", p_value := 2 * pnorm(q = abs(Wald_Statistic), lower.tail = FALSE)]
    if(standardize == TRUE) {
      if(!is.null(vars.list[["PV.names"]])) {
        estimates[Variable == "(Intercept)", (c("Coefficients", "Coefficients_SE", "Coefficients_SVR", "Coefficients_MVR", "Wald_Statistic", "p_value")) := NaN]
      } else {
        estimates[Variable == "(Intercept)", (c("Coefficients", "Coefficients_SE", "Wald_Statistic", "p_value")) := NaN]
      }
    }
    message('"Table Average" added to the estimates in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.add.table.average}[[3]], "%H:%M:%OS3"))
    ptm.add.model.stats <- proc.time()
    model.stats <- rbindlist(l = model.stats)
    setkeyv(x = model.stats, cols = c(key.vars, "Statistic"))
    model.stats <- compute.table.average(output.obj = model.stats, object.variables = vars.list, data.key.variables = c(key.vars, "Statistic"), data.properties = file.attributes)
    model.stats[eval(parse(text = colnames(model.stats)[1])) == "Table Average" & Statistic %in% c("Null Deviance (-2LL)", "Deviance (-2LL)", "DF Null", "DF Residual"), Estimate := NaN]
    model.stats[eval(parse(text = colnames(model.stats)[1])) == "Table Average" & Statistic %in% c("Null Deviance (-2LL)", "Deviance (-2LL)", "DF Null", "DF Residual"), Estimate_SE := NaN]
    message('\nModel statistics table assembled in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.add.model.stats}[[3]], "%H:%M:%OS3"), "\n")
    if(isTRUE(save.output)) {
      export.results(output.object = estimates, analysis.type = action.args.list[["executed.analysis.function"]], model.stats.obj = model.stats, analysis.info.obj = rbindlist(l = analysis.info), destination.file = output.file, open.exported.file = open.output, warns.list = unlist(warnings.collector))
    } else if(isFALSE(save.output)) {
      if(length(warnings.collector) == 0) {
        return(list(Estimates = estimates, `Model statistics` = model.stats, `Analysis information` = rbindlist(l = analysis.info)))
      } else{
        return(list(Estimates = estimates, `Model statistics` = model.stats, `Analysis information` = rbindlist(l = analysis.info), Warnings = unlist(unname(warnings.collector))))
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
    if(!is.null(warnings.collector[["ref.cat.set.default"]])) {
      warning(warnings.collector[["ref.cat.set.default"]], call. = FALSE)
    }
    if(!is.null(warnings.collector[["contrast.cat.set.default"]])) {
      warning(warnings.collector[["contrast.cat.set.default"]], call. = FALSE)
    }
    if(!is.null(warnings.collector[["countries.with.all.NA.vars"]])) {
      warning(warnings.collector[["countries.with.all.NA.vars"]], call. = FALSE)
    }
    if(!is.null(warnings.collector[["countries.with.constant.cat.vars"]])) {
      warning(warnings.collector[["countries.with.constant.cat.vars"]], call. = FALSE)
    }
  }
}
