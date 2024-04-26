#' @title Compute percentages of respondents in groups and/or means (arithmetic average, median or mode) on continuous variables within specified groups
#'
#' @description \code{lsa.pcts.means} computes percentages of respondents within groups defined by one or more variables and the means for one or more variables.
#'
#' @param data.file         The file containing \code{lsa.data} object. Either this or
#'                          \code{data.object} shall be specified, but not both.
#'                          See details.
#' @param data.object       The object in the memory containing \code{lsa.data} object. Either
#'                          this or \code{data.file} shall be specified, but not both.
#'                          See details.
#' @param split.vars        Categorical variable(s) to split the results by. If no split variables
#'                          are provided, the results will be for the overall countries'
#'                          populations. If one or more variables are provided, the results will
#'                          be split by all but the last variable and the percentages of
#'                          respondents will be computed by the unique values of the last splitting
#'                          variable.
#' @param bckg.avg.vars     Name(s) of continuous background or contextual variable(s) to compute
#'                          the means for. The results will be computed by all groups specified by
#'                          the splitting variables. See details.
#' @param PV.root.avg       The root name(s) for the set(s) of plausible values. See details.
#' @param central.tendency  Which measure of central tendency shall be computed - \code{mean}
#'                          (default) \code{median} or \code{mode}. See details.
#' @param weight.var        The name of the variable containing the weights. If no name of a weight
#'                          variable is provided, the function will automatically select the
#'                          default weight variable for the provided data, depending on the
#'                          respondent type.
#' @param include.missing   Logical, shall the missing values of the splitting variables be
#'                          included as categories to split by and all statistics produced for
#'                          them? The default (\code{FALSE}) takes all cases on the splitting
#'                          variables without missing values before computing any statistics.
#'                          See details.
#' @param shortcut          Logical, shall the "shortcut" method for IEA TIMSS, TIMSS Advanced,
#'                          TIMSS Numeracy, eTIMSS PSI, PIRLS, ePIRLS, PIRLS Literacy and RLII be
#'                          applied? The default (\code{FALSE}) applies the "full" design when
#'                          computing the variance components and the standard errors of the
#'                          estimates.
#' @param graphs            Logical, shall graphs be produced? Default is \code{FALSE}. See details.
#' @param perc.x.label      String, custom label for the horizontal axis in percentage graphs.
#'                          Ignored if \code{graphs = FALSE}. See details.
#' @param perc.y.label      String, custom label for the vertical axis in percentage graphs.
#'                          Ignored if \code{graphs = FALSE}. See details.
#' @param mean.x.labels     List of strings, custom labels for the horizontal axis in means' graphs.
#'                          Ignored if \code{graphs = FALSE}. See details.
#' @param mean.y.labels     List of strings, custom labels for the vertical axis in means' graphs.
#'                          Ignored if \code{graphs = FALSE}. See details.
#' @param save.output       Logical, shall the output be saved in MS Excel file (default) or not
#'                          (printed to the console or assigned to an object).
#' @param output.file       If \code{save.output = TRUE} (default), full path to the output file
#'                          including the file name. If omitted, a file with a default file name
#'                          "Analysis.xlsx" will be written to the working directory
#'                          (\code{getwd()}). Ignored if \code{save.output = FALSE}.
#' @param open.output       Logical, shall the output be open after it has been written? The default
#'                          (\code{TRUE}) opens the output in the default spreadsheet program
#'                          installed on the computer. Ignored if \code{save.output = FALSE}.
#'
#' @details
#' The function computes percentages of respondents specified by the categories of splitting variables. The percentages are computed within the groups specified by the last splitting variable. If a continuous variable(s) are provided (background or sets of plausible values), their means (as arithmetic means, medians or modes) will be computed by groups defined by one or more splitting variables. If no splitting variables are added, the results will be computed only by country.
#'
#' Either \code{data.file} or \code{data.object} shall be provided as source of data. If both of them are provided, the function will stop with an error message.
#'
#' Multiple continuous background variables can be provided to compute their means (as arithmetic means, medians or modes). Please note that in this case the results will slightly differ compared to using each of the same background continuous variables in separate analyses. This is because the cases with the missing values on \code{bckg.avg.vars} are removed in advance and the more variables are provided to \code{bckg.avg.vars}, the more cases are likely to be removed.
#'
#' Computation of means involving plausible values requires providing a root of the plausible values names in \code{PV.root.avg}. All studies (except CivED, TEDS-M, SITES, TALIS and TALIS Starting Strong Survey) have a set of PVs per construct (e.g. in TIMSS five for overall mathematics, five for algebra, five for geometry, etc.). In some studies (say TIMSS and PIRLS) the names of the PVs in a set always start with character string and end with sequential number of the PV. For example, the names of the set of PVs for overall mathematics in TIMSS are BSMMAT01, BSMMAT02, BSMMAT03, BSMMAT04 and BSMMAT05. The root of the PVs for this set to be added to \code{PV.root.avg} will be "BSMMAT". The function will automatically find all the variables in this set of PVs and include them in the analysis. In other studies like OECD PISA and IEA ICCS and ICILS the sequential number of each PV is included in the middle of the name. For example, in ICCS the names of the set of PVs are PV1CIV, PV2CIV, PV3CIV, PV4CIV and PV5CIV. The root PV name has to be specified in \code{PV.root.avg} as "PV#CIV". More than one set of PVs can be added. Note, however, that providing continuous variable(s) for the \code{bckg.avg.vars} argument and root PV for the \code{PV.root.avg} argument will affect the results for the PVs because the cases with missing on \code{bckg.avg.vars} will be removed and this will also affect the results from the PVs. On the other hand, using more than one set of PVs at the same time should not affect the results on any PV estimates because PVs shall not have any missing values.
#'
#' If no variables are specified for \code{bckg.avg.vars}, and no PV root names for \code{PV.root.avg}, the output will contain only percentages of cases in groups specified by the splitting variables, if any. If they are, their means will be computed either as arithmetic means, medians or modes. This can be controlled by setting the \code{central.tendency} argument to \code{mean} (default), \code{median} or \code{mode}. Note that if \code{central.tendency = "mode"} and the variables passed to \code{bckg.avg.vars} or the sets of PVs passed to \code{PV.root.avg} have more than one mode, the value for the lowest value will be included in the output. As a conseequence, the standard errors may be inflated.
#'
#' If \code{include.missing = FALSE} (default), all cases with missing values on the splitting variables will be removed and only cases with valid values will be retained in the statistics. Note that the data from the studies can be exported in two different ways: (1) setting all user-defined missing values to \code{NA}; and (2) importing all user-defined missing values as valid ones and adding their codes in an additional attribute to each variable. If the \code{include.missing} is set to \code{FALSE} (default) and the data used is exported using option (2), the output will remove all values from the variable matching the values in its \code{missings} attribute. Otherwise, it will include them as valid values and compute statistics for them.
#'
#' The \code{shortcut} argument is valid only for TIMSS, eTIMSS PSI, TIMSS Advanced, TIMSS Numeracy, PIRLS, ePIRLS, PIRLS Literacy and RLII. Previously, in computing the standard errors, these studies were using 75 replicates because one of the schools in the 75 JK zones had its weights doubled and the other one has been taken out. Since TIMSS 2015 and PIRLS 2016 the studies use 150 replicates and in each JK zone once a school has its weights doubled and once taken out, i.e. the computations are done twice for each zone. For more details see Foy & LaRoche (2016) and Foy & LaRoche (2017). If replication of the tables and figures is needed, the \code{shortcut} argument has to be changed to \code{TRUE}.
#'
#' If \code{graphs = TRUE}, the function will produce graphs. If only \code{split.vars} are specified, bar plots of percentages of respondents (population estimates) per group will be produced with error bars (95% confidence) for these percentages. If \code{bckg.avg.vars} and/or \code{PV.root.avg} are specified, plots with 95% confidence intervals of the averages (means, medians or modes) will be produced for each average analysis variable. All plots are produced per country. If \code{bckg.avg.vars} and/or \code{PV.root.avg} are specified, but no \code{split.vars} at the end there will be plots for each of the analysis average variables for all countries together. By default the percentage graphs horizontal axis is labeled with the name of the last splitting variable, and the vertical is labeled as "Percentages XXXXX" where XXXXX is the last splitting variable the percentages are computed for. For the means' plots the horizontal axis is labeled as the name of the last splitting variable for whose categories the means are computed by, and the vertical axis is labeled as "Mean XXXXX" where XXXXX is the name of the variable for which means are computed. These defaults can be overriden by supplying values to \code{perc.x.label}, \code{perc.y.label}, \code{mean.x.labels} and \code{mean.y.labels}. The \code{perc.x.label} and \code{perc.y.label} arguments accept vectors of length 1, and if longer vectors are supplied, error is thrown. The \code{mean.x.labels} and \code{mean.y.labels} accept lists with number of components equal to the number of variables (background or PVs) for which means are computed, longer or shorter lists throw errors. See the examples.
#'
#' row and column variable names are used for labeling the axes of the heatmaps, unless \code{graph.row.label} and/or \code{graph.col.label} arguments are supplied. These two arguments accept strings which will be used to label the axes.
#'
#' @return
#' If \code{save.output = FALSE}, a list containing the estimates and analysis information. If \code{graphs = TRUE}, the plots will be added to the list of estimates.
#'
#' If \code{save.output = TRUE} (default), an MS Excel (\code{.xlsx}) file (which can be opened in any spreadsheet program), as specified with the full path in the \code{output.file}. If the argument is missing, an Excel file with the generic file name "Analysis.xlsx" will be saved in the working directory (\code{getwd()}). The workbook contains three spreadsheets. The first one ("Estimates") contains a table with the results by country and the final part of the table contains averaged results from all countries' statistics. The following columns can be found in the table, depending on the specification of the analysis:
#'
#' \itemize{
#'   \item \verb{<}Country ID\verb{>} - a column containing the names of the countries in the file for which statistics are computed. The exact column header will depend on the country identifier used in the particular study.
#'   \item \verb{<}Split variable 1\verb{>}, \verb{<}Split variable 2\verb{>}... - columns containing the categories by which the statistics were split by. The exact names will depend on the variables in \code{split.vars}.
#'   \item n_Cases - the number of cases in the sample used to compute the statistics.
#'   \item Sum_\verb{<}Weight variable\verb{>} - the estimated population number of elements per group after applying the weights. The actual name of the weight variable will depend on the weight variable used in the analysis.
#'   \item Sum_\verb{<}Weight variable\verb{>}\verb{_}SE - the standard error of the the estimated population number of elements per group. The actual name of the weight variable will depend on the weight variable used in the analysis.
#'   \item Percentages_\verb{<}Last split variable\verb{>} - the percentages of respondents (population estimates) per groups defined by the splitting variables in \code{split.vars}. The percentages will be for the last splitting variable which defines the final groups.
#'   \item Percentages_\verb{<}Last split variable\verb{>}\verb{_}SE - the standard errors of the percentages from above.
#'   \item Mean_\verb{<}Background variable\verb{>} - returned if \code{central.tendency = "mean"}, the arithmetic average of the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.avg.vars}. There will be one column with the arithmetic average estimate for each variable specified in \code{bckg.avg.vars}.
#'   \item Mean_\verb{<}Background variable\verb{>}\verb{_}SE - returned if \code{central.tendency = "mean"}, the standard error of the arithmetic average of the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.avg.vars}. There will be one column with the SE of the average estimate for each variable specified in \code{bckg.avg.vars}.
#'   \item Variance_\verb{<}Background variable\verb{>} - returned if \code{central.tendency = "mean"}, the variance for the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.avg.vars}. There will be one column with the variance estimate for each variable specified in \code{bckg.avg.vars}.
#'   \item Variance_\verb{<}Background variable\verb{>}\verb{_}SE - returned if \code{central.tendency = "mean"}, the error of the variance for the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.avg.vars}. There will be one column with the error of the variance estimate for each variable specified in \code{bckg.avg.vars}.
#'   \item SD_\verb{<}Background variable\verb{>} - returned if \code{central.tendency = "mean"}, the standard deviation for the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.avg.vars}. There will be one column with the standard deviation estimate for each variable specified in \code{bckg.avg.vars}.
#'   \item SD_\verb{<}Background variable\verb{>}\verb{_}SE - returned if \code{central.tendency = "mean"}, the error of the standard deviation for the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.avg.vars}. There will be one column with the error of the standard deviation estimate for each variable specified in \code{bckg.avg.vars}.
#'   \item Median_\verb{<}Background variable\verb{>} - returned if \code{central.tendency = "median"}, the median of the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.avg.vars}. There will be one column with the median estimate for each variable specified in \code{bckg.avg.vars}.
#'   \item Median_\verb{<}Background variable\verb{>}\verb{_}SE - returned if \code{central.tendency = "median"}, the standard error of the median of the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.avg.vars}. There will be one column with the SE of the median estimate for each variable specified in \code{bckg.avg.vars}.
#'   \item MAD_\verb{<}Background variable\verb{>} - returned if \code{central.tendency = "median"}, the Median Absolute Deviation (MAD) for the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.avg.vars}. There will be one column with the MAD estimate for each variable specified in \code{bckg.avg.vars}.
#'   \item MAD_\verb{<}Background variable\verb{>}\verb{_}SE - returned if \code{central.tendency = "median"}, the standard error of MAD for the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.avg.vars}. There will be one column with the MAD SE estimate for each variable specified in \code{bckg.avg.vars}.
#'   \item Mode_\verb{<}Background variable\verb{>} - returned if \code{central.tendency = "mode"}, the mode of the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.avg.vars}. There will be one column with the mode estimate for each variable specified in \code{bckg.avg.vars}.
#'   \item Mode_\verb{<}Background variable\verb{>}\verb{_}SE - returned if \code{central.tendency = "mode"}, the standard error of the mode of the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.avg.vars}. There will be one column with the SE of the mode estimate for each variable specified in \code{bckg.avg.vars}.
#'   \item Percent_Missings_\verb{<}Background variable\verb{>} - the percentage of missing values for the \verb{<}Background variable\verb{>} specified in \code{bckg.avg.vars}. There will be one column with the percentage of missing values for each variable specified in \code{bckg.avg.vars}.
#'   \item Mean_\verb{<}root PV\verb{>} - returned if \code{central.tendency = "mean"}, the arithmetic average of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.avg}. There will be one column with the arithmetic average estimate for each set of PVs specified in \code{PV.root.avg}.
#'   \item Mean_\verb{<}root PV\verb{>}\verb{_}SE - returned if \code{central.tendency = "mean"}, the standard error of the arithmetic average of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.avg}. There will be one column with the standard error of arithmetic average estimate for each set of PVs specified in \code{PV.root.avg}.
#'   \item Mean_\verb{<}root PV\verb{>}\verb{_}SVR - returned if \code{central.tendency = "mean"}, the sampling variance component for the arithmetic average of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.avg}. There will be one column with the sampling variance component for the arithmetic average estimate for each set of PVs specified in \code{PV.root.avg}.
#'   \item Mean_\verb{<}root PV\verb{>}\verb{_}MVR - returned if \code{central.tendency = "mean"}, the measurement variance component for the arithmetic average of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.avg}. There will be one column with the measurement variance component for the arithmetic average estimate for each set of PVs specified in \code{PV.root.avg}.
#'   \item Variance_\verb{<}root PV\verb{>} - returned if \code{central.tendency = "mean"}, the total variance of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.avg}. There will be one column with the total variance of each set of PVs specified in \code{PV.root.avg}.
#'   \item Variance_\verb{<}root PV\verb{>}\verb{_}SE - returned if \code{central.tendency = "mean"}, the standard error of the total variance of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.avg}. There will be one column with the standard error of the total variance of each set of PVs specified in \code{PV.root.avg}.
#'   \item Variance_\verb{<}root PV\verb{>}\verb{_}SVR - returned if \code{central.tendency = "mean"}, the sampling component of the variance of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.avg}. There will be one column with the sampling component of the variance of each set of PVs specified in \code{PV.root.avg}.
#'   \item Variance_\verb{<}root PV\verb{>}\verb{_}MVR - returned if \code{central.tendency = "mean"}, the measurement component of the variance of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.avg}. There will be one column with the measurement component of the variance of each set of PVs specified in \code{PV.root.avg}.
#'   \item SD_\verb{<}root PV\verb{>} - returned if \code{central.tendency = "mean"}, the standard deviation of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.avg}. There will be one column with the standard deviation of each set of PVs specified in \code{PV.root.avg}.
#'   \item SD_\verb{<}root PV\verb{>}\verb{_}SE - returned if \code{central.tendency = "mean"}, the standard error of the standard deviation of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.avg}. There will be one column with the standard error of the standard deviation of each set of PVs specified in \code{PV.root.avg}.
#'   \item SD_\verb{<}root PV\verb{>}\verb{_}SVR - returned if \code{central.tendency = "mean"}, the sampling component of the standard deviation of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.avg}. There will be one column with the sampling component of the standard deviation of each set of PVs specified in \code{PV.root.avg}.
#'   \item SD_\verb{<}root PV\verb{>}\verb{_}MVR - returned if \code{central.tendency = "mean"}, the measurement component of the standard deviation of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.avg}. There will be one column with the measurement component of the standard deviation of each set of PVs specified in \code{PV.root.avg}.
#'   \item Median_\verb{<}root PV\verb{>} - returned if \code{central.tendency = "median"}, the median of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.avg}. There will be one column with the median estimate for each set of PVs specified in \code{PV.root.avg}.
#'   \item Median_\verb{<}root PV\verb{>}\verb{_}SE - returned if \code{central.tendency = "median"}, the standard error of the median of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.avg}. There will be one column with the standard error of median estimate for each set of PVs specified in \code{PV.root.avg}.
#'   \item MAD_\verb{<}root PV\verb{>} - returned if \code{central.tendency = "median"}, the Median Absolute Deviation (MAD) for a set of PVs specified in \code{PV.root.avg}. There will be one column with the MAD estimate for each set of PVs specified in \code{PV.root.avg}.
#'   \item MAD_\verb{<}root PV\verb{>}\verb{_}SE - returned if \code{central.tendency = "median"}, the standard error of MAD for a set of PVs specified in \code{PV.root.avg}. There will be one column with the SE estimate of the MAD for each set of PVs specified in \code{PV.root.avg}.
#'   \item Mode_\verb{<}root PV\verb{>} - returned if \code{central.tendency = "mode"}, the mode of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.avg}. There will be one column with the mode estimate for each set of PVs specified in \code{PV.root.avg}.
#'   \item Mode_\verb{<}root PV\verb{>}\verb{_}SE - returned if \code{central.tendency = "mode"}, the standard error of the mode of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.avg}. There will be one column with the standard error of mode estimate for each set of PVs specified in \code{PV.root.avg}.
#'   \item Percent_Missings_\verb{<}root PV\verb{>} - the percentage of missing values for the \verb{<}root PV\verb{>} specified in \code{PV.root.avg}. There will be one column with the percentage of missing values for each set of PVs specified in \code{PV.root.avg}.
#' }
#' The second sheet contains some additional information related to the analysis per country in the following columns:
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
#' # Compute percentages of female and male students in TIMSS 2015 grade 8 using data file, omit
#' # missing from the splitting variable (female and male as answered by the students), without
#' # shortcut, and open the output after the computations are done
#' \dontrun{
#' lsa.pcts.means(data.file = "C:/Data/TIMSS_2015_G8_Student_Miss_to_NA.RData",
#' split.vars = "BSBG01", include.missing = FALSE,
#' output.file = "C:/temp/test.xlsx", open.output = TRUE)
#' }
#'
#' # Compute the arithmetic average of the complex background scale "Students like learning
#' # mathematics" by student sex and frequency of using computer or tablet at home using TIMSS
#' # 2015 grade 8 data loaded in memory, using the shortcut, include the missing values in
#' # the splitting variables, and use the senate weights
#' \dontrun{
#' lsa.pcts.means(data.object = T15_G8_student_data, split.vars = c("BSBG01", "BSBG13A"),
#' bckg.avg.vars = "BSBGSLM", weight.var = "SENWGT", include.missing = FALSE, shortcut = TRUE,
#' output.file = "C:/temp/test.xlsx", open.output = TRUE)
#' }
#'
#' # Repeat the analysis from above, adding a second continuous variable to compute the arithmetic
#' # average for, the "Students Like Learning Science" complex scale
#' \dontrun{
#' lsa.pcts.means(data.object = T15_G8_student_data, split.vars = c("BSBG01", "BSBG13A"),
#' bckg.avg.vars = c("BSBGSLM", "BSBGSLS"), weight.var = "SENWGT", include.missing = FALSE,
#' shortcut = TRUE, output.file = "C:/temp/test.xlsx", open.output = TRUE)
#' }
#' # Same as above, but add graphs with custom labels for the percentages and means
#' \dontrun{
#' lsa.pcts.means(data.object = T15_G8_student_data, split.vars = c("BSBG01", "BSBG13A"),
#' bckg.avg.vars = c("BSBGSLM", "BSBGSLS"), weight.var = "SENWGT", include.missing = FALSE,
#' shortcut = TRUE, graphs = TRUE,
#' perc.x.label = "Using computer or tables for schoolwork at home",
#' perc.y.label = "Percentage of students",
#' mean.x.labels = list("Books at home", "Books at home"),
#' mean.y.labels = list("Average like learning math", "Average like learning science"),
#' output.file = "C:/temp/test.xlsx", open.output = TRUE)
#' }
#'
#' # Compute the arithmetic average of student overall reading achievement scores
#' # (i.e. using a set of PVs), using PIRLS 2016 student data file, split the output by student
#' # sex, use the full design, include the missing values od the splitting variable
#' # (i.e. student sex), and do not open the output after the computations are finished
#' \dontrun{
#' lsa.pcts.means(data.file = "C:/Data/PIRLS_2016_Student_Miss_to_NA.RData", split.vars = "ASBG01",
#' PV.root.avg = "ASRREA", include.missing = TRUE,
#' output.file = "C:/temp/test.xlsx", open.output = FALSE)
#' }
#'
#' # Same as above, this time compute the median instead of the arithmetic average
#' \dontrun{
#' lsa.pcts.means(data.file = "C:/Data/PIRLS_2016_Student_Miss_to_NA.RData", split.vars = "ASBG01",
#' PV.root.avg = "ASRREA", include.missing = TRUE,
#' central.tendency = "median",
#' output.file = "C:/temp/test.xlsx", open.output = FALSE)
#' }
#'
#' @references
#' LaRoche, S., Joncas, M., & Foy, P. (2016). Sample Design in TIMSS 2015. In M. O. Martin, I. V. S. Mullis, & M. Hooper (Eds.), \emph{Methods and Procedures in TIMSS 2015} (pp. 3.1-3.37). Chestnut Hill, MA: TIMSS & PIRLS International Study Center.
#' LaRoche, S., Joncas, M., & Foy, P. (2017). Sample Design in PIRLS 2016. In M. O. Martin, I. V. S. Mullis, & M. Hooper (Eds.), \emph{Methods and Procedures in PIRLS 2016} (pp. 3.1-3.34). Chestnut Hill, MA: Lynch School of Education, Boston College.
#'
#' @seealso \code{\link{lsa.convert.data}}
#' @export
lsa.pcts.means <- function(data.file, data.object, split.vars, bckg.avg.vars, PV.root.avg, central.tendency, weight.var, include.missing = FALSE, shortcut = FALSE, graphs = FALSE, perc.x.label = NULL, perc.y.label = NULL, mean.x.labels = NULL, mean.y.labels = NULL, save.output = TRUE, output.file, open.output = TRUE) {
  tmp.options <- options(scipen = 999, digits = 22)
  on.exit(expr = options(tmp.options), add = TRUE)
  warnings.collector <- list()
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
  vars.list.analysis.vars <- grep(pattern = "split.vars|bckg.avg.vars", x = names(vars.list), value = TRUE)
  vars.list.analysis.vars <- unlist(vars.list[vars.list.analysis.vars])
  vars.list.analysis.vars <- grep(pattern = paste(unique(unlist(studies.all.design.variables)), collapse = "|"), x = vars.list.analysis.vars, value = TRUE)
  if(length(vars.list.analysis.vars) > 0) {
    warnings.collector[["vars.list.analysis.vars"]] <- 'Some of the variables specified as analysis variables (in "split.vars" and/or "bckg.avg.vars") are design variables (sampling variables or PVs). This kind of variables shall not be used for analysis. Check your input.'
  }
  tryCatch({
    if(missing(central.tendency)) {
      action.args.list[["central.tendency"]] <- "mean"
    }
    if(!action.args.list[["central.tendency"]] %in% c("mean", "median", "mode")) {
      stop('\nThe "central.tendency" argument can be either "mean", "median" or "mode". Check your input.', call. = FALSE)
    }
    if(isTRUE(graphs)) {
      if(!is.null(perc.x.label) & length(perc.x.label) > 1 || !is.null(perc.y.label) & length(perc.y.label) > 1) {
        stop('\nThe "perc.x.label" and "perc.y.label" arguments accept only vectors of length 1. Check your input.', call. = FALSE)
      }
      if(!is.null(perc.x.label) & !is.vector(perc.x.label) | is.recursive(perc.x.label) || !is.null(perc.y.label) & !is.vector(perc.y.label) | is.recursive(perc.y.label)) {
        stop('\nThe "perc.x.label" and "perc.y.label" arguments accept only atomic vectors. Check your input.', call. = FALSE)
      }
      if(!is.null(mean.x.labels) & !is.list(mean.x.labels) || !is.null(mean.y.labels) & !is.list(mean.y.labels)) {
        stop('\nThe "mean.x.labels" and "mean.x.labels" arguments accept only lists. Check your input.', call. = FALSE)
      }
      if(!is.null(mean.x.labels) && length(unlist(c(mean.x.labels))) != length(unlist(c(vars.list[["bckg.avg.vars"]], vars.list[["PV.root.avg"]])))) {
        stop('\nThe number of list components in the "mean.x.labels" argument is not equal to the number of variable names supplied in "bckg.avg.vars" and/or "PV.root.avg". Check your input.', call. = FALSE)
      }
      if(!is.null(mean.y.labels) && length(unlist(c(mean.y.labels))) != length(unlist(c(vars.list[["bckg.avg.vars"]], vars.list[["PV.root.avg"]])))) {
        stop('\nThe number of list components in the "mean.y.labels" argument is not equal to the number of variable names supplied in "bckg.avg.vars" and/or "PV.root.avg". Check your input.', call. = FALSE)
      }
    }
    if(file.attributes[["lsa.study"]] %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "TiPi") & missing(shortcut)) {
      action.args.list[["shortcut"]] <- FALSE
    }
    data <- produce.analysis.data.table(data.object = data, object.variables = vars.list, action.arguments = action.args.list, imported.file.attributes = file.attributes)
    if(exists("removed.countries.where.any.split.var.is.all.NA") && length(removed.countries.where.any.split.var.is.all.NA) > 0) {
      warnings.collector[["removed.countries.where.any.split.var.is.all.NA"]] <- paste0('Some of the countries had one or more splitting variables which contains only missing values. These countries are: ', paste(removed.countries.where.any.split.var.is.all.NA, collapse = ', '), '.')
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
        bckg.avg.vars.all.NA <- names(Filter(function(i) {all(is.na(i))}, data))
        if(length(bckg.avg.vars.all.NA) > 0) {
          data1 <- copy(data)
          data1[ , (bckg.avg.vars.all.NA) := NULL]
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
        if(!is.null(vars.list[["bckg.avg.vars"]])) {
          if(action.args.list[["central.tendency"]] == "mean") {
            bckg.means <- lapply(X = vars.list[["bckg.avg.vars"]], FUN = compute.multiple.means.all.repwgt, data.object = data, weight.var = all.weights, keys = key.vars, include.missing.arg = action.args.list[["include.missing"]])
            bckg.variances <- lapply(X = vars.list[["bckg.avg.vars"]], FUN = compute.dispersion.all.repwgt, dispersion.type = "variance", data.object = data, weight.var = all.weights, keys = key.vars, include.missing.arg = action.args.list[["include.missing"]])
            bckg.SDs <- lapply(X = vars.list[["bckg.avg.vars"]], FUN = compute.dispersion.all.repwgt, dispersion.type = "SD", data.object = data, weight.var = all.weights, keys = key.vars, include.missing.arg = action.args.list[["include.missing"]])
            bckg.vars.pct.miss <- compute.cont.vars.pct.miss(vars.vector = vars.list[["bckg.avg.vars"]], data.object = data, weight.var = vars.list[["weight.var"]], keys = key.vars)
            bckg.vars.pct.miss <- na.omit(object = bckg.vars.pct.miss, cols = key.vars)
          } else if(action.args.list[["central.tendency"]] == "median") {
            bckg.medians <- na.omit(data[ , Map(f = wgt.prctl, variable = mget(vars.list[["bckg.avg.vars"]]), weight = mget(rep(all.weights, each = length(vars.list[["bckg.avg.vars"]]))), prctls.values = 0.5), by = eval(key.vars)])
            bckg.medians.new.names <- unlist(lapply(X = vars.list[["bckg.avg.vars"]], FUN = function(i) {
              paste0(i, 1:length(all.weights))
            }))
            setnames(bckg.medians, c(key.vars, bckg.medians.new.names))
            bckg.vars.pct.miss <- compute.cont.vars.pct.miss(vars.vector = vars.list[["bckg.avg.vars"]], data.object = data, weight.var = vars.list[["weight.var"]], keys = key.vars)
            bckg.vars.pct.miss <- na.omit(object = bckg.vars.pct.miss, cols = key.vars)
            bckg.MADs <- na.omit(data[ , Map(f = wgt.MAD, variable = mget(vars.list[["bckg.avg.vars"]]), weight = mget(rep(all.weights, each = length(vars.list[["bckg.avg.vars"]])))), by = eval(key.vars)])
            setnames(bckg.medians, c(key.vars, paste0("V", 1:(length(all.weights)*length(bckg.avg.vars)))))
            setnames(bckg.MADs, c(key.vars, paste0("V", 1:(length(all.weights)*length(bckg.avg.vars)))))
          } else if(action.args.list[["central.tendency"]] == "mode") {
            bckg.modes <- compute.multiple.modes.all.repwgt(data.object = data, vars.vector = vars.list[["bckg.avg.vars"]], weight.var = all.weights, keys = key.vars)
            bckg.vars.pct.miss <- compute.cont.vars.pct.miss(vars.vector = vars.list[["bckg.avg.vars"]], data.object = data, weight.var = vars.list[["weight.var"]], keys = key.vars)
            bckg.vars.pct.miss <- na.omit(object = bckg.vars.pct.miss, cols = key.vars)
          }
        }
        if(!is.null(vars.list[["PV.root.avg"]])) {
          if(action.args.list[["central.tendency"]] == "mean") {
            PV.means <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
              lapply(X = i, FUN = compute.multiple.means.all.repwgt, data.object = data, weight.var = all.weights, keys = key.vars, include.missing.arg = action.args.list[["include.missing"]])
            })
            PV.variances <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
              lapply(X = i, FUN = compute.dispersion.all.repwgt, dispersion.type = "variance", data.object = data, weight.var = all.weights, keys = key.vars, include.missing.arg = action.args.list[["include.missing"]])
            })
            PV.SDs <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
              lapply(X = i, compute.dispersion.all.repwgt, dispersion.type = "SD", data.object = data, weight.var = all.weights, keys = key.vars, include.missing.arg = action.args.list[["include.missing"]])
            })
            PVs.pct.miss <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
              compute.cont.vars.pct.miss(vars.vector = i, data.object = na.omit(object = data, cols = key.vars), weight.var = vars.list[["weight.var"]], keys = key.vars)
            })
          } else if(action.args.list[["central.tendency"]] == "median") {
            PV.medians <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
              lapply(X = i, FUN = function(j) {
                data[ , Map(f = wgt.prctl, variable = mget(j), weight = mget(all.weights), prctls.values = 0.5), by = eval(key.vars)]
              })
            })
            lapply(X = PV.medians, FUN = function(i) {
              lapply(X = i, FUN = function(j) {
                setnames(j, c(key.vars, paste0("V", 1:(length(all.weights)))))
              })
            })
            PVs.pct.miss <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
              compute.cont.vars.pct.miss(vars.vector = i, data.object = na.omit(object = data, cols = key.vars), weight.var = vars.list[["weight.var"]], keys = key.vars)
            })
            PV.MADs <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
              lapply(X = i, FUN = function(j) {
                data[ , Map(f = wgt.MAD, variable = mget(j), weight = mget(all.weights)), by = eval(key.vars)]
              })
            })
            lapply(X = PV.MADs, FUN = function(i) {
              lapply(X = i, FUN = function(j) {
                setnames(j, c(key.vars, paste0("V", 1:(length(all.weights)))))
              })
            })
          } else if(action.args.list[["central.tendency"]] == "mode") {
            PV.modes <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
              compute.multiple.modes.all.repwgt(data.object = data, vars.vector = i, weight.var = all.weights, keys = key.vars)
            })
            PVs.pct.miss <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
              compute.cont.vars.pct.miss(vars.vector = i, data.object = na.omit(object = data, cols = key.vars), weight.var = vars.list[["weight.var"]], keys = key.vars)
            })
          }
        }
      } else if (include.missing == TRUE) {
        bckg.avg.vars.all.NA <- names(Filter(function(i) {all(is.na(i))}, data))
        if(length(bckg.avg.vars.all.NA) > 0) {
          data1 <- copy(data)
          data1[ , (bckg.avg.vars.all.NA) := NULL]
          data1 <- na.omit(object = data1, cols = vars.list[["bckg.avg.vars"]][!vars.list[["bckg.avg.vars"]] %in% bckg.avg.vars.all.NA])
        } else {
          data1 <- na.omit(object = data, cols = unlist(vars.list["bckg.avg.vars"]))
        }
        if(!is.null(vars.list[["pcts.var"]])) {
          percentages <- data1[ , c(.(na.omit(unique(get(vars.list[["pcts.var"]])))), Map(f = wgt.pct, variable = .(get(vars.list[["pcts.var"]])), weight = mget(all.weights))), by = eval(vars.list[["group.vars"]])]
          number.of.cases <- data1[eval(parse(text = vars.list[["weight.var"]])) > 0, .(n_Cases = .N), by = key.vars]
          sum.of.weights <- data1[ , lapply(.SD, sum), by = key.vars, .SDcols = all.weights]
        } else {
          if(!is.null(vars.list[["bckg.avg.vars"]])) {
            percentages <- data1[ , c(.(na.omit(unique(get(key.vars)))), Map(f = wgt.pct, variable = .(get(key.vars)), weight = mget(all.weights)))]
            number.of.cases <- data1[ , .(n_Cases = .N), by = key.vars]
            sum.of.weights <- data1[ , lapply(.SD, sum), by = key.vars, .SDcols = all.weights]
          } else {
            percentages <- data[ , c(.(na.omit(unique(get(key.vars)))), Map(f = wgt.pct, variable = .(get(key.vars)), weight = mget(all.weights)))]
            number.of.cases <- data[ , .(n_Cases = .N), by = key.vars]
            sum.of.weights <- data[ , lapply(.SD, sum), by = key.vars, .SDcols = all.weights]
          }
        }
        if(!is.null(vars.list[["bckg.avg.vars"]])) {
          if(action.args.list[["central.tendency"]] == "mean") {
            bckg.means <- lapply(X = vars.list[["bckg.avg.vars"]], FUN = compute.multiple.means.all.repwgt, data.object = data, weight.var = all.weights, keys = key.vars, include.missing.arg = action.args.list[["include.missing"]])
            bckg.variances <- lapply(X = vars.list[["bckg.avg.vars"]], FUN = compute.dispersion.all.repwgt, dispersion.type = "variance", data.object = data, weight.var = all.weights, keys = key.vars, include.missing.arg = action.args.list[["include.missing"]])
            bckg.SDs <- lapply(X = vars.list[["bckg.avg.vars"]], FUN = compute.dispersion.all.repwgt, dispersion.type = "SD", data.object = data, weight.var = all.weights, keys = key.vars, include.missing.arg = action.args.list[["include.missing"]])
            bckg.vars.pct.miss <- compute.cont.vars.pct.miss(vars.vector = vars.list[["bckg.avg.vars"]], data.object = data, weight.var = vars.list[["weight.var"]], keys = key.vars)
          } else if(action.args.list[["central.tendency"]] == "median") {
            bckg.medians <- na.omit(data[ , Map(f = wgt.prctl, variable = mget(vars.list[["bckg.avg.vars"]]), weight = mget(rep(all.weights, each = length(vars.list[["bckg.avg.vars"]]))), prctls.values = 0.5), by = eval(key.vars)])
            setnames(bckg.medians, c(key.vars, paste0("V", 1:(length(all.weights)*length(bckg.avg.vars)))))
            bckg.vars.pct.miss <- compute.cont.vars.pct.miss(vars.vector = vars.list[["bckg.avg.vars"]], data.object = data, weight.var = vars.list[["weight.var"]], keys = key.vars)
            bckg.MADs <- na.omit(data[ , Map(f = wgt.MAD, variable = mget(vars.list[["bckg.avg.vars"]]), weight = mget(rep(all.weights, each = length(vars.list[["bckg.avg.vars"]])))), by = eval(key.vars)])
            setnames(bckg.MADs, c(key.vars, paste0("V", 1:(length(all.weights)*length(bckg.avg.vars)))))
          } else if(action.args.list[["central.tendency"]] == "mode") {
            bckg.modes <- compute.multiple.modes.all.repwgt(data.object = data, vars.vector = vars.list[["bckg.avg.vars"]], weight.var = all.weights, keys = key.vars)
            bckg.vars.pct.miss <- compute.cont.vars.pct.miss(vars.vector = vars.list[["bckg.avg.vars"]], data.object = data, weight.var = vars.list[["weight.var"]], keys = key.vars)
            bckg.vars.pct.miss <- na.omit(object = bckg.vars.pct.miss, cols = key.vars)
          }
        }
        if(!is.null(vars.list[["PV.root.avg"]])) {
          if(action.args.list[["central.tendency"]] == "mean") {
            PV.means <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
              lapply(X = i, FUN = compute.multiple.means.all.repwgt, data.object = data, weight.var = all.weights, keys = key.vars, include.missing.arg = action.args.list[["include.missing"]])
            })
            PV.variances <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
              lapply(X = i, FUN = compute.dispersion.all.repwgt, dispersion.type = "variance", data.object = data, weight.var = all.weights, keys = key.vars, include.missing.arg = action.args.list[["include.missing"]])
            })
            PV.SDs <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
              lapply(X = i, FUN = compute.dispersion.all.repwgt, dispersion.type = "SD", data.object = data, weight.var = all.weights, keys = key.vars, include.missing.arg = action.args.list[["include.missing"]])
            })
            PVs.pct.miss <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
              compute.cont.vars.pct.miss(vars.vector = i, data.object = data, weight.var = vars.list[["weight.var"]], keys = key.vars)
            })
          } else if(action.args.list[["central.tendency"]] == "median") {
            PV.medians <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
              lapply(X = i, FUN = function(j) {
                data[ , Map(f = wgt.prctl, variable = mget(j), weight = mget(all.weights), prctls.values = 0.5), by = eval(key.vars)]
              })
            })
            lapply(X = PV.medians, FUN = function(i) {
              lapply(X = i, FUN = function(j) {
                setnames(j, c(key.vars, paste0("V", 1:(length(all.weights)))))
              })
            })
            PVs.pct.miss <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
              compute.cont.vars.pct.miss(vars.vector = i, data.object = na.omit(object = data, cols = key.vars), weight.var = vars.list[["weight.var"]], keys = key.vars)
            })
            PV.MADs <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
              lapply(X = i, FUN = function(j) {
                data[ , Map(f = wgt.MAD, variable = mget(j), weight = mget(all.weights)), by = eval(key.vars)]
              })
            })
            lapply(X = PV.MADs, FUN = function(i) {
              lapply(X = i, FUN = function(j) {
                setnames(j, c(key.vars, paste0("V", 1:(length(all.weights)))))
              })
            })
          } else if(action.args.list[["central.tendency"]] == "mode") {
            PV.modes <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
              compute.multiple.modes.all.repwgt(data.object = data, vars.vector = i, weight.var = all.weights, keys = key.vars)
            })
            PVs.pct.miss <- lapply(X = vars.list[["PV.names"]], FUN = function(i) {
              compute.cont.vars.pct.miss(vars.vector = i, data.object = na.omit(object = data, cols = key.vars), weight.var = vars.list[["weight.var"]], keys = key.vars)
            })
          }
        }
      }
      percentages <- list(percentages)
      sum.of.weights <- list(sum.of.weights)
      if(action.args.list[["central.tendency"]] == "median" && !is.null(vars.list[["bckg.avg.vars"]])) {
        bckg.medians <- list(bckg.medians)
        bckg.MADs <- list(bckg.MADs)
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
      if(!is.null(vars.list[["bckg.avg.vars"]])) {
        if(action.args.list[["central.tendency"]] == "mean") {
          reshape.list.statistics.bckg(estimate.object = bckg.means, estimate.name = "Mean_", data.key.variables = key.vars, new.names.vector = vars.list[["bckg.avg.vars"]], bckg.vars.vector = vars.list[["bckg.avg.vars"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
          reshape.list.statistics.bckg(estimate.object = bckg.variances, estimate.name = "Variance_", data.key.variables = key.vars, new.names.vector = vars.list[["bckg.avg.vars"]], bckg.vars.vector = vars.list[["bckg.avg.vars"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
          reshape.list.statistics.bckg(estimate.object = bckg.SDs, estimate.name = "SD_", data.key.variables = key.vars, new.names.vector = vars.list[["bckg.avg.vars"]], bckg.vars.vector = vars.list[["bckg.avg.vars"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
          bckg.means <- Reduce(function(...) merge(...), bckg.means)
          bckg.variances <- Reduce(function(...) merge(...), bckg.variances)
          bckg.SDs <- Reduce(function(...) merge(...), bckg.SDs)
        } else if(action.args.list[["central.tendency"]] == "median") {
          reshape.list.statistics.bckg(estimate.object = bckg.medians, estimate.name = "Median_", data.key.variables = key.vars, new.names.vector = vars.list[["bckg.avg.vars"]], bckg.vars.vector = vars.list[["bckg.avg.vars"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
          reshape.list.statistics.bckg(estimate.object = bckg.MADs, estimate.name = "MAD_", data.key.variables = key.vars, new.names.vector = vars.list[["bckg.avg.vars"]], bckg.vars.vector = vars.list[["bckg.avg.vars"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut, multiply.columns = 1)
          bckg.medians <- Reduce(function(...) merge(...), bckg.medians)
          bckg.MADs <- Reduce(function(...) merge(...), bckg.MADs)
        } else if(action.args.list[["central.tendency"]] == "mode") {
          reshape.list.statistics.bckg(estimate.object = bckg.modes, estimate.name = "Mode_", data.key.variables = key.vars, new.names.vector = vars.list[["bckg.avg.vars"]], bckg.vars.vector = vars.list[["bckg.avg.vars"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
          bckg.modes <- Reduce(function(...) merge(...), bckg.modes)
        }
      }
      if(!is.null(vars.list[["PV.root.avg"]])) {
        if(action.args.list[["central.tendency"]] == "mean") {
          reshape.list.statistics.PV(estimate.object = PV.means, estimate.name = "Mean_", PV.vars.vector = vars.list[["PV.names"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
          reshape.list.statistics.PV(estimate.object = PV.variances, estimate.name = "Variance_", PV.vars.vector = vars.list[["PV.names"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
          reshape.list.statistics.PV(estimate.object = PV.SDs, estimate.name = "SD_", PV.vars.vector = vars.list[["PV.names"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
          lapply(X = PV.means, FUN = function(i) {
            lapply(X = i, FUN = function(j) {
              j[ , (grep(pattern = "_SumSq$", x = colnames(j), value = TRUE)) := lapply(.SD, function(k) {
                ifelse(test = is.nan(k), yes = 0, no = k)
              }), .SDcols = grep(pattern = "_SumSq", x = colnames(j), value = TRUE)]
            })
          })
          lapply(X = PV.variances, FUN = function(i) {
            lapply(X = i, FUN = function(j) {
              j[ , (grep(pattern = "_SumSq$", x = colnames(j), value = TRUE)) := lapply(.SD, function(k) {
                ifelse(test = is.nan(k), yes = 0, no = k)
              }), .SDcols = grep(pattern = "_SumSq", x = colnames(j), value = TRUE)]
            })
          })
          lapply(X = PV.SDs, FUN = function(i) {
            lapply(X = i, FUN = function(j) {
              j[ , (grep(pattern = "_SumSq$", x = colnames(j), value = TRUE)) := lapply(.SD, function(k) {
                ifelse(test = is.nan(k), yes = 0, no = k)
              }), .SDcols = grep(pattern = "_SumSq", x = colnames(j), value = TRUE)]
            })
          })
          PV.means <- lapply(X = PV.means, FUN = function(i) {
            lapply(X = i, FUN = function(j) {
              unique(x = j, by = key.vars)
            })
          })
          PV.variances <- lapply(X = PV.variances, FUN = function(i) {
            lapply(X = i, FUN = function(j) {
              unique(j, by = key.vars)
            })
          })
          PV.SDs <- lapply(X = PV.SDs, FUN = function(i) {
            lapply(X = i, FUN = function(j) {
              unique(j, by = key.vars)
            })
          })
          PV.means <- lapply(X = PV.means, FUN = function(i) {
            Reduce(function(...) merge(...), i)
          })
          PV.variances <- lapply(X = PV.variances, FUN = function(i) {
            Reduce(function(...) merge(...), i)
          })
          PV.SDs <- lapply(X = PV.SDs, FUN = function(i) {
            Reduce(function(...) merge(...), i)
          })
          aggregate.PV.estimates(estimate.object = PV.means, estimate.name = "Mean_", root.PV = vars.list[["PV.root.avg"]], PV.vars.vector = vars.list[["PV.names"]], data.key.variables = key.vars, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
          aggregate.PV.estimates(estimate.object = PV.variances, estimate.name = "Variance_", root.PV = vars.list[["PV.root.avg"]], PV.vars.vector = vars.list[["PV.names"]], data.key.variables = key.vars, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
          aggregate.PV.estimates(estimate.object = PV.SDs, estimate.name = "SD_", root.PV = vars.list[["PV.root.avg"]], PV.vars.vector = vars.list[["PV.names"]], data.key.variables = key.vars, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
          PV.means <- Reduce(function(...) merge(..., all = TRUE), PV.means)
          PV.variances <- Reduce(function(...) merge(..., all = TRUE), PV.variances)
          PV.SDs <- Reduce(function(...) merge(..., all = TRUE), PV.SDs)
          merged.PV.estimates <- Reduce(function(...) merge(..., all = TRUE), list(PV.means, PV.variances, PV.SDs))
        } else if(action.args.list[["central.tendency"]] == "median") {
          reshape.list.statistics.PV(estimate.object = PV.medians, estimate.name = "Median_", PV.vars.vector = vars.list[["PV.names"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut, multiply.columns = 1)
          reshape.list.statistics.PV(estimate.object = PV.MADs, estimate.name = "MAD_", PV.vars.vector = vars.list[["PV.names"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut, multiply.columns = 1)
          PV.medians <- lapply(X = PV.medians, FUN = function(i) {
            Reduce(function(...) merge(...), i)
          })
          PV.MADs <- lapply(X = PV.MADs, FUN = function(i) {
            Reduce(function(...) merge(...), i)
          })
          aggregate.PV.estimates(estimate.object = PV.medians, estimate.name = "Median_", root.PV = vars.list[["PV.root.avg"]], PV.vars.vector = vars.list[["PV.names"]], data.key.variables = key.vars, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
          aggregate.PV.estimates(estimate.object = PV.MADs, estimate.name = "MAD_", root.PV = vars.list[["PV.root.avg"]], PV.vars.vector = vars.list[["PV.names"]], data.key.variables = key.vars, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
          PV.medians <- Reduce(function(...) merge(..., all = TRUE), PV.medians)
          PV.MADs <- Reduce(function(...) merge(..., all = TRUE), PV.MADs)
          merged.PV.estimates <- Reduce(function(...) merge(..., all = TRUE), list(PV.medians, PV.MADs))
        } else if(action.args.list[["central.tendency"]] == "mode") {
          reshape.list.statistics.PV(estimate.object = PV.modes, estimate.name = "Mode_", PV.vars.vector = vars.list[["PV.names"]], weighting.variable = vars.list[["weight.var"]], replication.weights = rep.wgts.names, study.name = file.attributes[["lsa.study"]], SE.design = shortcut, multiply.columns = 1)
          PV.modes <- lapply(X = PV.modes, FUN = function(i) {
            Reduce(function(...) merge(...), i)
          })
          aggregate.PV.estimates(estimate.object = PV.modes, estimate.name = "Mode_", root.PV = vars.list[["PV.root.avg"]], PV.vars.vector = vars.list[["PV.names"]], data.key.variables = key.vars, study.name = file.attributes[["lsa.study"]], SE.design = shortcut)
          PV.modes <- Reduce(function(...) merge(..., all = TRUE), PV.modes)
          merged.PV.estimates <- PV.modes
        }
        if(file.attributes[["lsa.study"]] %in% c("PISA", "PISA for Development", "ICCS", "ICILS")) {
          lapply(X = PVs.pct.miss, FUN = function(i) {
            pct.miss.columns <- grep(pattern = paste0("Percent_Missing_", vars.list[["PV.root.avg"]], collapse = "|"), x = names(i), value = TRUE)
            i[ , avg.PVs.pct.miss := rowSums(.SD)/length(pct.miss.columns), .SDcols = pct.miss.columns]
          })
          lapply(X = PVs.pct.miss, FUN = function(i) {
            PV.root.avg.in.scope <- grep(pattern = paste(unlist(vars.list[["PV.names"]]), sep = "", collapse = "|"), x = colnames(i), value = TRUE)
            PV.root.avg.in.scope <- gsub(pattern = "Percent_Missing_", replacement = "", x = PV.root.avg.in.scope)
            PV.root.avg.in.scope <- unique(gsub(pattern = "[[:digit:]]+", replacement = "N", x = PV.root.avg.in.scope))
            setnames(x = i, old = "avg.PVs.pct.miss", new = paste0("Percent_Missing_", PV.root.avg.in.scope))
            i[ , grep(pattern = paste0("Percent_Missing_", paste(unlist(vars.list[["PV.names"]]), sep = "", collapse = "|"), collapse = "|"), x = names(i), value = TRUE) := NULL]
          })
        } else {
          lapply(X = PVs.pct.miss, FUN = function(i) {
            PV.root.avg.in.scope <- intersect(unique(gsub(pattern = "Percent_Missing_|[[:digit:]]+$", replacement = "", x = colnames(i))), unlist(vars.list[["PV.root.avg"]]))
            PV.root.avg.in.scope <- grep(pattern = paste0("Percent_Missing_", PV.root.avg.in.scope <- intersect(unique(gsub(pattern = "Percent_Missing_|[[:digit:]]+$", replacement = "", x = colnames(i))), unlist(vars.list[["PV.root.avg"]])), "[[:digit:]]+"), x = names(i), value = TRUE)
            i[ , avg.PVs.pct.miss := rowSums(.SD)/length(PV.root.avg.in.scope), .SDcols = PV.root.avg.in.scope]
          })
          lapply(X = PVs.pct.miss, FUN = function(i) {
            PV.root.avg.in.scope <- intersect(unique(gsub(pattern = "Percent_Missing_|[[:digit:]]+$", replacement = "", x = colnames(i))), unlist(vars.list[["PV.root.avg"]]))
            setnames(x = i, old = "avg.PVs.pct.miss", new = paste0("Percent_Missing_", PV.root.avg.in.scope))
            i[ , grep(pattern = paste0("Percent_Missing_", PV.root.avg.in.scope, "[[:digit:]]+"), x = names(i), value = TRUE) := NULL]
          })
        }
        PVs.pct.miss <- Reduce(function(...) merge(..., all = TRUE), PVs.pct.miss)
      }
      country.analysis.info <- produce.analysis.info(cnt.ID = unique(data[ , get(key.vars[[1]])]), data = used.data, study = file.attributes[["lsa.study"]], cycle = file.attributes[["lsa.cycle"]], weight.variable = vars.list[["weight.var"]], rep.design = DESIGN, used.shortcut = shortcut, number.of.reps = rep.wgts.names, in.time = cnt.start.time)
      analysis.info[[country.analysis.info[ , COUNTRY]]] <<- country.analysis.info
      if(action.args.list[["central.tendency"]] == "mean") {
        if(!is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.avg.vars"]]) && is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.means, bckg.variances, bckg.SDs, bckg.vars.pct.miss))
        } else if(!is.null(vars.list[["split.vars"]]) && is.null(vars.list[["bckg.avg.vars"]]) && !is.null(vars.list[["PV.root.avg"]])){
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, merged.PV.estimates, PVs.pct.miss))
        } else if(!is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.avg.vars"]]) && !is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.means, bckg.variances, bckg.SDs, merged.PV.estimates, bckg.vars.pct.miss, PVs.pct.miss))
        } else if(is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.avg.vars"]]) && !is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.means, bckg.variances, bckg.SDs, merged.PV.estimates, bckg.vars.pct.miss, PVs.pct.miss))
        } else if(!is.null(vars.list[["split.vars"]]) && is.null(vars.list[["bckg.avg.vars"]]) && is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages))
        } else if(is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.avg.vars"]]) && is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.means, bckg.variances, bckg.SDs, bckg.vars.pct.miss))
        } else if(is.null(vars.list[["split.vars"]]) && is.null(vars.list[["bckg.avg.vars"]]) && !is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, merged.PV.estimates, PVs.pct.miss))
        } else if(is.null(vars.list[["split.vars"]]) && is.null(vars.list[["bckg.avg.vars"]]) && is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages))
        }
      } else if(action.args.list[["central.tendency"]] == "median") {
        if(!is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.avg.vars"]]) && is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.medians, bckg.MADs, bckg.vars.pct.miss))
        } else if(!is.null(vars.list[["split.vars"]]) && is.null(vars.list[["bckg.avg.vars"]]) && !is.null(vars.list[["PV.root.avg"]])){
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, merged.PV.estimates, PVs.pct.miss))
        } else if(!is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.avg.vars"]]) && !is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.medians, bckg.MADs, merged.PV.estimates, bckg.vars.pct.miss, PVs.pct.miss))
        } else if(is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.avg.vars"]]) && !is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.medians, bckg.MADs, merged.PV.estimates, bckg.vars.pct.miss, PVs.pct.miss))
        } else if(!is.null(vars.list[["split.vars"]]) && is.null(vars.list[["bckg.avg.vars"]]) && is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages))
        } else if(is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.avg.vars"]]) && is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.medians, bckg.MADs, bckg.vars.pct.miss))
        } else if(is.null(vars.list[["split.vars"]]) && is.null(vars.list[["bckg.avg.vars"]]) && !is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, merged.PV.estimates, PVs.pct.miss))
        } else if(is.null(vars.list[["split.vars"]]) && is.null(vars.list[["bckg.avg.vars"]]) && is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages))
        }
      } else if(action.args.list[["central.tendency"]] == "mode") {
        if(!is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.avg.vars"]]) && is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.modes, bckg.vars.pct.miss))
        } else if(!is.null(vars.list[["split.vars"]]) && is.null(vars.list[["bckg.avg.vars"]]) && !is.null(vars.list[["PV.root.avg"]])){
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, merged.PV.estimates, PVs.pct.miss))
        } else if(!is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.avg.vars"]]) && !is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.modes, merged.PV.estimates, bckg.vars.pct.miss, PVs.pct.miss))
        } else if(is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.avg.vars"]]) && !is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.modes, merged.PV.estimates, bckg.vars.pct.miss, PVs.pct.miss))
        } else if(!is.null(vars.list[["split.vars"]]) && is.null(vars.list[["bckg.avg.vars"]]) && is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages))
        } else if(is.null(vars.list[["split.vars"]]) && !is.null(vars.list[["bckg.avg.vars"]]) && is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, bckg.modes, bckg.vars.pct.miss))
        } else if(is.null(vars.list[["split.vars"]]) && is.null(vars.list[["bckg.avg.vars"]]) && !is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages, merged.PV.estimates, PVs.pct.miss))
        } else if(is.null(vars.list[["split.vars"]]) && is.null(vars.list[["bckg.avg.vars"]]) && is.null(vars.list[["PV.root.avg"]])) {
          merged.outputs <- Reduce(function(...) merge(..., all = TRUE), list(number.of.cases, sum.of.weights, percentages))
        }
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
    estimates <- unique(x = estimates, fromLast = TRUE, by = key.vars)
    estimates[ , colnames(estimates)[1] := as.character(estimates[ , get(colnames(estimates)[1])])]
    setkeyv(x = estimates, cols = key.vars)
    total.exec.time <- rbindlist(analysis.info)[ , DURATION]
    total.exec.time.millisec <- sum(as.numeric(str_extract(string = total.exec.time, pattern = "[[:digit:]]{3}$")))/1000
    total.exec.time <- sum(as.ITime(total.exec.time), total.exec.time.millisec)
    if(length(unique(estimates[ , get(key.vars[[1]])])) > 1) {
      message("\nAll ", length(unique(estimates[ , get(key.vars[[1]])])), " countries with valid data processed in ", format(as.POSIXct("0001-01-01 00:00:00") + total.exec.time - 1, "%H:%M:%OS3"))
    } else {
      message("")
    }
    ptm.add.table.average <- proc.time()
    estimates <- compute.table.average(output.obj = estimates, object.variables = vars.list, data.key.variables = key.vars, data.properties = file.attributes)
    message('"Table Average" added to the estimates in ', format(as.POSIXct("0001-01-01 00:00:00") + {proc.time() - ptm.add.table.average}[[3]], "%H:%M:%OS3"))
    if(isFALSE(graphs)) {
      message("")
    }
    if(isTRUE(graphs)) {
      ptm.add.graphs <- proc.time()
      graphs.object <- copy(x = estimates[get(key.vars[1]) != "Table Average", mget(c(key.vars, grep(pattern = "^Percentages_|^Mean_|^Median_|^Mode_", x = colnames(estimates), value = TRUE)))])
      if(length(unlist(vars.list[["PV.root.avg"]])) > 0) {
        graphs.object[ , grep(pattern = "_SVR$|_MVR$|_SVR_SE$|_MVR_SE$", x = colnames(graphs.object), value = TRUE) := NULL]
      }
      graphs.object <- split(x = graphs.object, by = key.vars[1], drop = TRUE)
      if(length(key.vars) > 2) {
        lapply(X = graphs.object, FUN = function(i) {
          i[ , collapsed_split := factor(do.call(paste, c(.SD, sep = " // "))), .SDcols = key.vars[2:length(key.vars)]]
          i[ , collapsed_split := factor(x = str_wrap(string = collapsed_split, width = 50), levels = str_wrap(string = collapsed_split, width = 50))]
        })
      }
      means.NAs.only.vars.cnt <- lapply(X = graphs.object, FUN = function(i) {
        means.cols <- grep(pattern = paste(c(vars.list[["bckg.avg.vars"]], vars.list[["PV.root.avg"]]), collapse = "|"), x = colnames(i), value = TRUE)
        means.cols <- grep(pattern = "_SE$", x = means.cols, value = TRUE, invert = TRUE)
        any.NAs.cnt.means <- lapply(X = means.cols, FUN = function(j) {
          if(any(is.na(i[ , get(j)])) == TRUE) {
            unique(i[ , get(key.vars[1])])
          }
        })
      })
      means.NAs.only.vars.cnt <- unique(unlist(means.NAs.only.vars.cnt))
      if(length(means.NAs.only.vars.cnt)) {
        if(graphs == FALSE) {
          warnings.collector[["cnt.NAs.on.analysis.vars"]] <- paste0("In one or more countries computed means resulted in missing values for one or more variable, no statistics are computed. Check if the variables contained only missings: ", paste(unique(unlist(means.NAs.only.vars.cnt)), collapse = ", "), ".")
        } else {
          warnings.collector[["cnt.NAs.on.analysis.vars"]] <- paste0("In one or more countries computed means resulted in missing values for one or more variable, no statistics are computed and no graphs are produced. Check if the variables contained only missings: ", paste(unique(unlist(means.NAs.only.vars.cnt)), collapse = ", "), ".")
        }
      }
      if(is.null(perc.x.label)) {
        perc.x.label <- NULL
      }
      if(is.null(perc.y.label)) {
        perc.y.label <- NULL
      }
      perc.graphs.list <- produce.percentages.plots(data.obj = graphs.object, split.vars.vector = key.vars, type = "ordinary", perc.graph.xlab = perc.x.label, perc.graph.ylab = perc.y.label)
      if(length(c(vars.list[["bckg.avg.vars"]], vars.list[["PV.root.avg"]])) > 0) {
        if(is.null(mean.x.labels)) {
          mean.x.labels <- NULL
        }
        if(is.null(mean.y.labels)) {
          mean.y.labels <- NULL
        }
        means.graphs.list <- produce.means.plots(data.obj = graphs.object, estimates.obj = estimates, split.vars.vector = key.vars, type = "ordinary", mean.graph.xlab = mean.x.labels, mean.graph.ylab = mean.y.labels)
        if(length(key.vars) == 1) {
          graphs.object <- rbindlist(l = graphs.object)
          x.var <- sym(key.vars)
          graphs.avg.vars <- c(vars.list[["bckg.avg.vars"]], vars.list[["PV.root.avg"]])
          graphs.avg.vars <- gsub(pattern = "[[:digit:]]+", replacement = "N", x = graphs.avg.vars, fixed = TRUE)
          y.var <- lapply(X = graphs.avg.vars, FUN = function(i) {
            mean.names <- grep(pattern = i, x = colnames(graphs.object), value = TRUE)
            sym(mean.names[!mean.names %in% grep(pattern = "_SE$", x = mean.names, value = TRUE)])
          })
          int.means <- lapply(X = y.var, FUN = function(i) {
            cnt.plot <- ggplot(data = graphs.object, aes(x = !!x.var, y = !!i))
            cnt.plot <- cnt.plot + geom_errorbar(aes(ymin = !!i - 1.96 * !!sym(paste0(i, "_SE")), ymax = !!i + 1.96 * !!sym(paste0(i, "_SE"))),
                                                 width = 0.3,
                                                 linewidth = 1.3,
                                                 position = position_dodge(.9))
            cnt.plot <- cnt.plot + geom_point(size = 3)
            cnt.plot <- cnt.plot + theme(panel.background = element_rect(fill = "white"),
                                         panel.grid.major.x = element_blank(),
                                         panel.grid.major = element_line(colour = "black"),
                                         panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
                                         plot.background = element_rect(fill = "#e2e2e2"),
                                         legend.background = element_rect(fill = "#e2e2e2"),
                                         plot.title = element_text(hjust = 0.5))
            cnt.plot <- cnt.plot + scale_x_discrete(labels = function(k) {
              str_wrap(k, width = 20)
            })
            cnt.plot <- cnt.plot + scale_y_continuous(labels = function(k) {
              sprintf("%.2f", k)
            })
            cnt.plot + labs(title = "", x = mean.x.labels, y = gsub(pattern = "_", replacement = " ", x = i))
          })
          names(int.means) <- unlist(as.character(y.var))
          means.graphs.list[["International"]] <- int.means
        }
      }
    }
    if(isTRUE(save.output)) {
      save.graphs(out.path = action.args.list[["output.file"]])
      if(isFALSE(graphs)) {
        export.results(output.object = estimates, analysis.type = action.args.list[["executed.analysis.function"]], add.graphs = FALSE, analysis.info.obj = rbindlist(l = analysis.info), destination.file = output.file, open.exported.file = open.output, warns.list = unlist(warnings.collector))
      } else {
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
        if(length(c(vars.list[["bckg.avg.vars"]], vars.list[["PV.root.avg"]])) > 0) {
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
  which.cnt.multimode <- names(which(sapply(X = paste0("warnings.collector.multimodal.", rbindlist(l = analysis.info)[ , COUNTRY]), FUN = function(i) {
    exists(i)
  })))
  if(length(which.cnt.multimode)) {
    which.cnt.multimode <- rbindlist(l = analysis.info)[ , COUNTRY]
    warnings.collector[["multimodal.warning"]] <- paste0('One or more of the variables passed for analysis with "central.tendency == \'mode\'" have multiple modes. The smallest value(s) are cohosen in: ', paste(which.cnt.multimode, collapse = ", "), ".")
  }
  if(length(warnings.collector) > 0) {
    if(!is.null(warnings.collector[["removed.countries.where.any.split.var.is.all.NA"]])) {
      warning(warnings.collector[["removed.countries.where.any.split.var.is.all.NA"]], call. = FALSE)
    }
    if(!is.null(warnings.collector[["vars.list.analysis.vars"]])) {
      warning(warnings.collector[["vars.list.analysis.vars"]], call. = FALSE)
    }
    if(!is.null(warnings.collector[["data.no.JKZONE.JKREP"]])) {
      warning(warnings.collector[["data.no.JKZONE.JKREP"]], call. = FALSE)
    }
    if(!is.null(warnings.collector[["multimodal.warning"]])) {
      warning(warnings.collector[["multimodal.warning"]], call. = FALSE)
    }
    if(length(warnings.collector[["cnt.NAs.on.analysis.vars"]]) > 0) {
      warning(warnings.collector[["cnt.NAs.on.analysis.vars"]], call. = FALSE)
    }
  }
}
