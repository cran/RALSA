% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lsa.prctls.r
\name{lsa.prctls}
\alias{lsa.prctls}
\title{Compute percentiles of continuous variables within groups}
\usage{
lsa.prctls(
  data.file,
  data.object,
  split.vars,
  bckg.prctls.vars,
  PV.root.prctls,
  prctls = c(5, 25, 50, 75, 95),
  weight.var,
  include.missing = FALSE,
  shortcut = FALSE,
  graphs = FALSE,
  perc.x.label = NULL,
  perc.y.label = NULL,
  prctl.x.labels = NULL,
  prctl.y.labels = NULL,
  save.output = TRUE,
  output.file,
  open.output = TRUE
)
}
\arguments{
\item{data.file}{The file containing \code{lsa.data} object. Either this or \code{data.object}
shall be specified, but not both. See details.}

\item{data.object}{The object in the memory containing \code{lsa.data} object. Either this or
\code{data.file} shall be specified, but not both. See details.}

\item{split.vars}{Categorical variable(s) to split the results by. If no split variables
are provided, the results will be for the overall countries'
populations. If one or more variables are provided, the results will
be split by all but the last variable and the percentages of respondents
will be computed by the unique values of the last splitting variable.}

\item{bckg.prctls.vars}{Name(s) of continuous background or contextual variable(s) to compute
the percentiles for. The results will be computed by all groups specified
by the splitting variables. See details.}

\item{PV.root.prctls}{The root name(s) for the set(s) of plausible values. See details.}

\item{prctls}{Vector of integers specifying the percentiles to be computed, the default
is \code{c(5, 25, 50, 75, 95)}. See examples.}

\item{weight.var}{The name of the variable containing the weights. If no name of a weight
variable is provide, the function will automatically select the default
weight variable for the provided data, depending on the respondent type.}

\item{include.missing}{Logical, shall the missing values of the splitting variables be included
as categories to split by and all statistics produced for them? The
default (\code{FALSE}) takes all cases on the splitting variables
without missing values before computing any statistics. See details.}

\item{shortcut}{Logical, shall the "shortcut" method for IEA TIMSS, TIMSS Advanced,
TIMSS Numeracy, eTIMSS PSI, PIRLS, ePIRLS, PIRLS Literacy and RLII be
applied when using PVs? The default (\code{FALSE}) applies the "full"
design when computing the variance components and the standard errors of
the PV estimates.}

\item{graphs}{Logical, shall graphs be produced? Default is \code{FALSE}. See details.}

\item{perc.x.label}{String, custom label for the horizontal axis in percentage graphs.
Ignored if \code{graphs = FALSE}. See details.}

\item{perc.y.label}{String, custom label for the vertical axis in percentage graphs.
Ignored if \code{graphs = FALSE}. See details.}

\item{prctl.x.labels}{List of strings, custom labels for the horizontal axis in percentiles'
graphs. Ignored if \code{graphs = FALSE}. See details.}

\item{prctl.y.labels}{List of strings, custom labels for the vertical axis in percentiles'
graphs.  Ignored if \code{graphs = FALSE}. See details.}

\item{save.output}{Logical, shall the output be saved in MS Excel file (default) or not
(printed to the console or assigned to an object).}

\item{output.file}{If \code{save.output = TRUE} (default), full path to the output file
including the file name. If omitted, a file with a default file name
"Analysis.xlsx" will be written to the working directory
(\code{getwd()}). Ignored if \code{save.output = FALSE}.}

\item{open.output}{Logical, shall the output be open after it has been written? The default
(\code{TRUE}) opens the output in the default spreadsheet program
installed on the computer. Ignored if \code{save.output = FALSE}.}
}
\value{
If \code{save.output = FALSE}, a list containing the estimates and analysis information. If \code{graphs = TRUE}, the plots will be added to the list of estimates.

If \code{save.output = TRUE} (default), an MS Excel (\code{.xlsx}) file (which can be opened in any spreadsheet program), as specified with the full path in the \code{output.file}. If the argument is missing, an Excel file with the generic file name "Analysis.xlsx" will be saved in the working directory (\code{getwd()}). The workbook contains three spreadsheets. The first one ("Estimates") contains a table with the results by country and the final part of the table contains averaged results from all countries' statistics. The following columns can be found in the table, depending on the specification of the analysis:

\itemize{
\item \verb{<}Country ID\verb{>} - a column containing the names of the countries in the file for which statistics are computed. The exact column header will depend on the country identifier used in the particular study.
\item \verb{<}Split variable 1\verb{>}, \verb{<}Split variable 2\verb{>}... - columns containing the categories by which the statistics were split by. The exact names will depend on the variables in \code{split.vars}.
\item n_Cases - the number of cases in the sample used to compute the statistics.
\item Sum_\verb{<}Weight variable\verb{>} - the estimated population number of elements per group after applying the weights. The actual name of the weight variable will depend on the weight variable used in the analysis.
\item Sum_\verb{<}Weight variable\verb{>}\verb{_}SE - the standard error of the the estimated population number of elements per group. The actual name of the weight variable will depend on the weight variable used in the analysis.
\item Percentages_\verb{<}Last split variable\verb{>} - the percentages of respondents (population estimates) per groups defined by the splitting variables in \code{split.vars}. The percentages will be for the last splitting variable which defines the final groups.
\item Percentages_\verb{<}Last split variable\verb{>}\verb{_}SE - the standard errors of the percentages from above.
\item Prctl_\verb{<}Percentile value\verb{>}\verb{_}\verb{<}Background variable\verb{>} - the percentile of the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.prctls.vars}. There will be one column for each percentile estimate for each variable specified in \code{bckg.prctls.vars}.
\item Prctl_\verb{<}Percentile value\verb{>}\verb{_}\verb{<}Background variable\verb{>}\verb{_}SE - the standard error of the percentile of the continuous \verb{<}Background variable\verb{>} specified in \code{bckg.prctls.vars}. There will be one column with the SE per percentile estimate for each variable specified in \code{bckg.prctls.vars}.
\item Percent_Missings_\verb{<}Background variable\verb{>} - the percentage of missing values for the \verb{<}Background variable\verb{>} specified in \code{bckg.prctls.vars}. There will be one column with the percentage of missing values for each variable specified in \code{bckg.prctls.vars}.
\item Prctl_\verb{<}Percentile value\verb{>}\verb{_}\verb{<}root PV\verb{>} - the percentile of the PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.prctls}. There will be one column per percentile value estimate for each set of PVs specified in \code{PV.root.prctls}.
\item Prctl_\verb{<}Percentile value\verb{>}\verb{_}\verb{<}root PV\verb{>}\verb{_}SE - the standard error per percentile per set of PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.prctls}. There will be one column with the standard error of estimate per percentile per set of PVs specified in \code{PV.root.prctls}.
\item Prctl_\verb{<}Percentile value\verb{>}\verb{_}\verb{<}root PV\verb{>}\verb{_}SVR - the sampling variance component per percentile per set of PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.prctls}. There will be one column with the sampling variance component percentile estimate for each set of PVs specified in \code{PV.root.prctls}.
\item Prctl_\verb{<}Percentile value\verb{>}\verb{_}\verb{<}root PV\verb{>}\verb{_}MVR - the measurement variance component per percentiles per set of PVs with the same \verb{<}root PV\verb{>} specified in \code{PV.root.prctls}. There will be one column with the measurement variance component per percentile per set of PVs specified in \code{PV.root.prctls}.
\item Percent_Missings_\verb{<}root PV\verb{>} - the percentage of missing values for the \verb{<}root PV\verb{>} specified in \code{PV.root.prctls}. There will be one column with the percentage of missing values for each set of PVs specified in \code{PV.root.prctls}.
}
The second sheet contains some additional information related to the analysis per country in columns:
\itemize{
\item DATA - used \code{data.file} or \code{data.object}.
\item STUDY - which study the data comes from.
\item CYCLE - which cycle of the study the data comes from.
\item WEIGHT - which weight variable was used.
\item DESIGN - which resampling technique was used (JRR or BRR).
\item SHORTCUT - logical, whether the shortcut method was used.
\item NREPS - how many replication weights were used.
\item ANALYSIS_DATE - on which date the analysis was performed.
\item START_TIME - at what time the analysis started.
\item END_TIME - at what time the analysis finished.
\item DURATION - how long the analysis took in hours, minutes, seconds and milliseconds.
}

The third sheet contains the call to the function with values for all parameters as it was executed. This is useful if the analysis needs to be replicated later.

If \code{graphs = TRUE} there will be an additional "Graphs" sheet containing all plots.

If any warnings resulting from the computations are issued, these will be included in an additional "Warnings" sheet in the workbook as well.
}
\description{
\code{lsa.prctls} computes percentiles of continuous variables within groups defined by one or more variables.
}
\details{
Either \code{data.file} or \code{data.object} shall be provided as source of data. If both of them are provided, the function will stop with an error message.

The function computes percentiles of variables (background/contextual or sets of plausible values) by groups defined by one or more categorical variables (splitting variables). Multiple splitting variables can be added, the function will compute the percentages for all formed groups and their percentiles on the continuous variables. If no splitting variables are added, the results will be computed only by country.

Multiple continuous background variables can be provided to compute the specified percentiles for them. Please note that in this case the results will slightly differ compared to using each of the same background continuous variables in separate analyses. This is because the cases with the missing values on \code{bckg.prctls.vars} are removed in advance and the more variables are provided to \code{bckg.prctls.vars}, the more cases are likely to be removed.

Computation of percentiles involving plausible values requires providing a root of the plausible values names in \code{PV.root.prctls}. All studies (except CivED, TEDS-M, SITES, TALIS and TALIS Starting Strong Survey) have a set of PVs per construct (e.g. in TIMSS five for overall mathematics, five for algebra, five for geometry, etc.). In some studies (say TIMSS and PIRLS) the names of the PVs in a set always start with character string and end with sequential number of the PV. For example, the names of the set of PVs for overall mathematics in TIMSS are BSMMAT01, BSMMAT02, BSMMAT03, BSMMAT04 and BSMMAT05. The root of the PVs for this set to be added to \code{PV.root.prctls} will be "BSMMAT". The function will automatically find all the variables in this set of PVs and include them in the analysis. In other studies like OECD PISA and IEA ICCS and ICILS the sequential number of each PV is included in the middle of the name. For example, in ICCS the names of the set of PVs are PV1CIV, PV2CIV, PV3CIV, PV4CIV and PV5CIV. The root PV name has to be specified in \code{PV.root.prctls} as "PV#CIV". More than one set of PVs can be added. Note, however, that providing continuous variable(s) for the \code{bckg.prctls.vars} argument and root PV for the \code{PV.root.prctls} argument will affect the results for the PVs because the cases with missing on \code{bckg.prctls.vars} will be removed and this will also affect the results from the PVs. On the other hand, using more than one set of PVs at the same time should not affect the results on any PV estimates because PVs shall not have any missing values.

If \code{include.missing = FALSE} (default), all cases with missing values on the splitting variables will be removed and only cases with valid values will be retained in the statistics. Note that the data from the studies can be exported in two different ways using the \code{lsa.convert.data}: (1) setting all user-defined missing values to \code{NA}; and (2) importing all user-defined missing values as valid ones and adding their codes in an additional attribute to each variable. If the \code{include.missing} in \code{lsa.prctls} is set to \code{FALSE} (default) and the data used is exported using option (2), the output will remove all values from the variable matching the values in its \code{missings} attribute. Otherwise, it will include them as valid values and compute statistics for them.

The \code{shortcut} argument is valid only for TIMSS, eTIMSS PSI, TIMSS Advanced, TIMSS Numeracy, PIRLS, ePIRLS, PIRLS Literacy and RLII. Previously, in computing the standard errors, these studies were using 75 replicates because one of the schools in the 75 JK zones had its weights doubled and the other one has been taken out. Since TIMSS 2015 and PIRLS 2016 the studies use 150 replicates and in each JK zone once a school has its weights doubled and once taken out, i.e. the computations are done twice for each zone. For more details see Foy & LaRoche (2016) and Foy & LaRoche (2017). If replication of the tables and figures is needed, the \code{shortcut} argument has to be changed to \code{TRUE}.

If \code{graphs = TRUE}, the function will produce graphs. Bar plots of percentages of respondents (population estimates) per group will be produced with error bars (95\% confidence) for these percentages. Line plots for the percentiles per group defined by the \code{split.vars} will be created with 95\% confidence intervals for the percentile values. All plots are produced per country. If no \code{split.vars} are specified, at the end there will be percentile plots for each of the variables specified in \code{bckg.prctls.vars} and/or \code{PV.root.prctls} for all countries together. By default the percentage graphs horizontal axis is labeled with the name of the last splitting variable, and the vertical is labeled as "Percentages XXXXX" where XXXXX is the last splitting variable the percentages are computed for. For the percentiles' plots the horizontal axis is labeled as "Percentiles", and the vertical axis is labeled as the name of the variable for which percentiles are computed. These defaults can be overriden by supplying values to \code{perc.x.label}, \code{perc.y.label}, \code{prctl.x.labels} and \code{prctl.y.labels}. The \code{perc.x.label} and \code{perc.y.label} arguments accept vectors of length 1, and if longer vectors are supplied, error is thrown. The \code{prctl.x.labels} and \code{prctl.y.labels} accept lists with number of components equal to the number of variables (background or PVs) for which percentiles are computed, longer or shorter lists throw errors. See the examples.
}
\examples{
# Compute the 5th, 25th and 50th percentiles of the complex background scale "Students like
# learning mathematics" by student sex and frequency of using computer or tablet at home using
# TIMSS 2015 grade 8 data loaded in memory, without shortcut, exclude the cases with missing
# values in the splitting variables, and use the default (TOTWGT) weights
\dontrun{
lsa.pcts.prctls(data.object = T15_G8_student_data, split.vars = c("BSBG01", "BSBG13A"),
bckg.prctls.vars = "BSBGSLM", prctls = c(5, 25, 50),
output.file = "C:/temp/test.xlsx", open.output = TRUE)
}

# Repeat the analysis from above, this time with shortcut, include the cases with missing
# values in the splitting variables, and use the senate weights
\dontrun{
lsa.pcts.prctls(data.object = T15_G8_student_data, split.vars = c("BSBG01", "BSBG13A"),
bckg.prctls.vars = "BSBGSLM", prctls = c(5, 25, 50), weight.var = "SENWGT",
include.missing = TRUE, shortcut = TRUE, output.file = "C:/temp/test.xlsx",
open.output = TRUE)
}

# Repeat the analysis from above, adding a second continuous variable to compute the
# percentiles for, the "Students Like Learning Science" complex scale
\dontrun{
lsa.pcts.prctls(data.object = T15_G8_student_data, split.vars = c("BSBG01", "BSBG13A"),
bckg.prctls.vars = c("BSBGSLM", "BSBGSLS"), prctls = c(5, 25, 50), weight.var = "SENWGT",
include.missing = FALSE, shortcut = TRUE,
output.file = "C:/temp/test.xlsx", open.output = TRUE)
}

# Compute the 5th, 25th and 50th percentiles for the student overall reading achievement
# scores (i.e. using a set of PVs), using PIRLS 2016 student data file, split the output
# by student sex, use the full design, include the missing values od the splitting variable
# (i.e. student sex), and do not open the output after the computations are finished
\dontrun{
lsa.pcts.prctls(data.file = "C:/Data/PIRLS_2016_Student_Miss_to_NA.RData",
split.vars = "ASBG01", PV.root.prctls = "ASRREA", prctls = c(5, 25, 50),
include.missing = TRUE, output.file = "C:/temp/test.xlsx", open.output = FALSE)
}

}
\references{
LaRoche, S., Joncas, M., & Foy, P. (2016). Sample Design in TIMSS 2015. In M. O. Martin, I. V. S. Mullis, & M. Hooper (Eds.), \emph{Methods and Procedures in TIMSS 2015} (pp. 3.1-3.37). Chestnut Hill, MA: TIMSS & PIRLS International Study Center.

LaRoche, S., Joncas, M., & Foy, P. (2017). Sample Design in PIRLS 2016. In M. O. Martin, I. V. S. Mullis, & M. Hooper (Eds.), \emph{Methods and Procedures in PIRLS 2016} (pp. 3.1-3.34). Chestnut Hill, MA: Lynch School of Education, Boston College.
}
\seealso{
\code{\link{lsa.convert.data}}
}
