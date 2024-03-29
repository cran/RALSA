#' @title R Analyzer for Large-Scale Assessments (RALSA)
#'
#' @description The RALSA package provides functionality for analyzing data from large-scale assessments and surveys which use complex sampling and assessment design. Such (international) assessments and surveys are TIMSS, PIRLS and PISA, for example.
#'
#' The sampling (complex sampling design) in large-scale assessments and surveys is multistage with probability proportional to the size of the (primary) sampling units (usually schools), i.e. with unequal probabilities of selection. Thus, all weights assigned to the individual respondents reflect these unequal probabilities. This is quite different from the usual simple or systematic random sampling. Different modifications of Jackknife Repeated Replication (JRR, with full or half replication) or Balanced Repeated Replication (BRR) are used in different studies to compute the standard errors of the population estimates. The proficiency test scores (complex assessment design) is applied to cope with practical issues. No respondent takes all test items, but the items are distributed across multiple test item blocks and the blocks are rotated across multiple assessment booklets, each respondent taking one booklet only. As a consequence, no respondent receives a single test score, but five (or even 10) separate test scores (called "plausible values" or PVs) resulting from multiple imputation technique where the missing by design responses are imputed. As a consequence of the complex sampling and assessment designs, each estimate has to be computed with each JRR or BRR weight and each PV (this can take up to 781 computations per estimate per group per country, depending on the study), then summarized to compute the final estimate, its sampling and imputation variance, and the final standard error.
#'
#' RALSA provides data preparation and analysis functions which take into account the complex sampling and assessment design of the studies. Each study has its a different implementation of the complex sampling and assessment designs and RALSA handles these and implements the corresponding computational procedure.
#'
#' @section Studies:
#' Currently, RALSA works with data from **all cycles** of the the following studies:
#' \itemize{
#' \item IEA CivED;
#' \item IEA ICCS;
#' \item IEA ICILS;
#' \item IEA RLII;
#' \item IEA PIRLS (including PIRLS Literacy and ePIRLS);
#' \item IEA TIMSS (including TIMSS Numeracy, eTIMSS);
#' \item IEA TiPi (TIMSS and PIRLS joint study);
#' \item IEA TIMSS Advanced;
#' \item IEA SITES;
#' \item IEA TEDS-M;
#' \item IEA REDS;
#' \item OECD PISA;
#' \item OECD PISA for Development;
#' \item OECD TALIS; and
#' \item OECD TALIS Starting Strong Survey (a.k.a. TALIS 3S).
#' }
#'
#' More studies (national international) will be added in future.
#'
#' @section Functions:
#' Currently, RALSA provides the following functionality:
#' \itemize{
#'     \item Data preparation functions - prepare data for analysis
#'         \itemize{
#'             \item \code{lsa.convert.data} The studies provide their data in SPSS and SAS format. In addition, PISA cycles prior to 2015 provide the data in \code{.TXT} format, along with their SPSS and SAS import syntax files. This function takes the originally provided SPSS data (or \code{.TXT}, along with the import syntaxes) files and converts them into native \code{.RData} files. It also adds variable labels, user-defined missing codes (if requested) and identifiers of the study, cycle, and respondent types (i.e. student, parent, teacher, school principal).
#'             \item \code{lsa.select.countries.PISA} Utility function to select countries' data from a converted PISA file and save them as a new file or assign them to an object in memory. This makes it more convenient to work with PISA data files which contain data from all countries per respondent type.
#'             \item \code{lsa.merge.data} The studies provide data from different respondents (i.e. student, parent, teacher, school principal) which are sampled hierarchically (e.g. students are nested in classes, classes are nested in schools and taught by teachers) and linked between each other. The files in the databases are provided separately per country and respondent type. This function merges data sets from different respondents and/or countries assuring the links between the different types of respondents (i.e. linking students only to principals' data only for their school and to the teachers who teach them). This function merges data for all studies, except for PISA where the structure of the files does not allow (for now) merging data from different respondent types.
#'            \item \code{lsa.vars.dict} Prints and/or saves variable dictionaries in a file. Convenient when need to know the structure of the variables of interest.
#'            \item \code{lsa.data.diag} Helper function for quick frequency (for categorical variables) and descriptive (continuous variables) tables (weighted or unweighted). These can serve for initial exploration of the data and elaborating hypotheses. Not intended for actual analysis.
#'            \item \code{lsa.recode.vars} Recodes variables from large-scale assessments taking care of the user-defined missing values. Convenient for collapsing categories or changing their order.
#'   }
#'     \item Analysis functions - estimates are on population level, taking into account the complex sampling and assessment design
#'         \itemize{
#'             \item \code{lsa.pcts.means} Computes percentages of respondents and means (arithmetic average, median or mode) for continuous variables within groups
#'             \item \code{lsa.prctls} Computes percentiles of continuous variables within groups
#'             \item \code{lsa.bench} Computes percentages of respondents reaching or surpassing benchmarks of achievement
#'             \item \code{lsa.crosstabs} Crosstabulations with Rao-Scott first- and second-order chi-square adjustments
#'             \item \code{lsa.corr} Computes correlations between variables (Pearson or Spearman)
#'             \item \code{lsa.lin.reg} Computes linear regression with or without contrast coding of categorical variables
#'             \item \code{lsa.bin.log.reg} Computes binary logistic regression with or without contrast coding of categorical variables
#'   }
#' }
#'
#' The \code{lsa.pcts.means}, \code{lsa.prctls}, \code{lsa.bench} and \code{lsa.crosstabs} also have the option to produce graphs from the estimates.
#'
#' More studies and analysis types will be added in future, and the existing ones will be updated, adding more features.
#'
#' RALSA also has a Graphical User Interface (GUI) for the less technical users. The GUI incorporates all aspects of the data preparation and analysis functions.
#'
#' @references
#'
#' Here are the two articles presenting the package and it's technical details:
#'
#' Mirazchiyski, P.V. (2021). RALSA: The R analyzer for large-scale assessments. \emph{Large-scale Assess Educ 9}(21), 1-24. https://doi.org/10.1186/s40536-021-00114-4
#'
#' Mirazchiyski, P. V. (2021). RALSA: Design and Implementation. \emph{Psych, 3}(2), 233-248. https://doi.org/10.3390/psych3020018
#'
#' Here is a list of selected references related to some of the studies' design, relevant to their latest cycles:
#'
#' Foy, P., & LaRoche, S. (2017). Estimating Standard Errors in the PIRLS 2016 Results. In M. O. Martin, I. V. S. Mullis, & M. Hooper (Eds.), \emph{Methods and Procedures in PIRLS 2016} (p. 4.1-4.22). Lynch School of Education, Boston College.
#'
#' Foy, P., & Yin, L. (2016). TIMSS 2015 Achievement Scaling Methodology. In M. O. Martin, I. V. S. Mullis, & M. Hooper (Eds.), \emph{Methods and Procedures in TIMSS 2015} (p. 13.1-13.62). TIMSS & PIRLS International Study Center.
#'
#' LaRoche, S., Joncas, M., & Foy, P. (2016). Sample Design in TIMSS 2015. In M. O. Martin, I. V. S. Mullis, & M. Hooper (Eds.), \emph{Methods and Procedures in TIMSS 2015} (p. 3.1-3.37). TIMSS & PIRLS International Study Center.
#'
#' OECD. (in press). \emph{PISA 2018 Technical Report}. OECD.
#'
#' Rutkowski, L., Gonzalez, E., Joncas, M., & von Davier, M. (2010). International Large-Scale Assessment Data: Issues in Secondary Analysis and Reporting. \emph{Educational Researcher, 39}(2), 142-151.
#'
#' Rutkowski, L., Rutkowski, D., & von Davier, M. (2014). A Brief Introduction to Modern International Large-Scale Assessment. In L. Rutkowski, M. von Davier, & D. Rutkowski (Eds.), \emph{Handbook of International Large-Scale Assessments: Background, Technical Issues, and Methods of Data Analysis} (pp. 3-10). CRC Press.
#'
#' @name RALSA
#' @author Plamen V. Mirazchiyski, INERI
#' @aliases RALSA-package
#' @import data.table openxlsx stringr foreign readr stringi shiny shinyWidgets shinydashboard shinyFiles
#' @importFrom DT JS renderDT DTOutput
#' @importFrom Hmisc wtd.table wtd.mean wtd.var
#' @importFrom shinyjs html hide reset extendShinyjs hidden inlineCSS useShinyjs show js
#' @importFrom stats contr.sum contr.treatment contrasts<- cov.wt formula as.formula qnorm setNames model.frame model.matrix pchisq pf xtabs
#' @importFrom utils head tail menu browseURL
#' @importFrom methods is
#' @importFrom ggplot2 ggplot ggsave geom_line geom_point geom_bar geom_vline scale_fill_manual sym aes geom_errorbar position_dodge theme element_rect element_blank element_line element_text scale_x_discrete scale_y_continuous labs scale_color_manual guides guide_legend expansion facet_wrap geom_tile scale_fill_gradient xlab ylab geom_text scale_y_discrete coord_equal ggtitle
#' @importFrom rclipboard rclipButton
#' @importFrom rstudioapi jobRunScript executeCommand
#' @importFrom haven read_sav as_factor
#' @importFrom import from
NULL
#> NULL

globalVariables(c("sampling.variance", "mean.of.PV.estimates", "sum.of.PV.diff", "measurement.variance", ".", "na.omit", "weighted.mean", "N", "key.vars", "variable", "Variable", "Statistic", "DESIGN", "COUNTRY", "Wald_Statistic", "Coefficients", "Coefficients_SE", "p_value", "Wald_L95CI", "Wald_U95CI", "Odds_L95CI", "Odds_U95CI", "DURATION", "JUSTONEVALID", "pt", "ind", "values", "Values", "MATSUBJ", "SCIWGT", "SCISUBJ", "MATWGT", "capture.output", "Estimate", "V1", "degrees.of.freedom", "pnorm", "stack", "tmp.pcts.var", "V2", "tmp.group.vars", "n_cases", "g", "removed.countries.where.any.split.var.is.all.NA", "Role", "DDD", "Estimate_SE", "avg.PVs.pct.miss", "n_Cases", "t_value", "DF", "i.t_value", "Percentiles", "END_TIME", "sum.of.squares", "PRCTLS.VARS", "weight.var", "TMPWGT", "file.merged.respondents", "Percent", "Valid_Percent", "Cumulative_Percent", "Value_Type", "Frequency", "Labels", "Names", "i", "Total", "Type", "Value", "value", "cnt.warn.insuff.RS.collector", "warnings.collector.multimodal", "CNT", "perf_group", "Performance_Group", "collapsed_split", "percentage.plots.files", "means.plots.files", "percentiles.plots.files", "crosstabs.plots.files", "IDCNTRY"))
