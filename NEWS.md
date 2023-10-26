# RALSA v1.3.7 (2023-10-25)

This update focuses mainly on some critical issues that were introduced after the changes in the last versions of R and CRAN policies on package documentation.

## Bug fixes

* When a file with school and teacher data merged is imported, the GUI crashes for all analysis functions.

* The GUI does not recognize some populations' files in different studies when using the `lsa.convert.data` function and crashes.

## Miscellaneous

* Various improvements and optimizations in the GUI workfrlow for all data preparation and analysis functions, including showing and hiding elements depending on the user selections.

* Improved documentation.





# RALSA v1.3.5 (2023-05-23)

This update focuses on adding more functionality to the existing graph capabilities, bug fixes and overall improvement of the workflow, and updates existing features following updates of packages RALSA depends on. Saving syntaxes from the GUI now appends the new synatx to existing files instead of overwriting them.

## Bug fixes
* `lsa.convert.data` crashes when converting the TIMSS 1995 grade 4 ASA files. Thanks to Maximilian Brinkmann.

* When converting datasets with `lsa.convert.data`, some of the variable labels contain unrecognized characters. Thanks to Falk Brese.

* When a factor variable has only one level, the “Variable levels" also include NA as a level.

* After updates of the `DT` package the panels displaying the country and variable names changed to grey and the colors of the selected rows changed.

* In GUI with `lsa.convert.data` the syntax is not generated and cannot be executed when PISA (both pre-2015 and later) files are used.

* In GUI with `lsa.recode.vars` the summary table of recodings is printed in the GUI console, but if any NA values are in the “Source_XXXXX” and “New_XXXXX” variable columns, the output is shifted to the left and unreadable for these lines.

## New functionality
* The descriptive statistics functions (`lsa.pcts.means`, `lsa.prctls`, `lsa.bench` and `lsa.crosstabs`) now have new arguments that allow definition of custom x- and y-axis labels for the plots. This has also been implemented in the GUI.

  * The `lsa.crosstabs` function now has the possibility to add custom axes' labels in heat plots. Until now, the function used the row and column variable names to label the axes. The custom axis labels can be added using the `graph.row.label` and `graph.col.label` arguments.

  * The `lsa.pcts.means` function now has the possibility to add custom axes' labels in percentages of respondents within groups and in means' plots. Until now, the function used the last split and the average variable(s) names to label the axes. The custom axis labels can be added using the  arguments `perc.x.label`, `perc.x.label`, `mean.x.labels` and `mean.y.labels`.

  * The `lsa.bench` function now has the possibility to add custom axes' labels in plots of percentages of respondents within performance groups and the mean for continuous variable, if specified. Until now, the function used “Performance Group” and the PVs set name to to label the axes for the percentage plots, and “Performance Group” and the mean's variable name to label the axes of the mean plot. The custom axis labels can be added using the  arguments `perc.x.label`, `perc.x.label`, `mean.x.label` and `mean.y.label`.

  * The `lsa.prctls` function now has the possibility to add custom axes' labels in plots of percentages of respondents within groups and percentiles for continuous variables. Until now, the function used the last split variable name to label the axes for the percentage plots, and “Percentiles” and the continuous variables' names to label the axes of the percentiles plot. The custom axis labels can be added using the  arguments `perc.x.label`, `perc.x.label`, `prctl.x.label` and `prctl.y.label`.

## Miscellaneous
* Added support for PIRLS 2021 data.

* The GUI can now run in Rstudio without blocking the console and the session can run and be used on its own. The GUI runs as a background job. In case problems appear with the new way the GUI is started, use `ralsaGUIfailsafe()` instead of `ralsaGUI()` function. This will start the GUI using the old methods, but the console will be blocked.

* The graphical functionality in `lsa.pcts.means`, `lsa.prctls`, `lsa.bench` and `lsa.crosstabs` has been updated after the `ggplot2` update to version 3.4.0 where the `size` aesthetic has been replaced with `linewidth` in line based geoms.

* `lsa.vars.dict` now adds the levels' numeric values before the labels, as suggested by Falk Brese.

* GUI with all functions. Following the updates of the `shiny` and `DT` packages the lists of variables lost their formatting (background color and selected rows color). These are now recovered and when rows are selected the variable names and labels are bolded.

* The colors of the radio buttons and check boxes in the GUI have been changed to match the color scheme used in the rest of the application.

* The behavior of the “Save syntax” button for all tabs in the GUI has been changed. When the file with the syntax already exists, the new syntax is appended to the end instead of overwriting the entire file. This is more convenient, as it allows to save all the syntax generated from different tabs in the GUI in a single file.

* The definition of user-defined missing values in the “Recode variables” section in the GUI now uses semicolon as a delimiter.

* Changed the icon for the "Exit" tab and button in the GUI.

* The copyleft character in the footer of the GUI has been changed, now it is displayed properly.

* Various improvements and optimizations in the GUI workfrlow.

* Improved documentation.





# RALSA v1.3.0 (2022-07-08)

This update focuses mainly on bugfixes and improvements in the GUI and its workflow, but also introduces some new features.

## Bug fixes
* All analysis functions, as well `lsa.data.diag`, `lsa.recode.vars` and `lsa.vars.dict` crash with an error message `Error in exists(all.vars(match.call())) : first argument has length > 1` when any of the arguments specifying analysis variables point to objects which contain character vectors containing them. Thanks to Rodolfo Ilizaliturri.

* `lsa.prctls` - The percentiles on the x-axis are not properly added for every percentile computed. Thanks to Cecilia Bjorkhammer.

* `print.lsa.data` - The custom print method for `lsa.data` blocks some common `data.table` operations.

* `lsa.corr` - The function crashes when one of the countries has all missing values for one or more variables passed to the `bckg.corr.vars`. A warning when this occurs was added.

* GUI with `lsa.convert.data` - When data files only from one country are available in the source folder, the quote at the end of the path passed to `inp.folder` in the syntax is not closed and the syntax cannot be executed properly. Thanks to Gasper Cankar.

* GUI with `lsa.merge.data` - When data files only from one country are available in the source folder, the bracket at the end of the list passed to `file.types` is not closed and the syntax cannot be executed properly. Thanks to multiple teachers during series of workshops delivered in Slovenia.

* GUI with `lsa.recode.vars` - When new variable names are defined, the table for the new variable labels still displays the old variable names, as shown below. Thanks to Cecilia Bjorkhammer.

* GUI with `lsa.data.diag` - When the working data set is changed, series of errors are dropped in the R console (`missing value where TRUE/FALSE is needed`).

* GUI with `lsa.pcts.means` - When the working data set is changed, an error is dropped in the R console (`Error in paste0: object 'Variables' not found`) and shown in the interface.

* GUI with `lsa.prctls` - When the working data set is changed, an error is dropped in the R console (`Error in if: argument is of length zero`) and shown in the interface.

* GUI with `lsa.bench` - When a data set is loaded or the working data set is changed, an error is dropped in the R console (`Error in paste0: object 'Variables' not found`).

* GUI with `lsa.crosstabs` - When a data set is loaded or the working data set is changed, an error is dropped in the R console (`Error in if: argument is of length zero`).

* GUI with `lsa.corr` - When the working data set is changed, an error is dropped in the R console (`Error in if: argument is of length zero`) and shown in the GUI.

* GUI with `lsa.lin.reg` - When the working data set is changed, errors are dropped in the R console (`Error in if: argument is of length zero` and `Warning: object 'Variables' not found`) and shown in the GUI.

* GUI with `lsa.bin.log.reg` - When the working data set is changed, errors are dropped in the R console (`Error in if: argument is of length zero` and `Warning: object 'Variables' not found`).

## New functionality
* The `lsa.crosstabs` function now has the possibility to produce heatmap graphs (optional). The graphs are included in a separate sheet in the MS Excel output file if `save.output = TRUE`. If `save.output = FALSE`, the graphs are added to the list output object in memory and can be printed in R's graphic device.

* All GUI tabs for data preparation and analysis functions received “Save syntax” and “Copy syntax” buttons, as suggested by Erika Majoros.

## Miscellaneous
* Further work on graphics (`lsa.crosstabs`, `lsa.bench`, `lsa.pcts.means`, `lsa.prctls`):

  * All functions producing graphical representation of the results now have controlled dpi depending on the number of `split.vars` to better fit the plots and their labels in space;
  
  * The `lsa.prctls` function now adds facets when the number of `split.vars` is greater than two to fix the issue of dot, line and error bar positioning and overlapping on the plot and improve readability.

* Multiple fixes in GUI for better workflow.

* In GUI with `lsa.recode.vars` the “Old/New variable names” table was merged with the “New variable labels” table. This change was provoked by fixing the bug that the “New variable labels” table was still showing the old variable names, as discovered by Cecilia Björkhammer. Now the workflow is more intuitive.

* Improved documentation.





# RALSA v1.2.0 (2022-05-04)

## Bug fixes
* `lsa.pcts.means` - when many split variables are added, in some countries some of the rows in the estimates are repeated multiple times by the categories of the split variables.

## New functionality
* The `lsa.pcts.means`, `lsa.bench` and `lsa.prctls` functions now have the possibility to produce graphs (optional). The graphs are included in a separate sheet in the MS Excel output file if `save.output = TRUE`. If `save.output = FALSE`, the graphs are added to the list output object in memory and can be printed in R's graphic device.

* The MS Excel output files from all analysis functions received an additional sheet with warnings (if any) related with the computations. So far these warnings were just printed on screen.

## Miscellaneous
* Internal reorganization of the code producing warnings related with the computations and the way they are issued.

* Improved documentation.

* Various code changes following the update of R to version 4.2.0.





# RALSA v1.1.5 (2022-03-30)

## Bug fixes
* GUI with `lsa.pcts.means` does not show the syntax when only splitting variables are chosen for the analysis and the GUI freezes when the analysis is ran.

## New functionality
* `lsa.pcts.means` received a new argument, `central.tendency`, which allows users to compute either the arithmetic mean (default and available so far), median (new) or mode (new) for continuous variables.

* New function, `lsa.select.countries.PISA`, a utility function that allows the user to select countries of interest from a converted PISA data file (or PISA object residing in memory) and remove the rest of the countries' data. This is useful when the user does not want to analyze all countries data available in an original a PISA data file.

## Miscellaneous
* All analysis functions received a new argument, `save.output`. If `TRUE` (default), the output is written into MS Excel file, as it was so far. If `FALSE`, the output (a list of all different estimates) is printed on screen or can be assigned to an object. The argument is available in command-line use, but not in the GUI.

* Multiple fixes in GUI for better workflow.

* Improved documentation.





# RALSA v1.1.0 (2022-02-04)

## Bug fixes
* All analysis functions. When "IDCNTRY" is added explicitly to the list of `split.vars` and `include.missing = TRUE`, the function crashes with the following error message. Thanks to Laura Ringiene.

* `lsa.prctls` function. When computations involve PVs, the estimates of specific percentile appear in a wrong column, e.g. the 25th percentile columns contain the estimates for the 5th percentile columns and the other way around. Thanks to Laura Ringiene.

* `lsa.data.diag` function. When the tables are exported to Excel, the "Index" sheet appears as the first one, but the file opens with the sheet with statistics for the first variable on the front.

* GUI with `lsa.data.diag` and all analysis functions. When using the GUI and an output file with the same name as the specified is already opened, the GUI crashes when the respective function tries to write the new output file.

* `lsa.bin.log.reg` function. The coefficients output is not sorted properly by the names of the independent variables and the desired order.

* GUI with `lsa.corr` function. When the loaded data file in "Correlations" analysis contains no PVs, the radio buttons for choosing between Pearson and Spearman correlation are not shown.

* GUI with all functions. If any error occurs during a function execution (i.e. after pressing the "Execute syntax" button), the GUI crashes.

## New functionality
* New analysis function, `lsa.crosstabs`. It computes a two-way table (crosstabulations) and Rao-Scott first- and second-order design corrected chi-square statistics. The Rao-Scott adjustment is needed because of the clustered design of the large-scale assessments and surveys data.

* Added the possibility to include two-way interaction terms in `lsa.lin.reg` and `lsa.bin.log.reg`. The interactions can be between two categorical variables, categorical and continuous, continuous and continuous, categorical with PVs, continuous with PVs, and PVs with PVs.

* Added a new printing method for `lsa.data` objects in memory. It prints the study name, cycle, respondent type(s), total number of countries, key, if the data has user-defined missing values or not, and a snippet of the data.

## New study added
* Support for the IEA Responses to Educational Disruption Survey (REDS) 2021 was added.

## Miscellaneous
* Added support for the recently released IEA eTIMSS 2019 Problem Solving and Inquiry (PSI) tasks' data.

* The RALSA logos in the GUI and GUI start-up screen was replaced with higher-resolution images.

* Various visual improvements of the GUI.

* Updated and improved documentation.





# RALSA v1.0.2 (2021-10-21)

This is a maintenance version fixing some bugs and removing package dependency.

## Bug fixes
* `lsa.pcts.means` function crashes with an error message when no split variables are added and both background variables and PVs are added to compute means for. Thanks to an anonymous RALSA user.

* `lsa.bin.log.reg` function crashes when the variable name passed to `bin.dep.var` contains numeric characters.

* `lsa.lin.reg` function crashes when the variable name passed to `bckg.dep.var` contains numeric characters.

* When used in the GUI, the `lsa.recode.vars` assigns the new value labels to the recoded values in incorrect order. Thanks to multiple workshop participants at the ECER 2021 pre-conference training.

## Miscellaneous
* The `gdata` package is no longer needed by `RALSA`.

* Start up and warning messages when dependencies are loaded during GUI start are not displayed anymore.





# RALSA v1.0.1 (2021-05-28)

This is a maintenance version following the update of base R to v4.1.0 where some functions' behavior has changed and cause crashes in the analysis functions.

## Bug fixes
* All analysis functions. Some functions in base R were updated and their behavior has changed causing crashes in the analysis functions with `first argument has length \> 1` error messages.

* `lsa.vars.dict` function. When there are just two levels for a factor variable, the second level in ?Variable levels? ends without a single quote.

* Occasionally, the `lsa.data.diag`  freezes R with a question mark in the console and eventually crashes the R session when `split.vars` are provided.

## Miscellaneous
* Various optimizations in the `lsa.corr`, `lsa.lin.reg` and `lsa.bin.log.reg` functions.

* The `lsa.data.diag` function now has user interruption handling with a message.





# RALSA v1.0.0 (2021-04-28)

## Bug fixes
* All analysis functions. Any analysis function crashes when TALIS 3S data is used and the weight variable is specified explicitly.

* All analysis functions. When ICCS teacher and school data are merged, the default weight, jakknifing zone and jakknifing replication indicator are not automatically detected and the function crashes, while specifying the weight manually passes.

* `lsa.prctls` function. The order of the columns for the percentiles, their SE, SVR and MVR is scrambled.

* All data preparation and analysis functions. When using `data.object` to provide data and the object is in quotes, the function crashes with uninterpretable error message. Added custom handle and error message.

* `lsa.lin.reg` function. The function crashes when using specific variable combinations.

* GUI. The filtering of the tables with country ISOs, country names, variable names and labels is very slow and oftentimes unresponsive.

* GUI with `lsa.recode.vars`. Occasionally, when the `lsa.recode.vars` function is executed by pressing the "Execute syntax" button, the interface crashes. This is because there is a factor level for which no actual values exist in the data. Now handled with custom error message.

* `lsa.merge.data` function. The school weight was dropped when merging school and teacher data in TALIS which prevented the use of the merged file in multilevel models. Thanks to Jelena Veletic.

## New functionality
* RALSA has a new data preparation function, `lsa.data.diag`, a quick automated production of weighted or unweighted frequency tables for categorical variables and descriptive statistics for continuous variables. These tables are for data diagnostic purposes prior to analysis and are not for reporting results from large-scale assessments.

## Miscellaneous
* After updating R to v4.0.5 and the packages RALSA depends on, the warning messages in the GUI console were not displayed, although displayed in RStudio console. The warning messages are now back in the GUI console as well.

* Showing and hiding elements in the GUI when using the benchmarks analysis type was improved.

* The version number was incremented to 1.0 since the package has already achieved stability and maturity.





# RALSA v0.90.3 (2021-03-15)

## Bug fixes
* `lsa.convert.data` function. The function crashes when the different cycles of the study change the case of the file names and their extensions. Thanks to Yuan-Ling (Linda) Liaw.

* `lsa.recode.vars` function. If the recoding instruction ends with semicolon, the function crashes.

* `lsa.bin.log.reg` function. The function crashes if some specific patterns are found in the binary dependent variable names.

* `lsa.pcts.means` function. Some missing estimates in columns Mean, Mean_SE, Mean_SVR, Mean_MVR, SD_SE and SD_MVR when the groups are too small.

* All analysis functions crash using SITES 2006 data when only mathematics teacher or only science teacher data are used.

* `lsa.pcts.means` function. Some estimates using CivED data were incorrect.

* `lsa.pcts.means` function. Missing estimates in some columns for specific combination of splitting variables.

## Miscellaneous
* The `ISO` argument in `lsa.convert.data` is now case-insensitive.

* The `file.types` and `ISO` arguments in `lsa.merge.data` are now case insensitive.

* The error messages in `lsa.recode.vars` are improved.




# RALSA v0.90.2 (2021-01-02)

## Bug fixes
* `lsa.convert.data` function. Unrecognized characters in factor levels are now fixed. Such were, for example the levels of the number of books variable in PIRLS 2016 and other cycles which displayed unrecognized characters instead of instead of "-".

* `GUI`. When categorical variables are added in the list of `Independent background categorical variables` in linear and logistic regression, the number of categories (`N cat.` column) and the drop-down menu for the reference categories (`Ref. cat.` column) include the missing values as well.

* `lsa.convert.data` function. For some variables categories have the same labels as the missing ones in other variables and are improperly converted as missing.

* When loading or switching to a tab in the `GUI`, it is scrolled to the position where the previous tab was scrolled to.

## New study cycles added
TIMSS 2019 is now fully supported.

## New study added
PISA for Development is now supported, as requested by David Joseph Rutkowski.

## Miscellaneous
* Various improvements for the `GUI` elements location.

* Improved documentation.

* Links to the documentation for each functionality `RALSA` supports were added to the `Help` section of the `GUI`.

* Improved messages, warnings and error messages.




# RALSA v0.90.1 (2020-10-26)

The first version of the R Analyzer for Large-Scale Assessments (`RALSA`) is released. `RALSA` targets both the experienced R users, as well as those less technical skills. Thus, along with the traditional command-line R interface, a Graphical User Interface is featured.

Note that this is a "first release" version, so some bugs are expected.

`RALSA` is is used for preparation and analysis of data from large-scale assessments and surveys which use complex sampling and assessment design. Currently, `RALSA` supports a number of studies with different design and a number of analysis types (see below). Both of these will increase in future.

`RALSA` is a free and open source software licensed under GPL v2.0.

Currently, `RALSA` supports the following functionality:

* Prepare data for analysis

    * Convert data (SPSS, or text in case of PISA prior 2015)
    
    * Merge study data files from different countries and/or respondents
    
    * View variable properties (name, class, variable label, response categories/unique values, user-defined missing values)
    
    * Recode variables
    
* Perform analyses (more analysis types will be added in future)
    
    * Percentages of respondents in certain groups and averages on variables of interest, per group
    
    * Percentiles of variables within groups of respondents
    
    * Percentages of respondents reaching or surpassing benchmarks of achievement
    
    * Correlations (Pearson or Spearman)
    
    * Linear regression
    
    * Binary logistic regression

All data preparation and analysis functions automatically recognize the study design and apply the appropriate techniques to handle the complex sampling assessment design issues, while giving freedom to tweak the analysis (e.g. change the default weight, apply the "shortcut" method in TIMSS and PIRLS, and so on).

Currently, `RALSA` can work with data from all cycles of the following studies (more will be added in future):

* CivED

* ICCS

* ICILS

* RLII

* PIRLS (including PIRLS Literacy and ePIRLS)

* TIMSS (including TIMSS Numeracy, eTIMSS will be added with the upcoming release of TIMSS 2019)

* TiPi (TIMSS and PIRLS joint study)

* TIMSS Advanced

* SITES

* TEDS-M

* PISA

* TALIS

* TALIS Starting Strong Survey (a.k.a. TALIS 3S)
