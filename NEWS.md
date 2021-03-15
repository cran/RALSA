# RALSA v.0.90.2 (2021-03-15)

## Bug fixes
* `lsa.convert.data` function. The function crashes when the different cycles of the study change the case of the file names and their extensions. Thank to Yuan-Ling (Linda) Liaw.

* `lsa.recode.vars` function. If the recoding instruction ends with semicolon, the function crashes.

* `lsa.bin.log.reg` function. The function crashes if some specific patterns are found in the binary dependent variable names.

* `lsa.pcts.means` function. Some missing estimates in columns Mean, Mean_SE, Mean_SVR, Mean_MVR, SD_SE and SD_MVR when the groups are too small.

* All analysis functions crash using SITES 2006 data when only mathematics teacher or only science teacher data are used.

* `lsa.pcts.means` function. Some estimates using CivED data were incorrect.

* `lsa.pcts.means` function. Missing estimates in some columns for specific combination of splitting variables.

## Miscellaneous
The `ISO` argument in `lsa.convert.data` is now case-insensitive.

The `file.types` and `ISO` arguments in `lsa.merge.data` are now case insensitive.

The error messages in `lsa.recode.vars` are improved.




# RALSA v.0.90.2 (2021-01-02)

## Bug fixes
* `lsa.convert.data` function. Unrecognized characters in factor levels are now fixed. Such were, for example the levels of the number of books variable in PIRLS 2016 and other cycles which displayed unrecognized characters instead of instead of "-".

* `GUI`. When categorical variables are added in the list of `Independent background categorical variables` in linear and logistic regression, the number of categories (`N cat.` column) and the drop-down menu for the reference categories (`Ref. cat.` column) include the missing values as well.

* `lsa.convert.data` function. For some variables categories have the same labels as the missing ones in other variables and are improperly converted as missing.

* When loading or switching to a tab in the `GUI`, it is scrolled to the position where the previous tab was scrolled to.

## New study cycles added
TIMSS 2019 is now fully supported.

## New study added
PISA for Development is now supported, as suggested by David Joseph Rutkowski.

## Miscellaneous
* Various improvements for the `GUI` elements location.

* Improved documentation.

* Links to the documentation for each functionality `RALSA` supports were added to the `Help` section of the `GUI`.

* Improved messages, warnings and error messages.




# RALSA v.0.90.1 (2020-10-26)

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
