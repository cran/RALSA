suppressWarnings(suppressMessages(suppressPackageStartupMessages(library(data.table, warn.conflicts = FALSE, quietly = TRUE))))
suppressWarnings(suppressMessages(suppressPackageStartupMessages(library(openxlsx, warn.conflicts = FALSE, quietly = TRUE))))
suppressWarnings(suppressMessages(suppressPackageStartupMessages(library(Hmisc, warn.conflicts = FALSE, quietly = TRUE))))
suppressWarnings(suppressMessages(suppressPackageStartupMessages(library(stringr, warn.conflicts = FALSE, quietly = TRUE))))
suppressWarnings(suppressMessages(suppressPackageStartupMessages(library(foreign, warn.conflicts = FALSE, quietly = TRUE))))
suppressWarnings(suppressMessages(suppressPackageStartupMessages(library(readr, warn.conflicts = FALSE, quietly = TRUE))))
suppressWarnings(suppressMessages(suppressPackageStartupMessages(library(stringi, warn.conflicts = FALSE, quietly = TRUE))))
suppressWarnings(suppressMessages(suppressPackageStartupMessages(library(shiny, warn.conflicts = FALSE, quietly = TRUE))))
suppressWarnings(suppressMessages(suppressPackageStartupMessages(library(shinydashboard, warn.conflicts = FALSE, quietly = TRUE))))
suppressWarnings(suppressMessages(suppressPackageStartupMessages(library(DT, warn.conflicts = FALSE, quietly = TRUE))))
suppressWarnings(suppressMessages(suppressPackageStartupMessages(library(shinyjs, warn.conflicts = FALSE, quietly = TRUE))))
suppressWarnings(suppressMessages(suppressPackageStartupMessages(library(shinyFiles, warn.conflicts = FALSE, quietly = TRUE))))
suppressWarnings(suppressMessages(suppressPackageStartupMessages(library(ggplot2, warn.conflicts = FALSE, quietly = TRUE))))
suppressWarnings(suppressMessages(suppressPackageStartupMessages(library(rclipboard, warn.conflicts = FALSE, quietly = TRUE))))
suppressWarnings(suppressMessages(suppressPackageStartupMessages(library(haven, warn.conflicts = FALSE, quietly = TRUE))))
suppressWarnings(suppressMessages(suppressPackageStartupMessages(library(shinyWidgets, warn.conflicts = FALSE, quietly = TRUE))))

# Load RALSA functions
import::from(RALSA,
             lsa.convert.data,
             lsa.select.countries.PISA,
             lsa.merge.data, lsa.vars.dict,
             lsa.data.diag, lsa.recode.vars,
             lsa.pcts.means,
             lsa.prctls,
             lsa.bench,
             lsa.crosstabs,
             lsa.corr,
             lsa.lin.reg,
             lsa.bin.log.reg)

ui <- tagList(
  
  load.app.CSS.screen <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 1;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
",
jscode.close.RALSA.GUI <- "shinyjs.closeWindow = function() { window.close(); }",

jscode.scroll.tab.to.top <- 'shinyjs.scrolltop = function() {window.scrollTo(0, 0);}',

  useShinyjs(),
  rclipboardSetup(),
  inlineCSS(load.app.CSS.screen),
  div(
    id = "loading-content", class = "center",
    h1(HTML("<br/><br/><blink>Loading...</blink><br/><br/>")),
    img(src = 'RALSA_Logo.png', style = "width: 452px; height: 170px;"),
    h1(HTML("<br/>Brought to you by the<br/><br/><strong>International Educational Research and Evaluation Institute</strong><br/><br/>"), tags$a(href="https://www.ralsa.ineri.org/", "(https://www.ineri.org/)", target = "_blank"))
  ),
  hidden(
    div(
      id = "app-content",
      dashboardPage(
        title = "RALSA",
        dashboardHeader(
          title =
            tags$a(img(src = "RALSA_Logo.png", height = "60px"), href = "https://ralsa.ineri.org/", target = "_blank"), titleWidth = 260,
          tags$li(class = "dropdown",
                  tags$style(".main-header .logo {height: 70px; padding-top: 5px; padding-left: 15px; background-color: #000000 !important;}"),
                  tags$style(".sidebar-toggle {background-color: #000000 !important; width: 0px;}"),
                  tags$style(".navbar {background-color: #000000 !important; color: #ffffff !important; font-size: 20px;}"),
                  tags$header(HTML("R Analyzer for Large-Scale Assessments"), align = "left", style = "
position: fixed;
left: 300px; /*Add a left margin of 300px so that the header does not cover the logo on the left*/
width: 100%;
height: 22px;
color: white;
font-size: 25px;
padding-top: 15px;
padding-left: 75px; /*Add left padding to indent the header text so that it is displayed a bit further away from the toggle. The best would be to be centered to the navigation bar, but it is not possible at the moment.*/
background-color: #000000;"),
          )
        ),
        dashboardSidebar(width = 260,
                         tags$style(".main-sidebar {background-color: #000000 !important;}"),
                         sidebarMenu(id = "home",
                                     br(), br(),
                                     menuItem(text = "Home", icon = icon("home"), tabName = "home")
                         ),
                         sidebarMenu(id = "dataMenu",
                                     menuItem(text = "Data preparation", icon = icon("database"), tabName = "dataPreparation",
                                              menuSubItem(text = "Convert data", icon = icon("random"), tabName = "convertData"),
                                              menuSubItem(text = "Merge data", icon = icon("puzzle-piece"), tabName = "mergeData"),
                                              menuSubItem(text = "Variable dictionaries", icon = icon("clipboard-list"), tabName = "varProperties"),
                                              menuSubItem(text = "Data diagnostics", icon = icon("table"), tabName = "dataDiag"),
                                              menuSubItem(text = "Recode variables", icon = icon("sort-numeric-down"), tabName = "recodeVars"),
                                              menuSubItem(text = "Select PISA countries", icon = icon("filter"), tabName = "selectPISACountries")
                                     )
                         ),
                         sidebarMenu(id = "analysisMenu",
                                     menuItem(text = "Analysis types", icon = icon("square-root-alt"), tabName = "analyzeData",
                                              menuSubItem(text = "Percentages and means", icon = icon("chart-pie"), tabName = "pctsMeans"),
                                              menuSubItem(text = "Percentiles", icon = icon("chart-pie"), tabName = "prctls"),
                                              menuSubItem(text = "Benchmarks", icon = icon("chart-pie"), tabName = "bnchMarks"),
                                              menuSubItem(text = "Crosstabulations", icon = icon("chart-pie"), tabName = "crossTabs"),
                                              menuSubItem(text = "Correlations", icon = icon("chart-pie"), tabName = "corr"),
                                              menuSubItem(text = "Linear regression", icon = icon("chart-pie"), tabName = "linReg"),
                                              menuSubItem(text = "Binary logistic regression", icon = icon("chart-pie"), tabName = "binLogReg")
                                     )
                         ),
                         sidebarMenu(id = "help",
                                     menuItem(text = "Help", icon = icon("question-circle"), tabName = "ralsaHelp",
                                              menuSubItem(text = "Help sections", icon = icon("question-circle"), tabName = "helpSections"),
                                              menuSubItem(text = "Participating countries", icon = icon("table"), tabName = "partCountries")
                                     )
                         ),
                         sidebarMenu(id = "exit",
                                     menuItem(text = "Exit", icon = icon("power-off"), tabName = "exitUI")
                         ),
                         tags$style(".main-sidebar {background-color: #000000 !important;}"),
                         tags$style(".sidebar-menu li a {color: white !important;}"),
                         tags$style(HTML(".sidebar-menu li.active a {border-left-color: red !important;}")),
                         tags$style(HTML(".sidebar-menu li:hover a {border-left-color: red !important;}")),
                         tags$style(HTML(".treeview-menu a {background-color: #000000 !important; text-indent: 5px !important;}")),
                         tags$style(HTML(".treeview-menu li.active a {background-color: #FE0F1A !important; text-indent: 10px !important;}")),
                         tags$style(HTML(".treeview-menu li:hover a {background-color: #000000 !important;}"))
        ),
        dashboardBody(
          extendShinyjs(text = jscode.scroll.tab.to.top, functions = c("scrolltop")),
          tags$style(HTML('table.dataTable tbody>* {background-color: #ffffff !important;}')),
          tags$style(HTML('div.dt-buttons {position: fixed; right: 0;}')),
          tags$style(HTML('table.dataTable tbody tr.selected>* {box-shadow: inset 0 0 0 9999px #e2e2e2 !important; color: #000000; font-weight: bold}')),
          tags$style(HTML("input[type='checkbox']:checked {width: 10px; height: 10px; accent-color: #ffffff; outline: 2px solid #767676; outline-offset: max(0px, 0em); border-radius: 10px;}")),
          tags$style(HTML('input[type="radio"]:checked {accent-color: #767676;}')),
          tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico", type="image/x-icon")),
          tags$style(HTML(".content-wrapper, .right-side {
background-color: #e2e2e2;
}")),
          tags$head(tags$style(HTML(
            "
.multicol .shiny-options-group {
width: 700px;
height: auto;
padding-top: 15px !important;
-webkit-column-count: 4; /* Chrome, Safari, Opera */
-moz-column-count: 4;    /* Firefox */
column-count: 4;
-moz-column-fill: balanced;
-column-fill: balanced;
}
.checkbox {
margin-top: 0px !important;
-webkit-margin-after: 0px !important;
}
"))),
          tags$script(HTML("$('body').addClass('fixed');")),
          tags$head(
            tags$style(
              HTML(".shiny-notification {
position:fixed;
top: calc(50%);;
left: calc(50%);
color: black;
font-size: 18px;
opacity: 1;}"))
          ),
          tags$head(tags$style(HTML("a {color: #FE0F1A}"))),
          tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {border: 0.05em solid #c1c1c1; background-color: #dddddd !important;}')),
          tags$head(tags$style(HTML('.has-feedback .form-control {padding-left: 0px; padding-right: 0px;}'))),
          tags$head(tags$script(
            HTML(
              "Shiny.addCustomMessageHandler('unbindDT', function(id) {
var $table = $('#'+id).find('table');
if($table.length > 0) {
Shiny.unbindAll($table.DataTable().table().node());
}
})")
          )),
          tags$head(tags$style("td .form-group {margin-bottom: 0; margin-top: 0;}")),
          tabItems(
            tabItem(tabName = "home", class = "active",
                    fluidRow(align = "center",
                             h1(textOutput(outputId = "welcomeToRALSA"))
                    ),
                    htmlOutput(outputId = "welcomeText")
            ),
            tabItem(tabName = "helpSections", class = "active",
                    h1(textOutput(outputId = "helpSectionsHeading")),
                    htmlOutput(outputId = "helpOnRALSAWebsite"),
                    htmlOutput(outputId = "helpSectionRALSAWebsiteLinks")
            ),
            tabItem(tabName = "partCountries", class = "active",
                    h1(textOutput(outputId = "partCountriesHeading")),
                    htmlOutput(outputId = "helpOnPartCountries"),
                    div(
                      style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                      div(uiOutput("selectStudyCycleDropdown")),
                      div(style = "width: 30px;"),
                      div(checkboxInput(inputId = "partCountriesFilterParticipating", label = "Filter only the participating countries", value = FALSE), style = " padding-top: 30px;")
                    ),
                    DTOutput(outputId = "selectStudyCycleTable"),
                    br()
            ),
            tabItem(tabName = "exitUI", class = "active",
                    h1(textOutput(outputId = "exitHeading")),
                    extendShinyjs(text = jscode.close.RALSA.GUI, functions = c("closeWindow")),
                    actionButton(inputId = "closeGUI", label = "Exit", icon = icon("power-off"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
            ),
            tabItem(tabName = "convertData", class = "active",
                    h1(textOutput(outputId = "h1ConvertData")),
                    htmlOutput(outputId = "convertIntro"),
                    div(
                      style = "display:-webkit-flex; display:-ms-flexbox; display:flex; padding-bottom:30px",
                      shinyDirButton(id = "convertChooseSrcDir", label = "Choose source folder", title = "Navigate and select a folder", icon = icon("folder-open"), style = "color: #ffffff; background-color: #000000; border-radius: 10px; height: 33px;"),
                      div(style = "width: 30px;"),
                      conditionalPanel(condition = "input.convertChooseSrcDir",
                                       div(verbatimTextOutput(outputId = "convertSrcPathDisplay"), style = "min-width: 750px;"),
                                       tags$head(tags$style("#convertSrcPathDisplay {background-color: white;}"))
                      ),
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px;",
                        div(style = "display: inline-block;",
                            htmlOutput(outputId = "convertIEAStudyName"),
                            htmlOutput(outputId = "convertIEAStudyCycle")))
                    ),
                    conditionalPanel(condition = "output.convertIEAStudyName && output.convertIEAStudyCycle",
                                     div(style = "display: inline-block; padding-top: 30px; padding-bottom:15px;",
                                         htmlOutput(outputId = "convertAvailableIEACntsText")
                                     ),
                                     fluidRow(
                                       column(width = 6,
                                              DTOutput(outputId = "convertAvailCntIEAFiles"),
                                              tags$head(tags$style("#convertAvailCntIEAFiles {white-space: nowrap;}"))
                                       ),
                                       conditionalPanel(condition = "output.convertAvailCntIEAFiles",
                                                        column(width = 1, align = "center",
                                                               br(), br(), br(), br(), br(), br(), br(), br(), br(),
                                                               uiOutput(outputId = "convertArrowIEARight"),
                                                               uiOutput(outputId = "convertDblArrowIEARight"),
                                                               br(), br(),
                                                               uiOutput(outputId = "convertArrowIEALeft"),
                                                               uiOutput(outputId = "convertDblArrowIEALeft")
                                                        )
                                       ),
                                       column(width = 5,
                                              DTOutput(outputId = "convertSelectionIEA"),
                                              tags$head(tags$style("#convertSelectionIEA {white-space: nowrap;}"))
                                       )
                                     )
                    ),
                    fluidRow(column(width = 12,
                                    htmlOutput(outputId = "convertPISA2015PlusStudyName"),
                                    htmlOutput(outputId = "convertPISA2015PlusStudyCycle")
                    )),
                    conditionalPanel(condition = "output.convertPISA2015PlusStudyName && output.convertPISA2015PlusStudyCycle",
                                     div(style = "display: inline-block; padding-top: 30px; padding-bottom:15px;",
                                         htmlOutput(outputId = "convertAvailablePISA2015PlusFilesText")
                                     ),
                                     fluidRow(
                                       column(width = 2),
                                       column(width = 8,
                                              DTOutput(outputId = "convertPISA2015PlusFiles"),
                                              tags$head(tags$style("#convertPISA2015PlusFiles {white-space: nowrap;}"))
                                       ),
                                       column(width = 2)
                                     )),
                    fluidRow(column(width = 12,
                                    htmlOutput(outputId = "convertPISApre2015StudyName"),
                                    htmlOutput(outputId = "convertPISApre2015StudyCycle")
                    )),
                    conditionalPanel(condition = "output.convertPISApre2015StudyName && output.convertPISApre2015StudyCycle",
                                     div(style = "display: inline-block; padding-top: 30px; padding-bottom:15px;",
                                         htmlOutput(outputId = "convertAvailablePISApre2015FilesText")
                                     ),
                                     fluidRow(
                                       column(width = 2),
                                       column(width = 8,
                                              DTOutput(outputId = "convertPISApre2015Files"),
                                              tags$head(tags$style("#convertPISApre2015Files {white-space: nowrap;}"))
                                       ),
                                       column(width = 2)
                                     )),
                    fluidRow(column(width = 12,
                                    htmlOutput(outputId = "convertPISADev2019PlusStudyName"),
                                    htmlOutput(outputId = "convertPISADev2019PlusStudyCycle")
                    )),
                    conditionalPanel(condition = "output.convertPISADev2019PlusStudyName && output.convertPISADev2019PlusStudyCycle",
                                     div(style = "display: inline-block; padding-top: 30px; padding-bottom:15px;",
                                         htmlOutput(outputId = "convertAvailablePISADev2019PlusFilesText")
                                     ),
                                     fluidRow(
                                       column(width = 2),
                                       column(width = 8,
                                              DTOutput(outputId = "convertPISADev2019PlusFiles"),
                                              tags$head(tags$style("#convertPISADev2019PlusFiles {white-space: nowrap;}"))
                                       ),
                                       column(width = 2)
                                     )),
                    fluidRow(
                      column(width = 12,
                             conditionalPanel(condition = "output.convertSelectionIEA || output.convertPISA2015PlusFiles || output.convertPISApre2015Files || output.convertPISADev2019PlusFiles",
                                              fluidRow(
                                                column(width = 4,
                                                       div(style = "margin-top:38px;;",
                                                           checkboxInput(inputId = "convertMissToNA", label = "Convert user-defined missings to NA", value = FALSE))
                                                )),
                                              fluidRow(column(width = 6,
                                                              div(
                                                                style = "display:-webkit-flex; display:-ms-flexbox; display:flex; padding-top: 30px; margin-bottom: 20px;",
                                                                shinyDirButton(id = "convertChooseOutDir", label = "Choose destination folder", title = "Navigate and select a folder", icon = icon("folder-open"), style = "color: #ffffff; background-color: #000000; border-radius: 10px; height: 33px;"),
                                                                div(style = "width: 30px;"),
                                                                div(verbatimTextOutput(outputId = "convertOutPathDisplay"), style = "min-width: 750px;"),
                                                                tags$head(tags$style("#convertOutPathDisplay {background-color: white;}"))
                                                              )
                                              )),
                                              conditionalPanel(condition = "intput.convertChooseOutDir",
                                                               div(style="display:inline-block", textOutput(outputId = "convertSyntaxHead")),
                                                               div(style="display:inline-block", shinySaveButton(id = "saveConvertSyntax", label = "Save syntax", "Save syntax as...", filetype = list(R = "r"), icon = icon("download"), style = "color: #ffffff; background-color: #000000; border-radius: 5px; font-size: 80%; margin-bottom: 1px; padding: 1px; width: 85px; margin-bottom: 0px; margin-left: 25px")),
                                                               div(style="display:inline-block", uiOutput(outputId = "copyConvertSyntax")),
                                                               verbatimTextOutput(outputId = "convertSyntax"),
                                                               tags$head(tags$style(HTML("#convertSyntax {background-color: white; white-space: pre-wrap;}")))
                                              ),
                                              conditionalPanel(condition = "output.convertSyntax",
                                                               div(style = "margin-top: 30px",
                                                                   textOutput(outputId = "convertExecBtnHead")
                                                               ),
                                                               div(style = "margin-bottom: 30px",
                                                                   uiOutput(outputId = "execConvertData")
                                                               )
                                              ),
                                              conditionalPanel(condition = "output.execConvertData != 0",
                                                               verbatimTextOutput(outputId = "consoleConvertData"),
                                                               tags$head(tags$style("#consoleConvertData {color:red; background-color: white; overflow-y:scroll; max-height: 500px; margin-bottom: 30px}")),
                                                               tags$script(
                                                                 '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleConvertData"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                                               ),
                                              ),
                             )
                      )),
            ),
            tabItem(tabName = "mergeData", class = "active",
                    h1(textOutput(outputId = "h1MergeData")),
                    htmlOutput(outputId = "mergeIntro"),
                    div(
                      style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                      shinyDirButton(id = "mergeChooseSrcDir", label = "Choose source folder", title = "Navigate and select a folder", icon = icon("folder-open"), style = "color: #ffffff; background-color: #000000; border-radius: 10px; height: 33px;"),
                      div(style = "width: 30px;"),
                      conditionalPanel(condition = "input.mergeChooseSrcDir",
                                       div(verbatimTextOutput(outputId = "mergeSrcPathDisplay"), style = "min-width: 750px;"),
                                       tags$head(tags$style("#mergeSrcPathDisplay {background-color: white;}"))
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px; padding-bottom:30px",
                        div(style = "display: inline-block; padding-top:30px;",
                            htmlOutput(outputId = "mergeIEAStudyName"),
                            htmlOutput(outputId = "mergeIEAStudyCycle")
                        )
                      )),
                    div(style = "display: inline-block; padding-bottom: 15px;",
                        htmlOutput(outputId = "mergeAvailableIEACntsText")
                    ),
                    conditionalPanel(condition = "output.mergeIEAStudyName && output.mergeIEAStudyCycle",
                                     fluidRow(
                                       column(width = 6,
                                              DTOutput(outputId = "mergeAvailCntIEAFiles"),
                                              tags$head(tags$style("#mergeAvailCntIEAFiles {white-space: nowrap;}"))
                                       ),
                                       conditionalPanel(condition = "output.mergeAvailCntIEAFiles",
                                                        column(width = 1, align = "center",
                                                               br(), br(), br(), br(), br(), br(), br(), br(), br(),
                                                               uiOutput(outputId = "mergeArrowIEARight"),
                                                               uiOutput(outputId = "mergeDblArrowIEARight"),
                                                               br(), br(),
                                                               uiOutput(outputId = "mergeArrowIEALeft"),
                                                               uiOutput(outputId = "mergeDblArrowIEALeft")
                                                        )
                                       ),
                                       column(width = 5,
                                              DTOutput(outputId = "mergeSelectionIEA"),
                                              tags$head(tags$style("#mergeSelectionIEA {white-space: nowrap;}"))
                                       ),
                                     ),
                                     fluidRow(
                                       div(style = "display: inline-block; margin-top: 35px;",
                                           column(width = 12, htmlOutput(outputId = "mergeAvailRespText"))
                                       )
                                     ),
                                     fluidRow(
                                       column(width = 12,
                                              div(style = "display: inline-block; margin-top: 5px;",
                                                  uiOutput(outputId = "mergeAvailRespCheckboxes")
                                              )
                                       ),
                                       column(width = 12,
                                              uiOutput(outputId = "warnNoSuchCombination"),
                                              tags$head(tags$style("#warnNoSuchCombination {color: red; font-weight: bold;}"))
                                       )
                                     ),
                                     fluidRow(
                                       column(width = 12,
                                              div(style = "display: inline-block; margin-top: 20px; margin-bottom: 5px;",
                                                  htmlOutput(outputId = "mergeAvailVarsText")
                                              )
                                       ),
                                       column(width = 6,
                                              DTOutput(outputId = "mergeAllAvailableVars"),
                                              tags$head(tags$style("#mergeAllAvailableVars {white-space: nowrap;}"))
                                       ),
                                       column(width = 1, align = "center",
                                              br(), br(), br(), br(), br(), br(), br(), br(), br(),
                                              uiOutput(outputId = "mergeArrowSelVarsRight"),
                                              uiOutput(outputId = "mergeDblArrowSelVarsRight"),
                                              br(), br(),
                                              uiOutput(outputId = "mergeArrowSelVarsLeft"),
                                              uiOutput(outputId = "mergeDblArrowSelVarsLeft")
                                       ),
                                       column(width = 5,
                                              DTOutput(outputId = "mergeVarsSelection"),
                                              tags$head(tags$style("#mergeVarsSelection {white-space: nowrap;}"))
                                       )
                                     )
                    ),
                    conditionalPanel(condition = "output.mergeVarsSelection",
                                     div(
                                       style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-top: 60px; margin-bottom: 30px",
                                       div(style="display:inline-block",
                                           shinySaveButton(id = "mergeChooseOutFile", label = "Define merged file name", title = "Define file name", icon = icon("file-import"), filetype = list(RData = "RData"), style = "color: #ffffff; background-color: #000000; border-radius: 10px"))),
                                     div(style="display: inline-block; margin-top: 5px;", htmlOutput(outputId = "mergeSyntaxHead")),
                                     div(style="display:inline-block", shinySaveButton(id = "saveMergeSyntax", label = "Save syntax", "Save syntax as...", filetype = list(R = "r"), icon = icon("download"), style = "color: #ffffff; background-color: #000000; border-radius: 5px; font-size: 80%; margin-bottom: 1px; padding: 1px; width: 85px; margin-bottom: 0px; margin-left: 25px")),
                                     div(style="display:inline-block", uiOutput(outputId = "copyMergeSyntax")),
                                     verbatimTextOutput(outputId = "mergeSyntax"),
                                     tags$head(tags$style(HTML("#mergeSyntax {background-color: white; white-space: pre-wrap;}")))
                    ),
                    conditionalPanel(condition = "output.mergeSyntax",
                                     div(style = "margin-top: 40px",
                                         textOutput(outputId = "mergeExecBtnHead")
                                     ),
                                     div(style = "margin-bottom: 30px",
                                         uiOutput(outputId = "execMergeData")
                                     )
                    ),
                    conditionalPanel(condition = "output.execMergeData != 0",
                                     verbatimTextOutput(outputId = "consoleMergeData"),
                                     tags$head(tags$style("#consoleMergeData {color:red; background-color: white; overflow-y:scroll; max-height: 500px; margin-bottom: 30px}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleMergeData"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     ),
                    ),
            ),
            tabItem(tabName = "varProperties", class = "active",
                    h1(textOutput(outputId = "h1VarProperties")),
                    htmlOutput(outputId = "varPropsIntro"),
                    div(
                      style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                      shinyFilesButton(id = "varPropsChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px; height: 33px;"),
                      div(style = "width: 30px;"),
                      conditionalPanel(condition = "input.varPropsChooseSrcFile",
                                       div(verbatimTextOutput(outputId = "varPropsSrcPathDisplay"), style = "min-width: 750px;"),
                                       tags$head(tags$style("#varPropsSrcPathDisplay {background-color: white;}"))
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px; padding-bottom:30px",
                        div(style = "display: inline-block; padding-top:30px;",
                            htmlOutput(outputId = "varPropsStudyName"),
                            htmlOutput(outputId = "varPropsStudyCycle"),
                        ),
                        div(style = "width: 135px;"),
                        div(style = "display: inline-block; padding-top:17px;",
                            htmlOutput(outputId = "varPropsRespHead"),
                            htmlOutput(outputId = "varPropsRespAvailable")
                        )
                      )
                    ),
                    div(style = "display: inline-block; padding-bottom:15px;",
                        htmlOutput(outputId = "varPropsExplText")
                    ),
                    fluidRow(
                      column(width = 6,
                             DTOutput(outputId = "varPropsAllAvailableVars"),
                             tags$head(tags$style("#varPropsAllAvailableVars {white-space: nowrap;}"))
                      ),
                      column(width = 1, align = "center",
                             br(), br(), br(), br(), br(), br(), br(), br(), br(),
                             uiOutput(outputId = "varPropsArrowSelVarsRight"),
                             uiOutput(outputId = "varPropsDblArrowSelVarsRight"),
                             br(), br(),
                             uiOutput(outputId = "varPropsArrowSelVarsLeft"),
                             uiOutput(outputId = "varPropsDblArrowSelVarsLeft")
                      ),
                      column(width = 5,
                             DTOutput(outputId = "varPropsVarsSelection"),
                             tags$head(tags$style("#varPropsVarsSelection {white-space: nowrap;}"))
                      )
                    ),
                    conditionalPanel(condition = "output.varPropsVarsSelection",
                                     fluidRow(
                                       column(width = 12,
                                              div(
                                                style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-top: 20px;",
                                                checkboxInput(inputId = "varPropsSaveOutput", label = "Save the variable dictionaries in a file", value = FALSE, width = "400px")))),
                                     conditionalPanel(condition = "output.varPropsSaveOutput",
                                                      div(
                                                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-top: 44px; margin-bottom: 25px;",
                                                        shinySaveButton(id = "varPropsChooseOutFile", label = "Define output file name", title = "Define file name", icon = icon("file-export"), filetype = list(txt = "txt"), style = "color: #ffffff; background-color: #000000; border-radius: 10px; height: 33px;"),
                                                        div(style="display:inline-block; margin-left: 15px; margin-top: 7px;",
                                                            checkboxInput(inputId = "varPropsOpenOutput", label = "Open the output when done", value = TRUE)
                                                        )
                                                      )
                                     ),
                                     div(style="display:inline-block", htmlOutput(outputId = "varPropsSyntaxHead")),
                                     div(style="display:inline-block", shinySaveButton(id = "saveVarPropsSyntax", label = "Save syntax", "Save syntax as...", filetype = list(R = "r"), icon = icon("download"), style = "color: #ffffff; background-color: #000000; border-radius: 5px; font-size: 80%; margin-bottom: 1px; padding: 1px; width: 85px; margin-bottom: 0px; margin-left: 25px")),
                                     div(style="display:inline-block", uiOutput(outputId = "copyVarPropsSyntax")),
                                     verbatimTextOutput(outputId = "varPropsSyntax"),
                                     tags$head(tags$style(HTML("#varPropsSyntax {background-color: white; white-space: pre-wrap;}"))),
                                     conditionalPanel(condition = "output.varPropsSyntax",
                                                      div(style = "margin-top: 41px",
                                                          textOutput(outputId = "varPropsExecBtnHead")
                                                      ),
                                                      div(style = "margin-bottom: 30px",
                                                          uiOutput(outputId = "execVarProps")
                                                      )
                                     ),
                                     conditionalPanel(condition = "varPropsSyntax",
                                                      verbatimTextOutput(outputId = "consoleVarProps"),
                                                      tags$head(tags$style("#consoleVarProps {color:red; background-color: white; overflow-y:scroll; max-height: 500px; margin-bottom: 30px}")),
                                                      tags$script(
                                                        '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleVarProps"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                                      ),
                                     )
                    ),
            ),
            tabItem(tabName = "dataDiag", class = "active",
                    h1(textOutput(outputId = "h1DataDiag")),
                    htmlOutput(outputId = "dataDiagIntro"),
                    div(
                      style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                      shinyFilesButton(id = "dataDiagChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px; height: 33px;"),
                      div(style = "width: 30px;"),
                      conditionalPanel(condition = "input.dataDiagChooseSrcFile",
                                       div(verbatimTextOutput(outputId = "dataDiagSrcPathDisplay"), style = "min-width: 750px;"),
                                       tags$head(tags$style("#dataDiagSrcPathDisplay {background-color: white;}"))
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px; padding-bottom:30px",
                        div(style = "display: inline-block; padding-top:30px;",
                            htmlOutput(outputId = "dataDiagStudyName"),
                            htmlOutput(outputId = "dataDiagStudyCycle"),
                        ),
                        div(style = "width: 135px;"),
                        div(style = "display: inline-block; padding-top:17px;",
                            htmlOutput(outputId = "dataDiagRespHead"),
                            htmlOutput(outputId = "dataDiagRespAvailable")
                        )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "dataDiagNoWeights"),
                             tags$head(tags$style("#dataDiagNoWeights {color: red; font-weight: bold;}"))
                      )
                    ),
                    div(style = "display: inline-block; padding-bottom:15px;",
                        htmlOutput(outputId = "dataDiagVariablesExplText")
                    ),
                    fluidRow(
                      column(width = 6, align = "center",
                             DTOutput(outputId = "dataDiagAllAvailableVars"),
                             tags$head(tags$style("#dataDiagAllAvailableVars {white-space: nowrap;}"))
                      ),
                      column(width = 6,
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "dataDiagArrowSelSplitVarsRight"),
                                      uiOutput(outputId = "dataDiagArrowSelSplitVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "dataDiagSplitVars"),
                                      tags$head(tags$style("#dataDiagSplitVars {white-space: nowrap;}")),
                                      uiOutput(outputId = "dataDiagInclMiss"),
                                      br()
                               )
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "dataDiagArrowSelAnalVarsRight"),
                                      uiOutput(outputId = "dataDiagArrowSelAnalVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "dataDiagAnalVars"),
                                      tags$head(tags$style("#dataDiagAnalVars {white-space: nowrap;}"))
                               ),
                               br()
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "dataDiagArrowSelWeightVarsRight"),
                                      uiOutput(outputId = "dataDiagArrowSelWeightVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "dataDiagWeightVar"),
                                      tags$head(tags$style("#dataDiagWeightVar {white-space: nowrap;}"))
                               )
                             ),
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "dataDiagWgtsNotWgts"),
                             tags$head(tags$style("#dataDiagWgtsNotWgts {color: red; font-weight: bold;}")),
                             br()
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(
                               style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-top: 20px; margin-bottom:25px",
                               uiOutput(outputId = "dataDiagContFreq")
                             ),
                             div(style="display:inline-block; margin-bottom: 33px;",
                                 shinySaveButton(id = "dataDiagChooseOutFile", label = "Define the output file name", title = "Define file name", icon = icon("file-export"), filetype = list(xlsx = "xlsx"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")),
                             div(style="display:inline-block; margin-left: 15px; margin-top: 8px;",
                                 div(style="display:inline-block", uiOutput(outputId = "dataDiagOpenOutput"))
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block", htmlOutput(outputId = "dataDiagSyntaxHead")),
                             div(style="display:inline-block", shinySaveButton(id = "saveDataDiagSyntax", label = "Save syntax", "Save syntax as...", filetype = list(R = "r"), icon = icon("download"), style = "color: #ffffff; background-color: #000000; border-radius: 5px; font-size: 80%; margin-bottom: 1px; padding: 1px; width: 85px; margin-bottom: 0px; margin-left: 25px")),
                             div(style="display:inline-block", uiOutput(outputId = "copyDataDiagSyntax")),
                             verbatimTextOutput(outputId = "dataDiagSyntax"),
                             tags$head(tags$style(HTML("#dataDiagSyntax {background-color: white; white-space: pre-wrap;}")))
                      )
                    ),
                    conditionalPanel(condition = "output.dataDiagSyntax",
                                     div(style = "margin-top: 30px",
                                         textOutput(outputId = "dataDiagExecBtnHead")
                                     ),
                                     div(style = "margin-bottom: 30px",
                                         uiOutput(outputId = "execDataDiag")
                                     )
                    ),
                    conditionalPanel(condition = "dataDiagSyntax",
                                     verbatimTextOutput(outputId = "consoleDataDiag"),
                                     tags$head(tags$style("#consoleDataDiag {color:red; background-color: white; overflow-y:scroll; max-height: 500px; margin-bottom: 30px}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleDataDiag"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     )
                    )
            ),
            tabItem(tabName = "recodeVars", class = "active",
                    h1(textOutput(outputId = "h1RecodeVars")),
                    htmlOutput(outputId = "recodeIntro"),
                    div(
                      style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                      shinyFilesButton(id = "recodeChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px; height: 33px;"),
                      div(style = "width: 30px;"),
                      conditionalPanel(condition = "input.recodeChooseSrcFile",
                                       div(verbatimTextOutput(outputId = "recodeSrcPathDisplay"), style = "min-width: 750px;"),
                                       tags$head(tags$style("#recodeSrcPathDisplay {background-color: white;}"))
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px; padding-bottom:30px",
                        div(style = "display: inline-block; padding-top:30px;",
                            htmlOutput(outputId = "recodeStudyName"),
                            htmlOutput(outputId = "recodeStudyCycle"),
                        ),
                        div(style = "width: 135px;"),
                        div(style = "display: inline-block; padding-top:17px;",
                            htmlOutput(outputId = "recodeRespHead"),
                            htmlOutput(outputId = "recodeRespAvailable")
                        )
                      )
                    ),
                    div(style = "display: inline-block; padding-bottom:15px;",
                        htmlOutput(outputId = "recodeVariablesExplText")
                    ),
                    fluidRow(
                      column(width = 6,
                             DTOutput(outputId = "recodeAllAvailableVars"),
                             tags$head(tags$style("#recodeAllAvailableVars {white-space: nowrap;}"))
                      ),
                      column(width = 1, align = "center",
                             br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                             uiOutput(outputId = "recodeArrowSelVarsRight"),
                             br(), br(),
                             uiOutput(outputId = "recodeArrowSelVarsLeft"),
                      ),
                      column(width = 5,
                             DTOutput(outputId = "recodeVarsSelection"),
                             tags$head(tags$style("#recodeVarsSelection {white-space: nowrap;}"))
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(
                               style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-top: 20px;",
                               htmlOutput(outputId = "recodeSchemeExpl"),
                               htmlOutput(outputId = "recodeSchemeWarn"),
                               tags$head(tags$style("#recodeSchemeWarn {color: red; font-weight: bold;}"))
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "recodeWarnSchemeIncomplete"),
                             tags$head(tags$style("#recodeWarnSchemeIncomplete {color: red; font-weight: bold;}")),
                             br(),
                             htmlOutput(outputId = "warningNotNumeric"),
                             tags$head(tags$style("#warningNotNumeric {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "warningDiffNumValuesLabels"),
                             tags$head(tags$style("#warningDiffNumValuesLabels {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "warningUniqueLabelsValues"),
                             tags$head(tags$style("#warningUniqueLabelsValues {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "warningUniqueValuesLabels"),
                             tags$head(tags$style("#warningUniqueValuesLabels {color: red; font-weight: bold;}"))
                      )
                    ),
                    fluidRow(
                      column(width = 12, align = "center",
                             DTOutput(outputId = "recodeSchemeFAC"),
                             tags$head(tags$style("#recodeSchemeFAC {white-space: nowrap;}")),
                             DTOutput(outputId = "recodeSchemeNUM"),
                             tags$head(tags$style("#recodeSchemeNUM {white-space: nowrap;}")),
                             DTOutput(outputId = "recodeSchemeCHAR"),
                             tags$head(tags$style("#recodeSchemeCHAR {white-space: nowrap;}"))
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             htmlOutput(outputId = "recodeMissings"),
                             textAreaInput(inputId = "recodeNewMissings", label = "Enter the new user-defined missing values", width = "550px", height = "175px", placeholder = 'Enter the new missing values divided by semicolons (;). For example:\n\nOmitted or invalid; Logically not applicable; Not reached\n\nThe values must be available in the newly defined levels. If no user-defined missing values are specified, the existing ones (if any) will appear as valid values in the recoded variable(s).')
                      ),
                      column(width = 6,
                             htmlOutput(outputId = "recodeMissingsWarn"),
                             tags$head(tags$style("#recodeMissingsWarn {color: red; font-weight: bold;}"))
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             checkboxInput(inputId = "recodeInNewVars", label = "Recode into new variables", value = TRUE),
                             htmlOutput(outputId = "recodeOverwriteWarn"),
                             tags$head(tags$style("#recodeOverwriteWarn {font-weight: bold;}")),
                             DTOutput(outputId = "recodeNewVarNames")
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(
                               style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                               div(style="display:inline-block; margin-top: 66px; margin-bottom: 35px;",
                                   shinySaveButton(id = "recodeChooseOutFile", label = "Define recoded output file name", title = "Define file name", icon = icon("file-export"), filetype = list(RData = "RData"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                               )
                             ))),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block", htmlOutput(outputId = "recodeSyntaxHead")),
                             div(style="display:inline-block", shinySaveButton(id = "saveRecodeSyntax", label = "Save syntax", "Save syntax as...", filetype = list(R = "r"), icon = icon("download"), style = "color: #ffffff; background-color: #000000; border-radius: 5px; font-size: 80%; margin-bottom: 1px; padding: 1px; width: 85px; margin-bottom: 0px; margin-left: 25px")),
                             div(style="display:inline-block", uiOutput(outputId = "copyRecodeSyntax")),
                             verbatimTextOutput(outputId = "recodeSyntax"),
                             tags$head(tags$style(HTML("#recodeSyntax {background-color: white; white-space: pre-wrap;}")))
                      )
                    ),
                    conditionalPanel(condition = "output.recodeSyntax",
                                     div(style = "margin-top: 30px",
                                         textOutput(outputId = "recodeExecBtnHead")
                                     ),
                                     div(style = "margin-bottom: 30px",
                                         uiOutput(outputId = "execRecode")
                                     )
                    ),
                    conditionalPanel(condition = "recodeSyntax",
                                     verbatimTextOutput(outputId = "consoleRecode"),
                                     tags$head(tags$style("#consoleRecode {color:red; background-color: white; overflow-y:scroll; max-height: 500px; margin-bottom: 30px}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleRecode"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     )
                    )
            ),
            tabItem(tabName = "selectPISACountries", class = "active",
                    h1(textOutput(outputId = "h1selectPISACountries")),
                    htmlOutput(outputId = "selectPISACountriesIntro"),
                    div(
                      style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                      shinyFilesButton(id = "selectPISACountriesChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px; height: 33px;"),
                      div(style = "width: 30px;"),
                      conditionalPanel(condition = "input.selectPISACountriesChooseSrcFile",
                                       div(verbatimTextOutput(outputId = "selectPISACountriesSrcPathDisplay"), style = "min-width: 750px;"),
                                       tags$head(tags$style("#selectPISACountriesSrcPathDisplay {background-color: white;}"))
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px; padding-bottom:30px",
                        div(style = "display: inline-block; padding-top:30px;",
                            htmlOutput(outputId = "selectPISACountriesStudyName"),
                            htmlOutput(outputId = "selectPISACountriesStudyCycle"),
                        ),
                        div(style = "width: 135px;"),
                        div(style = "display: inline-block; padding-top:17px;",
                            htmlOutput(outputId = "selectPISACountriesRespHead"),
                            htmlOutput(outputId = "selectPISACountriesRespAvailable")
                        )
                      )
                    ),
                    div(style = "display: inline-block; padding-bottom:15px;",
                        htmlOutput(outputId = "selectPISACountriesExplText")
                    ),
                    fluidRow(
                      column(width = 6, align = "center",
                             DTOutput(outputId = "selectPISACountriesAvailableCountries"),
                             tags$head(tags$style("#selectPISACountriesAvailableCountries {white-space: nowrap;}"))
                      ),
                      column(width = 6,
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "selectPISACountriesArrowSelCntRight"),
                                      uiOutput(outputId = "selectPISACountriesArrowSelCntLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "selectPISASelectedCountries"),
                                      tags$head(tags$style("#selectPISASelectedCountries {white-space: nowrap;}"))
                               )
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block; margin-top: 60px; margin-bottom: 33px;",
                                 shinySaveButton(id = "selectPISACountriesChooseOutFile", label = "Define the output file name", title = "Define file name", icon = icon("file-export"), filetype = list(RData = "RData"), style = "color: #ffffff; background-color: #000000; border-radius: 10px"))
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block", htmlOutput(outputId = "selectPISACountriesSyntaxHead")),
                             div(style="display:inline-block", shinySaveButton(id = "saveSelectPISACountriesSyntax", label = "Save syntax", "Save syntax as...", filetype = list(R = "r"), icon = icon("download"), style = "color: #ffffff; background-color: #000000; border-radius: 5px; font-size: 80%; margin-bottom: 1px; padding: 1px; width: 85px; margin-bottom: 0px; margin-left: 25px")),
                             div(style="display:inline-block", uiOutput(outputId = "copySelectPISACountriesSyntax")),
                             verbatimTextOutput(outputId = "selectPISACountriesSyntax"),
                             tags$head(tags$style(HTML("#selectPISACountriesSyntax {background-color: white; white-space: pre-wrap;}")))
                      )
                    ),
                    conditionalPanel(condition = "output.selectPISACountriesSyntax",
                                     div(style = "margin-top: 30px",
                                         textOutput(outputId = "selectPISACountriesExecBtnHead")
                                     ),
                                     div(style = "margin-bottom: 30px",
                                         uiOutput(outputId = "execSelectPISACountries")
                                     )
                    ),
                    conditionalPanel(condition = "selectPISACountriesSyntax",
                                     verbatimTextOutput(outputId = "consoleSelectPISACountries"),
                                     tags$head(tags$style("#consoleSelectPISACountries {color:red; background-color: white; overflow-y:scroll; max-height: 500px; margin-bottom: 30px}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleSelectPISACountries"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     )
                    )
            ),
            tabItem(tabName = "pctsMeans", class = "active",
                    h1(textOutput(outputId = "h1PctsMeans")),
                    htmlOutput(outputId = "pctsMeansIntro"),
                    div(
                      style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                      shinyFilesButton(id = "pctsMeansChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px; height: 33px;"),
                      div(style = "width: 30px;"),
                      conditionalPanel(condition = "input.pctsMeansChooseSrcFile",
                                       div(verbatimTextOutput(outputId = "pctsMeansSrcPathDisplay"), style = "min-width: 750px;"),
                                       tags$head(tags$style("#pctsMeansSrcPathDisplay {background-color: white;}"))
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px; padding-bottom:30px",
                        div(style = "display: inline-block; padding-top:30px;",
                            htmlOutput(outputId = "pctsMeansStudyName"),
                            htmlOutput(outputId = "pctsMeansStudyCycle")
                        ),
                        div(style = "width: 135px;"),
                        div(style = "display: inline-block; padding-top:17px;",
                            htmlOutput(outputId = "pctsMeansRespHead"),
                            htmlOutput(outputId = "pctsMeansRespAvailable"),
                        )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "pctsMeansNoWeights"),
                             tags$head(tags$style("#pctsMeansNoWeights {color: red; font-weight: bold;}"))
                      )
                    ),
                    div(style = "display: inline-block; padding-bottom:10px;",
                        htmlOutput(outputId = "pctsMeansVariablesExplText")
                    ),
                    fluidRow(
                      column(width = 6, align = "center",
                             DTOutput(outputId = "pctsMeansAllAvailableVars"),
                             tags$head(tags$style("#pctsMeansAllAvailableVars {white-space: nowrap;}"))
                      ),
                      column(width = 6,
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "pctsMeansArrowSelSplitVarsRight"),
                                      uiOutput(outputId = "pctsMeansArrowSelSplitVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "pctsMeansSplitVars"),
                                      tags$head(tags$style("#pctsMeansSplitVars {white-space: nowrap;}")),
                                      uiOutput(outputId = "pctsMeansInclMiss"),
                                      br()
                               )
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "pctsMeansArrowSelBckgVarsRight"),
                                      uiOutput(outputId = "pctsMeansArrowSelBckgVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "pctsMeansBckgVars"),
                                      tags$head(tags$style("#pctsMeansBckgVars {white-space: nowrap;}"))
                               ),
                               br()
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "pctsMeansArrowSelPVsRight"),
                                      uiOutput(outputId = "pctsMeansArrowSelPVsLeft"),
                                      uiOutput(outputId = "pctsMeansArrowSelPVsRightDisbld"),
                                      uiOutput(outputId = "pctsMeansArrowSelPVsLeftDisbld")
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "pctsMeansPVVars"),
                                      tags$head(tags$style("#pctsMeansPVVars {white-space: nowrap;}")),
                                      DTOutput(outputId = "pctsMeansPVVarsDisbld"),
                                      br()
                               )
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "pctsMeansArrowSelWeightVarsRight"),
                                      uiOutput(outputId = "pctsMeansArrowSelWeightVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "pctsMeansWeightVar"),
                                      tags$head(tags$style("#pctsMeansWeightVar {white-space: nowrap;}"))
                               )
                             ),
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "pctsMeansPVsNotPVs"),
                             tags$head(tags$style("#pctsMeansPVsNotPVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "pctsMeansSplitArePVs"),
                             tags$head(tags$style("#pctsMeansSplitArePVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "pctsMeansBckgArePVs"),
                             tags$head(tags$style("#pctsMeansBckgArePVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "pctsMeansWgtsNotWgts"),
                             tags$head(tags$style("#pctsMeansWgtsNotWgts {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "pctsMeansWarnMoreVars")
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px; padding-bottom: 30px",
                        div(style = "display: inline-block; margin-top: 30px;",
                            uiOutput(outputId = "centralTendencyType")
                        ),
                        div(style = "width: 15px;"),
                        div(style = "display: inline-block; padding-top:15px;",
                            htmlOutput(outputId = "centralTendencyTypeExpl")
                        )
                      )
                    ),
                    fluidRow(
                      column(width = 12, offset = 0, style = "margin-top: -20px;",
                             uiOutput(outputId = "pctsMeansShortcut"),
                             uiOutput(outputId = "pctsMeansGraphs"),
                             fluidRow(
                               div(
                                 style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                                 div(style = "display: inline-block; margin-left: 20px; padding-top:7px; padding-left: 7px",
                                     uiOutput(outputId = "pctsMeansGraphsPctXlabelChk")
                                 ),
                                 div(style = "width: 15px;"),
                                 div(uiOutput(outputId = "pctsMeansGraphsPctXlabelTXT"), style = "min-width: 500px;")
                               )
                             ),
                             fluidRow(
                               div(
                                 style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                                 div(style = "display: inline-block; margin-left: 20px; padding-top:7px; padding-left: 7px",
                                     uiOutput(outputId = "pctsMeansGraphsPctYlabelChk")
                                 ),
                                 div(style = "width: 15px;"),
                                 div(uiOutput(outputId = "pctsMeansGraphsPctYlabelTXT"), style = "min-width: 500px;")
                               )
                             ),
                             fluidRow(
                               div(
                                 style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                                 div(style = "display: inline-block; margin-left: 20px; padding-top:7px; padding-left: 7px",
                                     uiOutput(outputId = "pctsMeansGraphsMeanXlabelsChk")
                                 ),
                                 div(style = "width: 15px;"),
                                 div(uiOutput(outputId = "pctsMeansGraphsMeanXlabelsTXT"), style = "min-width: 500px;")
                               )
                             ),
                             fluidRow(
                               div(
                                 style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                                 div(style = "display: inline-block; margin-left: 20px; padding-top:7px; padding-bottom: 30px; padding-left: 7px",
                                     uiOutput(outputId = "pctsMeansGraphsMeanYlabelsChk")
                                 ),
                                 div(style = "width: 15px;"),
                                 div(uiOutput(outputId = "pctsMeansGraphsMeanYlabelsTXT"), style = "min-width: 500px;")
                               )
                             ),
                             fluidRow(
                               column(width = 12,
                                      htmlOutput(outputId = "warnPctsMeansCustomXlab"),
                                      tags$head(tags$style("#warnPctsMeansCustomXlab {color: red; font-weight: bold;}")),
                                      htmlOutput(outputId = "warnPctsMeansCustomYlab"),
                                      tags$head(tags$style("#warnPctsMeansCustomYlab {color: red; font-weight: bold;}"))
                               )
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(
                               style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-bottom: 25px",
                               div(style="display:inline-block",
                                   shinySaveButton(id = "pctsMeansChooseOutFile", label = "Define the output file name", title = "Define file name", icon = icon("file-export"), filetype = list(xlsx = "xlsx"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                               ),
                               div(style="display:inline-block; margin-left: 15px; margin-top: 8px;",
                                   uiOutput(outputId = "pctsMeansOpenOutput"))
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block", htmlOutput(outputId = "pctsMeansSyntaxHead")),
                             div(style="display:inline-block", shinySaveButton(id = "savePctsMeansSyntax", label = "Save syntax", "Save syntax as...", filetype = list(R = "r"), icon = icon("download"), style = "color: #ffffff; background-color: #000000; border-radius: 5px; font-size: 80%; margin-bottom: 1px; padding: 1px; width: 85px; margin-bottom: 0px; margin-left: 25px")),
                             div(style="display:inline-block", uiOutput(outputId = "copyPctsMeansSyntax")),
                             verbatimTextOutput(outputId = "pctsMeansSyntax"),
                             tags$head(tags$style(HTML("#pctsMeansSyntax {background-color: white; white-space: pre-wrap;}")))
                      )
                    ),
                    conditionalPanel(condition = "output.pctsMeansSyntax",
                                     div(style = "margin-top: 30px",
                                         textOutput(outputId = "pctsMeansExecBtnHead")
                                     ),
                                     div(style = "margin-bottom: 30px",
                                         uiOutput(outputId = "execPctsMeans")
                                     )
                    ),
                    conditionalPanel(condition = "pctsMeansSyntax",
                                     verbatimTextOutput(outputId = "consolePctsMeans"),
                                     tags$head(tags$style("#consolePctsMeans {color:red; background-color: white; overflow-y:scroll; max-height: 500px; margin-bottom: 30px}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consolePctsMeans"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     )
                    )
            ),
            tabItem(tabName = "prctls", class = "active",
                    h1(textOutput(outputId = "h1Prctls")),
                    htmlOutput(outputId = "prctlsIntro"),
                    div(
                      style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                      shinyFilesButton(id = "prctlsChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px; height: 33px;"),
                      div(style = "width: 30px;"),
                      conditionalPanel(condition = "input.prctlsChooseSrcFile",
                                       div(verbatimTextOutput(outputId = "prctlsSrcPathDisplay"), style = "min-width: 750px;"),
                                       tags$head(tags$style("#prctlsSrcPathDisplay {background-color: white;}"))
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px; padding-bottom:30px",
                        div(style = "display: inline-block; padding-top: 30px;",
                            htmlOutput(outputId = "prctlsStudyName"),
                            htmlOutput(outputId = "prctlsStudyCycle"),
                        ),
                        div(style = "width: 135px;"),
                        div(style = "display: inline-block; padding-top: 17px",
                            htmlOutput(outputId = "prctlsRespHead"),
                            htmlOutput(outputId = "prctlsRespAvailable")
                        )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "prctlsNoWeights"),
                             tags$head(tags$style("#prctlsNoWeights {color: red; font-weight: bold;}"))
                      )
                    ),
                    div(style = "display: inline-block; padding-bottom:10px;",
                        htmlOutput(outputId = "prctlsVariablesExplText")
                    ),
                    fluidRow(
                      column(width = 6, align = "center",
                             DTOutput(outputId = "prctlsAllAvailableVars"),
                             tags$head(tags$style("#prctlsAllAvailableVars {white-space: nowrap;}"))
                      ),
                      column(width = 6,
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "prctlsArrowSelSplitVarsRight"),
                                      uiOutput(outputId = "prctlsArrowSelSplitVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "prctlsSplitVars"),
                                      tags$head(tags$style("#prctlsSplitVars {white-space: nowrap;}")),
                                      uiOutput(outputId = "prctlsInclMiss"),
                                      br()
                               )
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "prctlsArrowSelBckgVarsRight"),
                                      uiOutput(outputId = "prctlsArrowSelBckgVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "prctlsBckgVars"),
                                      tags$head(tags$style("#prctlsBckgVars {white-space: nowrap;}"))
                               ),
                               br()
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "prctlsArrowSelPVsRight"),
                                      uiOutput(outputId = "prctlsArrowSelPVsLeft"),
                                      uiOutput(outputId = "prctlsArrowSelPVsRightDisbld"),
                                      uiOutput(outputId = "prctlsArrowSelPVsLeftDisbld")
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "prctlsPVVars"),
                                      tags$head(tags$style("#prctlsPVVars {white-space: nowrap;}")),
                                      DTOutput(outputId = "prctlsPVVarsDisbld"),
                                      br()
                               )
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "prctlsArrowSelWeightVarsRight"),
                                      uiOutput(outputId = "prctlsArrowSelWeightVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "prctlsWeightVar"),
                                      tags$head(tags$style("#prctlsWeightVar {white-space: nowrap;}"))
                               )
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "prctlsPVsNotPVs"),
                             tags$head(tags$style("#prctlsPVsNotPVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "prctlsSplitArePVs"),
                             tags$head(tags$style("#prctlsSplitArePVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "prctlsBckgArePVs"),
                             tags$head(tags$style("#prctlsBckgArePVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "prctlsBckgNotCnt"),
                             tags$head(tags$style("#prctlsBckgNotCnt {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "prctlsWgtsNotWgts"),
                             tags$head(tags$style("#prctlsWgtsNotWgts {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "prctlsWarnMoreVars")
                      )
                    ),
                    fluidRow(
                      div(style = "margin-top: 20px; margin-bottom: 30px; margin-left: 15px;",
                          htmlOutput(outputId = "prctlsValuesExpl")
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px; padding-bottom:30px",
                        div(style = "display: inline-block;",
                            uiOutput(outputId = "prctlsValues")
                        ),
                        div(style = "width: 15px;"),
                        div(style = "margin-top: 25px",
                            uiOutput(outputId = "prctlsValuesReset")
                        ),
                        div(style = "width: 15px;"),
                        div(style = "padding-top: 15px;",
                            htmlOutput(outputId = "prctlsNotNum"),
                            tags$head(tags$style("#prctlsNotNum {color: red; font-weight: bold;}")),
                            htmlOutput(outputId = "prctlsNotInRange"),
                            tags$head(tags$style("#prctlsNotInRange {color: red; font-weight: bold;}"))
                        )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             uiOutput(outputId = "prctlsShortcut")
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style = "margin-top: 25px",
                                 uiOutput(outputId = "prctlsGraphs")
                             )
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                        div(style = "display: inline-block; margin-left: 20px; padding-top:7px; padding-left: 7px",
                            uiOutput(outputId = "prctlsGraphsPctXlabelChk")
                        ),
                        div(style = "width: 15px;"),
                        div(uiOutput(outputId = "prctlsGraphsPctXlabelTXT"), style = "min-width: 500px;")
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                        div(style = "display: inline-block; margin-left: 20px; padding-top:7px; padding-left: 7px",
                            uiOutput(outputId = "prctlsGraphsPctYlabelChk")
                        ),
                        div(style = "width: 15px;"),
                        div(uiOutput(outputId = "prctlsGraphsPctYlabelTXT"), style = "min-width: 500px;")
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                        div(style = "display: inline-block; margin-left: 20px; padding-top:7px; padding-left: 7px",
                            uiOutput(outputId = "prctlsGraphsPrctlXlabelsChk")
                        ),
                        div(style = "width: 15px;"),
                        div(uiOutput(outputId = "prctlsGraphsPrctlXlabelsTXT"), style = "min-width: 500px;")
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                        div(style = "display: inline-block; margin-left: 20px; padding-bottom: 30px; padding-top:7px; padding-left: 7px",
                            uiOutput(outputId = "prctlsGraphsPrctlYlabelsChk")
                        ),
                        div(style = "width: 15px;"),
                        div(uiOutput(outputId = "prctlsGraphsPrctlYlabelsTXT"), style = "min-width: 500px;")
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "warnPrctlsCustomXlab"),
                             tags$head(tags$style("#warnPrctlsCustomXlab {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "warnPrctlsCustomYlab"),
                             tags$head(tags$style("#warnPrctlsCustomYlab {color: red; font-weight: bold;}"))
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(
                               style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-bottom: 25px",
                               div(
                                 style="display:inline-block",
                                 shinySaveButton(id = "prctlsChooseOutFile", label = "Define the output file name", title = "Define file name", icon = icon("file-export"), filetype = list(xlsx = "xlsx"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                               ),
                               div(style="display:inline-block; margin-left: 15px; margin-top: 8px;", uiOutput(outputId = "prctlsOpenOutput"))
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block", htmlOutput(outputId = "prctlsSyntaxHead")),
                             div(style="display:inline-block", shinySaveButton(id = "savePrctlstSyntax", label = "Save syntax", "Save syntax as...", filetype = list(R = "r"), icon = icon("download"), style = "color: #ffffff; background-color: #000000; border-radius: 5px; font-size: 80%; margin-bottom: 1px; padding: 1px; width: 85px; margin-bottom: 0px; margin-left: 25px")),
                             div(style="display:inline-block", uiOutput(outputId = "copyPrctlsSyntax")),
                             verbatimTextOutput(outputId = "prctlsSyntax"),
                             tags$head(tags$style(HTML("#prctlsSyntax {background-color: white; white-space: pre-wrap;}")))
                      )
                    ),
                    conditionalPanel(condition = "output.prctlsSyntax",
                                     div(style = "margin-top: 30px;",
                                         textOutput(outputId = "prctlsExecBtnHead")
                                     ),
                                     div(style = "margin-bottom: 30px",
                                         uiOutput(outputId = "execPrctls")
                                     )
                    ),
                    conditionalPanel(condition = "prctlsSyntax",
                                     verbatimTextOutput(outputId = "consolePrctls"),
                                     tags$head(tags$style("#consolePrctls {color:red; background-color: white; overflow-y:scroll; max-height: 500px; margin-bottom: 30px;}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consolePrctls"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     )
                    )
            ),
            tabItem(tabName = "bnchMarks", class = "active",
                    h1(textOutput(outputId = "h1Bench")),
                    htmlOutput(outputId = "benchIntro"),
                    div(
                      style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                      shinyFilesButton(id = "benchChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px; height: 33px;"),
                      div(style = "width: 30px;"),
                      conditionalPanel(condition = "input.benchChooseSrcFile",
                                       div(verbatimTextOutput(outputId = "benchSrcPathDisplay"), style = "min-width: 750px;"),
                                       tags$head(tags$style("#benchSrcPathDisplay {background-color: white;}"))
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px; padding-bottom:30px",
                        div(style = "display: inline-block; padding-top:30px;",
                            htmlOutput(outputId = "benchStudyName"),
                            htmlOutput(outputId = "benchStudyCycle")
                        ),
                        div(style = "width: 135px;"),
                        div(style = "display: inline-block; padding-top:17px;",
                            htmlOutput(outputId = "benchRespHead"),
                            htmlOutput(outputId = "benchRespAvailable")
                        )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "benchNoWeights"),
                             tags$head(tags$style("#benchNoWeights {color: red; font-weight: bold;}"))
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "noPVsInFile"),
                             tags$head(tags$style("#noPVsInFile {color: red; font-weight: bold;}"))
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px; padding-bottom:15px",
                        div(style = "display: inline-block;",
                            uiOutput(outputId = "benchType"),
                        ),
                        div(style = "width: 15px;"),
                        div(style = "display: inline-block; padding-top:5px;",
                            htmlOutput(outputId = "benchTypeExpl")
                        )
                      )
                    ),
                    div(style = "display: inline-block; padding-bottom:15px;",
                        htmlOutput(outputId = "benchVariablesExplText")
                    ),
                    fluidRow(
                      column(width = 6, align = "center",
                             DTOutput(outputId = "benchAllAvailableVars"),
                             tags$head(tags$style("#benchAllAvailableVars {white-space: nowrap;}"))
                      ),
                      column(width = 6,
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "benchArrowSelSplitVarsRight"),
                                      uiOutput(outputId = "benchArrowSelSplitVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "benchSplitVars"),
                                      tags$head(tags$style("#benchSplitVars {white-space: nowrap;}")),
                                      uiOutput(outputId = "benchInclMiss")
                               ),
                               br()
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "benchArrowSelBckgVarsRight"),
                                      uiOutput(outputId = "benchArrowSelBckgVarsLeft"),
                                      uiOutput(outputId = "benchArrowSelBckgVarsRightDisbld"),
                                      uiOutput(outputId = "benchArrowSelBckgVarsLeftDisbld"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "benchBckgVars"),
                                      tags$head(tags$style("#benchBckgVars {white-space: nowrap;}")),
                                      DTOutput(outputId = "benchBckgVarsDisbld"),
                                      br()
                               )
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "benchArrowSelPVsRight"),
                                      uiOutput(outputId = "benchArrowSelPVsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "benchPVVars"),
                                      tags$head(tags$style("#benchPVVars {white-space: nowrap;}")),
                                      br()
                               )
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "benchArrowSelWeightVarsRight"),
                                      uiOutput(outputId = "benchArrowSelWeightVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "benchWeightVar"),
                                      tags$head(tags$style("#benchWeightVar {white-space: nowrap;}"))
                               )
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "benchPVsNotPVs"),
                             tags$head(tags$style("#benchPVsNotPVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "benchSplitArePVs"),
                             tags$head(tags$style("#benchSplitArePVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "benchBckgArePVs"),
                             tags$head(tags$style("#benchBckgArePVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "benchWgtsNotWgts"),
                             tags$head(tags$style("#benchWgtsNotWgts {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "benchWarnMoreVars")
                      )
                    ),
                    fluidRow(
                      div(style = "margin-top: 20px; margin-bottom: 30px; margin-left: 15px;",
                          htmlOutput(outputId = "benchValuesExpl")
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px; padding-bottom:30px",
                        div(style = "display: inline-block;",
                            uiOutput(outputId = "benchValues"),
                        ),
                        div(style = "width: 15px;"),
                        div(style = "margin-top: 25px",
                            uiOutput(outputId = "benchValuesReset")
                        ),
                        div(style = "width: 15px;"),
                        div(style = "padding-top: 15px;",
                            htmlOutput(outputId = "benchNotNum"),
                            tags$head(tags$style("#benchNotNum {color: red; font-weight: bold;}")),
                            htmlOutput(outputId = "benchNotInRange"),
                            tags$head(tags$style("#benchNotInRange {color: red; font-weight: bold;}"))
                        )
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px; padding-bottom:25px",
                        div(style = "display: inline-block;",
                            uiOutput(outputId = "benchComputeWithin")
                        ),
                        div(style = "width: 15px;"),
                        div(style = "display: inline-block;",
                            htmlOutput(outputId = "benchComputeWithinExpl")
                        )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             uiOutput(outputId = "benchShortcut"),
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style = "margin-top: 25px",
                                 uiOutput(outputId = "benchGraphs")
                             )
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                        div(style = "display: inline-block; margin-left: 20px; padding-top:7px; padding-left: 7px",
                            uiOutput(outputId = "benchGraphsPctXlabelChk")
                        ),
                        div(style = "width: 15px;"),
                        div(uiOutput(outputId = "benchGraphsPctXlabelTXT"), style = "min-width: 500px;")
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                        div(style = "display: inline-block; margin-left: 20px; padding-top:7px; padding-left: 7px",
                            uiOutput(outputId = "benchGraphsPctYlabelChk")
                        ),
                        div(style = "width: 15px;"),
                        div(uiOutput(outputId = "benchGraphsPctYlabelTXT"), style = "min-width: 500px;")
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                        div(style = "display: inline-block; margin-left: 20px; padding-top:7px; padding-left: 7px",
                            uiOutput(outputId = "benchGraphsMeanXlabelsChk")
                        ),
                        div(style = "width: 15px;"),
                        div(uiOutput(outputId = "benchGraphsMeanXlabelsTXT"), style = "min-width: 500px;")
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                        div(style = "display: inline-block; margin-left: 20px; padding-top:7px; padding-bottom: 30px; padding-left: 7px",
                            uiOutput(outputId = "benchGraphsMeanYlabelsChk")
                        ),
                        div(style = "width: 15px;"),
                        div(uiOutput(outputId = "benchGraphsMeanYlabelsTXT"), style = "min-width: 500px;")
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(
                               style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-bottom: 25px",
                               div(style="display:inline-block",
                                   shinySaveButton(id = "benchChooseOutFile", label = "Define the output file name", title = "Define file name", icon = icon("file-export"), filetype = list(xlsx = "xlsx"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                               ),
                               div(style="display:inline-block; margin-left: 15px; margin-top: 8px;", uiOutput(outputId = "benchOpenOutput"))
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block", htmlOutput(outputId = "benchSyntaxHead")),
                             div(style="display:inline-block", shinySaveButton(id = "saveBenchSyntax", label = "Save syntax", "Save syntax as...", filetype = list(R = "r"), icon = icon("download"), style = "color: #ffffff; background-color: #000000; border-radius: 5px; font-size: 80%; margin-bottom: 1px; padding: 1px; width: 85px; margin-bottom: 0px; margin-left: 25px")),
                             div(style="display:inline-block", uiOutput(outputId = "copyBenchSyntax")),
                             verbatimTextOutput(outputId = "benchSyntax"),
                             tags$head(tags$style(HTML("#benchSyntax {background-color: white; white-space: pre-wrap;}")))
                      )
                    ),
                    conditionalPanel(condition = "output.benchSyntax",
                                     div(style = "margin-top: 30px",
                                         textOutput(outputId = "benchExecBtnHead")
                                     ),
                                     div(style = "margin-bottom: 30px",
                                         uiOutput(outputId = "execBench")
                                     )
                    ),
                    conditionalPanel(condition = "benchSyntax",
                                     verbatimTextOutput(outputId = "consoleBench"),
                                     tags$head(tags$style("#consoleBench {color:red; background-color: white; overflow-y:scroll; max-height: 500px; margin-bottom: 30px}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleBench"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     )
                    )
            ),
            tabItem(tabName = "crossTabs", class = "active",
                    h1(textOutput(outputId = "h1CrossTabs")),
                    htmlOutput(outputId = "crossTabsIntro"),
                    div(
                      style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                      shinyFilesButton(id = "crossTabsChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px; height: 33px;"),
                      div(style = "width: 30px;"),
                      conditionalPanel(condition = "input.crossTabsChooseSrcFile",
                                       div(verbatimTextOutput(outputId = "crossTabsSrcPathDisplay"), style = "min-width: 750px;"),
                                       tags$head(tags$style("#crossTabsSrcPathDisplay {background-color: white;}"))
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px; padding-bottom:30px",
                        div(style = "display: inline-block; padding-top:30px;",
                            htmlOutput(outputId = "crossTabsStudyName"),
                            htmlOutput(outputId = "crossTabsStudyCycle"),
                        ),
                        div(style = "width: 135px;"),
                        div(style = "display: inline-block; padding-top:17px;",
                            htmlOutput(outputId = "crossTabsRespHead"),
                            htmlOutput(outputId = "crossTabsRespAvailable")
                        )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "crossTabsNoWeights"),
                             tags$head(tags$style("#crossTabsNoWeights {color: red; font-weight: bold;}"))
                      )
                    ),
                    htmlOutput(outputId = "crossTabsVariablesExplText"),
                    fluidRow(
                      column(width = 6, align = "center",
                             DTOutput(outputId = "crossTabsAllAvailableVars"),
                             tags$head(tags$style("#crossTabsAllAvailableVar {white-space: nowrap;}"))
                      ),
                      column(width = 6,
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "crossTabsArrowSelSplitVarsRight"),
                                      uiOutput(outputId = "crossTabsArrowSelSplitVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "crossTabsSplitVars"),
                                      tags$head(tags$style("#crossTabsSplitVars {white-space: nowrap;}")),
                                      uiOutput(outputId = "crossTabsInclMiss"),
                                      br()
                               )
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "crossTabsArrowSelBckgRowVarRight"),
                                      uiOutput(outputId = "crossTabsArrowSelBckgRowVarLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "crossTabsBckgRowVar"),
                                      tags$head(tags$style("#crossTabsBckgRowVar {white-space: nowrap;}"))
                               ),
                               br()
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "crossTabsArrowSelBckgColVarRight"),
                                      uiOutput(outputId = "crossTabsArrowSelBckgColVarLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "crossTabsBckgColVar"),
                                      tags$head(tags$style("#crossTabsBckgColVar {white-space: nowrap;}"))
                               ),
                               br()
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "crossTabsArrowSelWeightVarRight"),
                                      uiOutput(outputId = "crossTabsArrowSelWeightVarLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "crossTabsWeightVar"),
                                      tags$head(tags$style("#crossTabsWeightVar {white-space: nowrap;}"))
                               )
                             ),
                      ),
                      fluidRow(
                        column(width = 12,
                               htmlOutput(outputId = "crossTabsSplitArePVs"),
                               tags$head(tags$style("#crossTabsSplitArePVs {color: red; font-weight: bold;}")),
                               htmlOutput(outputId = "crossTabsBckgRowIsPVs"),
                               tags$head(tags$style("#crossTabsBckgRowIsPVs {color: red; font-weight: bold;}")),
                               htmlOutput(outputId = "crossTabsBckgColIsPVs"),
                               tags$head(tags$style("#crossTabsBckgColIsPVs {color: red; font-weight: bold;}")),
                               htmlOutput(outputId = "crossTabsWgtsNotWgts"),
                               tags$head(tags$style("#crossTabsWgtsNotWgts {color: red; font-weight: bold;}"))
                        )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(
                               style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-top: 20px; padding-bottom:30px",
                               div(style = "display: inline-block;",
                                   uiOutput(outputId = "crossTabsExpCnts"),
                                   uiOutput(outputId = "crossTabsRowPcts"),
                                   uiOutput(outputId = "crossTabsColPcts"),
                                   uiOutput(outputId = "crossTabsTotPcts")
                               )
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             uiOutput(outputId = "crossTabsShortcut")
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style = "margin-top: 25px",
                                 uiOutput(outputId = "crossTabsGraphs")
                             )
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                        div(style = "display: inline-block; margin-left: 20px; padding-top:7px; padding-left: 7px",
                            uiOutput(outputId = "crossTabsGraphsPlotXlabelChk")
                        ),
                        div(style = "width: 15px;"),
                        div(uiOutput(outputId = "crossTabsGraphsPlotXlabelTXT"), style = "min-width: 500px;")
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                        div(style = "display: inline-block; margin-left: 20px; padding-top:7px; padding-bottom: 40px; padding-left: 7px",
                            uiOutput(outputId = "crossTabsGraphsPlotYlabelChk")
                        ),
                        div(style = "width: 15px;"),
                        div(uiOutput(outputId = "crossTabsGraphsPlotYlabelTXT"), style = "min-width: 500px;")
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(
                               style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-bottom: 25px",
                               div(style="display:inline-block",
                                   shinySaveButton(id = "crossTabsChooseOutFile", label = "Define the output file name", title = "Define file name", icon = icon("file-export"), filetype = list(xlsx = "xlsx"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")),
                               div(style="display:inline-block; margin-left: 15px; margin-top: 8px;",
                                   uiOutput(outputId = "crossTabsOpenOutput"))
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block", htmlOutput(outputId = "crossTabsSyntaxHead")),
                             div(style="display:inline-block", shinySaveButton(id = "saveCrosstabsSyntax", label = "Save syntax", "Save syntax as...", filetype = list(R = "r"), icon = icon("download"), style = "color: #ffffff; background-color: #000000; border-radius: 5px; font-size: 80%; margin-bottom: 1px; padding: 1px; width: 85px; margin-bottom: 0px; margin-left: 25px")),
                             div(style="display:inline-block", uiOutput(outputId = "copyCrosstabsSyntax")),
                             verbatimTextOutput(outputId = "crossTabsSyntax"),
                             tags$head(tags$style(HTML("#crossTabsSyntax {background-color: white; white-space: pre-wrap;}")))
                      )
                    ),
                    conditionalPanel(condition = "output.crossTabsSyntax",
                                     div(style = "margin-top: 30px",
                                         textOutput(outputId = "crossTabsExecBtnHead")
                                     ),
                                     div(style = "margin-bottom: 30px",
                                         uiOutput(outputId = "execCrossTabs")
                                     )
                    ),
                    conditionalPanel(condition = "crossTabsSyntax",
                                     verbatimTextOutput(outputId = "consoleCrossTabs"),
                                     tags$head(tags$style("#consoleCrossTabs {color:red; background-color: white; overflow-y:scroll; max-height: 500px; margin-bottom: 30px}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleCrossTabs"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     )
                    )
            ),
            tabItem(tabName = "corr", class = "active",
                    h1(textOutput(outputId = "h1Corr")),
                    htmlOutput(outputId = "corrIntro"),
                    div(
                      style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                      shinyFilesButton(id = "corrChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px; height: 33px;"),
                      div(style = "width: 30px;"),
                      conditionalPanel(condition = "input.corrChooseSrcFile",
                                       div(verbatimTextOutput(outputId = "corrSrcPathDisplay"), style = "min-width: 750px;"),
                                       tags$head(tags$style("#corrSrcPathDisplay {background-color: white;}"))
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px; padding-bottom:30px",
                        div(style = "display: inline-block; padding-top:30px;",
                            htmlOutput(outputId = "corrStudyName"),
                            htmlOutput(outputId = "corrStudyCycle"),
                        ),
                        div(style = "width: 135px;"),
                        div(style = "display: inline-block; padding-top:17px;",
                            htmlOutput(outputId = "corrRespHead"),
                            htmlOutput(outputId = "corrRespAvailable")
                        )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "corrNoWeights"),
                             tags$head(tags$style("#corrNoWeights {color: red; font-weight: bold;}"))
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px; padding-bottom:30px",
                        div(style = "display: inline-block;",
                            uiOutput(outputId = "corrType")
                        ),
                        div(style = "width: 15px;"),
                        div(style = "display: inline-block; margin-top:-15px;",
                            htmlOutput(outputId = "corrTypeExpl")
                        )
                      )
                    ),
                    div(style = "display: inline-block; padding-bottom:10px;",
                        htmlOutput(outputId = "corrVariablesExplText")
                    ),
                    fluidRow(
                      column(width = 6, align = "center",
                             DTOutput(outputId = "corrAllAvailableVars"),
                             tags$head(tags$style("#corrAllAvailableVars {white-space: nowrap;}"))
                      ),
                      column(width = 6,
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "corrArrowSelSplitVarsRight"),
                                      uiOutput(outputId = "corrArrowSelSplitVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "corrSplitVars"),
                                      tags$head(tags$style("#corrSplitVars {white-space: nowrap;}")),
                                      uiOutput(outputId = "corrInclMiss"),
                                      br()
                               )
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "corrArrowSelBckgVarsRight"),
                                      uiOutput(outputId = "corrArrowSelBckgVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "corrBckgVars"),
                                      tags$head(tags$style("#corrBckgVars {white-space: nowrap;}"))
                               ),
                               br()
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "corrArrowSelPVsRight"),
                                      uiOutput(outputId = "corrArrowSelPVsLeft"),
                                      uiOutput(outputId = "corrArrowSelPVsRightDisbld"),
                                      uiOutput(outputId = "corrArrowSelPVsLeftDisbld")
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "corrPVVars"),
                                      tags$head(tags$style("#corrPVVars {white-space: nowrap;}")),
                                      DTOutput(outputId = "corrPVVarsDisbld"),
                                      br()
                               )
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "corrArrowSelWeightVarsRight"),
                                      uiOutput(outputId = "corrArrowSelWeightVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "corrWeightVar"),
                                      tags$head(tags$style("#corrWeightVar {white-space: nowrap;}"))
                               )
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "corrPVsNotPVs"),
                             tags$head(tags$style("#corrPVsNotPVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "corrSplitArePVs"),
                             tags$head(tags$style("#corrSplitArePVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "corrBckgArePVs"),
                             tags$head(tags$style("#corrBckgArePVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "corrWgtsNotWgts"),
                             tags$head(tags$style("#corrWgtsNotWgts {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "corrWarnMoreVars")
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style = "margin-top: 35px",
                                 uiOutput(outputId = "corrShortcut")
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(
                               style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-top: 30px; margin-bottom: 25px",
                               div(style="display:inline-block",
                                   div(style="display:inline-block",
                                       shinySaveButton(id = "corrChooseOutFile", label = "Define the output file name", title = "Define file name", icon = icon("file-export"), filetype = list(xlsx = "xlsx"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")),
                                   div(style="display:inline-block; margin-left: 15px; margin-top: 8px;",
                                       uiOutput(outputId = "corrOpenOutput"))
                               )
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block", htmlOutput(outputId = "corrSyntaxHead")),
                             div(style="display:inline-block", shinySaveButton(id = "saveCorrSyntax", label = "Save syntax", "Save syntax as...", filetype = list(R = "r"), icon = icon("download"), style = "color: #ffffff; background-color: #000000; border-radius: 5px; font-size: 80%; margin-bottom: 1px; padding: 1px; width: 85px; margin-bottom: 0px; margin-left: 25px")),
                             div(style="display:inline-block", uiOutput(outputId = "copyCorrSyntax")),
                             verbatimTextOutput(outputId = "corrSyntax"),
                             tags$head(tags$style(HTML("#corrSyntax {background-color: white; white-space: pre-wrap;}")))
                      )
                    ),
                    conditionalPanel(condition = "output.corrSyntax",
                                     div(style = "margin-top: 30px",
                                         textOutput(outputId = "corrExecBtnHead")
                                     ),
                                     div(style = "margin-bottom: 30px",
                                         uiOutput(outputId = "execCorr")
                                     )
                    ),
                    conditionalPanel(condition = "corrSyntax",
                                     verbatimTextOutput(outputId = "consoleCorr"),
                                     tags$head(tags$style("#consoleCorr {color:red; background-color: white; overflow-y:scroll; max-height: 500px; margin-bottom: 30px}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleCorr"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     )
                    )
            ),
            tabItem(tabName = "linReg", class = "active",
                    h1(textOutput(outputId = "h1LinReg")),
                    htmlOutput(outputId = "linRegIntro"),
                    div(
                      style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                      shinyFilesButton(id = "linRegChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px; height: 33px;"),
                      div(style = "width: 30px;"),
                      conditionalPanel(condition = "input.linRegChooseSrcFile",
                                       div(verbatimTextOutput(outputId = "linRegSrcPathDisplay"), style = "min-width: 750px;"),
                                       tags$head(tags$style("#linRegSrcPathDisplay {background-color: white;}"))
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px; padding-bottom:30px",
                        div(style = "display: inline-block; padding-top:30px;",
                            htmlOutput(outputId = "linRegStudyName"),
                            htmlOutput(outputId = "linRegStudyCycle"),
                        ),
                        div(style = "width: 135px;"),
                        div(style = "display: inline-block; padding-top:17px;",
                            htmlOutput(outputId = "linRegRespHead"),
                            htmlOutput(outputId = "linRegRespAvailable")
                        )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "linRegNoWeights"),
                             tags$head(tags$style("#linRegNoWeights {color: red; font-weight: bold;}"))
                      )
                    ),
                    div(style = "display: inline-block; padding-bottom:10px;",
                        htmlOutput(outputId = "linRegVariablesExplText")
                    ),
                    fluidRow(
                      column(width = 6, align = "center",
                             DTOutput(outputId = "linRegAllAvailableVars"),
                             tags$head(tags$style("#linRegAllAvailableVars {white-space: nowrap;}"))
                      ),
                      column(width = 6,
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "linRegArrowSelSplitVarsRight"),
                                      uiOutput(outputId = "linRegArrowSelSplitVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "linRegSplitVars"),
                                      tags$head(tags$style("#linRegSplitVars {white-space: nowrap;}")),
                                      uiOutput(outputId = "linRegInclMiss")
                               )
                             ),
                             br(),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(),
                                      uiOutput(outputId = "linRegArrowSelIndepCatBckgVarsRight"),
                                      uiOutput(outputId = "linRegArrowSelIndepCatBckgVarsLeft"),
                               ),
                               column(width = 10,
                                      htmlOutput(outputId = "linRegIndepCatCaption"),
                                      tags$head(tags$style("#linRegIndepCatCaption {color: black; font-weight: bold; margin-top: -3px; padding-bottom: 9px;}")),
                                      DTOutput(outputId = "linRegIndepCatBckgVars"),
                                      tags$head(tags$style("#linRegIndepCatBckgVars {white-space: nowrap;}"))
                               )
                             ),
                             br(),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "linRegArrowSelIndepCntBckgVarsRight"),
                                      uiOutput(outputId = "linRegArrowSelIndepCntBckgVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "linRegIndepCntBckgVars"),
                                      tags$head(tags$style("#linRegIndepCntBckgVars {white-space: nowrap;}"))
                               )
                             ),
                             br(),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "linRegArrowSelIndepPVsRight"),
                                      uiOutput(outputId = "linRegArrowSelIndepPVsLeft"),
                                      br(),
                                      uiOutput(outputId = "linRegArrowSelIndepPVsRightDisbld"),
                                      uiOutput(outputId = "linRegArrowSelIndepPVsLeftDisbld")
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "linRegIndepPVVars"),
                                      tags$head(tags$style("#linRegIndepPVVars {white-space: nowrap;}")),
                                      DTOutput(outputId = "linRegIndepPVVarsDisbld")
                               )
                             ),
                             fluidRow(
                               column(width = 2),
                               column(width = 9,
                                      uiOutput(outputId = "linRegChooseDepType")
                               )
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "linRegArrowSelDepBckgVarsRight"),
                                      uiOutput(outputId = "linRegArrowSelDepBckgVarsLeft"),
                                      br(),
                                      uiOutput(outputId = "linRegArrowSelDepPVsRight"),
                                      uiOutput(outputId = "linRegArrowSelDepPVsLeft"),
                                      br(),
                                      uiOutput(outputId = "linRegArrowSelDepPVsRightDisbld"),
                                      uiOutput(outputId = "linRegArrowSelDepPVsLeftDisbld")
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "linRegDepBckgVars"),
                                      tags$head(tags$style("#linRegDepBckgVars {white-space: nowrap;}")),
                                      DTOutput(outputId = "linRegDepPVVars"),
                                      tags$head(tags$style("#linRegDepPVVars {white-space: nowrap;}")),
                                      DTOutput(outputId = "linRegDepPVVarsDisbld")
                               )
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br()
                               ),
                               column(width = 10, align = "center",
                                      DTOutput(outputId = "linRegInteractions"),
                                      tags$head(tags$style("#linRegInteractions {white-space: nowrap;}"))
                               )
                             ),
                             br(), br(),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "linRegArrowSelWeightVarsRight"),
                                      uiOutput(outputId = "linRegArrowSelWeightVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "linRegWeightVar"),
                                      tags$head(tags$style("#linRegWeightVar {white-space: nowrap;}"))
                               )
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "linRegSplitArePVs"),
                             tags$head(tags$style("#linRegSplitArePVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "linRegBckgCatArePVs"),
                             tags$head(tags$style("#linRegBckgCatArePVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "linRegBckgCntArePVs"),
                             tags$head(tags$style("#linRegBckgCntArePVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "linRegIndepPVsAreBckg"),
                             tags$head(tags$style("#linRegIndepPVsAreBckg {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "linRegDepBckgArePVs"),
                             tags$head(tags$style("#linRegDepBckgArePVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "linRegDepPVsAreBckg"),
                             tags$head(tags$style("#linRegDepPVsAreBckg {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "linRegWgtsNotWgts"),
                             tags$head(tags$style("#linRegWgtsNotWgts {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "linRegWarnMoreVars")
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style = "margin-top: 40px",
                                 uiOutput(outputId = "linRegStandardize")
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             div(style = "margin-top: 25px; padding-bottom: 44px;",
                                 uiOutput(outputId = "linRegShortcut")
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(
                               style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-bottom: 25px",
                               div(style="display:inline-block",
                                   shinySaveButton(id = "linRegChooseOutFile", label = "Define the output file name", title = "Define file name", icon = icon("file-export"), filetype = list(xlsx = "xlsx"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")),
                               div(style="display:inline-block; margin-left: 15px; margin-top: 8px;", uiOutput(outputId = "linRegOpenOutput"))
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block", htmlOutput(outputId = "linRegSyntaxHead")),
                             div(style="display:inline-block", shinySaveButton(id = "saveLinRegSyntax", label = "Save syntax", "Save syntax as...", filetype = list(R = "r"), icon = icon("download"), style = "color: #ffffff; background-color: #000000; border-radius: 5px; font-size: 80%; margin-bottom: 1px; padding: 1px; width: 85px; margin-bottom: 0px; margin-left: 25px")),
                             div(style="display:inline-block", uiOutput(outputId = "copyLinRegSyntax")),
                             verbatimTextOutput(outputId = "linRegSyntax"),
                             tags$head(tags$style(HTML("#linRegSyntax {background-color: white; white-space: pre-wrap;}")))
                      )
                    ),
                    conditionalPanel(condition = "output.linRegSyntax",
                                     div(style = "margin-top: 30px",
                                         textOutput(outputId = "linRegExecBtnHead")
                                     ),
                                     div(style = "margin-bottom: 30px",
                                         uiOutput(outputId = "execLinReg"),
                                     )
                    ),
                    conditionalPanel(condition = "linRegSyntax",
                                     verbatimTextOutput(outputId = "consoleLinReg"),
                                     tags$head(tags$style("#consoleLinReg {color:red; background-color: white; overflow-y:scroll; max-height: 500px; ; margin-bottom: 30px}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleLinReg"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     )
                    )
            ),
            tabItem(tabName = "binLogReg", class = "active",
                    h1(textOutput(outputId = "h1binLogReg")),
                    htmlOutput(outputId = "binLogRegIntro"),
                    div(
                      style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                      shinyFilesButton(id = "binLogRegChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px; height: 33px;"),
                      div(style = "width: 30px;"),
                      conditionalPanel(condition = "input.binLogRegChooseSrcFile",
                                       div(verbatimTextOutput(outputId = "binLogRegSrcPathDisplay"), style = "min-width: 750px;"),
                                       tags$head(tags$style("#binLogRegSrcPathDisplay {background-color: white;}"))
                      )
                    ),
                    fluidRow(
                      div(
                        style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-left:15px; padding-bottom:30px",
                        div(style = "display: inline-block; padding-top:30px;",
                            htmlOutput(outputId = "binLogRegStudyName"),
                            htmlOutput(outputId = "binLogRegStudyCycle"),
                        ),
                        div(style = "width: 135px;"),
                        div(style = "display: inline-block; padding-top:17px;",
                            htmlOutput(outputId = "binLogRegRespHead"),
                            htmlOutput(outputId = "binLogRegRespAvailable")
                        )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "binLogRegNoWeights"),
                             tags$head(tags$style("#binLogRegNoWeights {color: red; font-weight: bold;}"))
                      )
                    ),
                    div(style = "display: inline-block; padding-bottom:10px;",
                        htmlOutput(outputId = "binLogRegVariablesExplText")
                    ),
                    fluidRow(
                      column(width = 6, align = "center",
                             DTOutput(outputId = "binLogRegAllAvailableVars"),
                             tags$head(tags$style("#binLogRegAllAvailableVars {white-space: nowrap;}"))
                      ),
                      column(width = 6,
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "binLogRegArrowSelSplitVarsRight"),
                                      uiOutput(outputId = "binLogRegArrowSelSplitVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "binLogRegSplitVars"),
                                      tags$head(tags$style("#binLogRegSplitVars {white-space: nowrap;}")),
                                      uiOutput(outputId = "binLogRegInclMiss")
                               )
                             ),
                             br(),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(),
                                      uiOutput(outputId = "binLogRegArrowSelIndepCatBckgVarsRight"),
                                      uiOutput(outputId = "binLogRegArrowSelIndepCatBckgVarsLeft"),
                               ),
                               column(width = 10,
                                      htmlOutput(outputId = "binLogRegIndepCatCaption"),
                                      tags$head(tags$style("#binLogRegIndepCatCaption {color: black; font-weight: bold; margin-top: -3px; padding-bottom: 9px;}")),
                                      DTOutput(outputId = "binLogRegIndepCatBckgVars"),
                                      tags$head(tags$style("#binLogRegIndepCatBckgVars {white-space: nowrap;}"))
                               )
                             ),
                             br(),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "binLogRegArrowSelIndepCntBckgVarsRight"),
                                      uiOutput(outputId = "binLogRegArrowSelIndepCntBckgVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "binLogRegIndepCntBckgVars"),
                                      tags$head(tags$style("#binLogRegIndepCntBckgVars {white-space: nowrap;}"))
                               )
                             ),
                             br(),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "binLogRegArrowSelIndepPVsRight"),
                                      uiOutput(outputId = "binLogRegArrowSelIndepPVsLeft"),
                                      br(),
                                      uiOutput(outputId = "binLogRegArrowSelIndepPVsRightDisbld"),
                                      uiOutput(outputId = "binLogRegArrowSelIndepPVsLeftDisbld")
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "binLogRegIndepPVVars"),
                                      tags$head(tags$style("#binLogRegIndepPVVars {white-space: nowrap;}")),
                                      DTOutput(outputId = "binLogRegIndepPVVarsDisbld")
                               )
                             ),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "binLogRegArrowSelDepBinVarsRight"),
                                      uiOutput(outputId = "binLogRegArrowSelDepBinVarsLeft")
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "binLogRegDepBinVars"),
                                      tags$head(tags$style("#binLogRegDepBinVars {white-space: nowrap;}"))
                               )
                             ),
                             br(),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br()
                               ),
                               column(width = 10, align = "center",
                                      DTOutput(outputId = "binLogRegInteractions"),
                                      tags$head(tags$style("#binLogRegInteractions {white-space: nowrap;}"))
                               )
                             ),
                             br(), br(),
                             fluidRow(
                               column(width = 2, align = "center",
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "binLogRegArrowSelWeightVarsRight"),
                                      uiOutput(outputId = "binLogRegArrowSelWeightVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "binLogRegWeightVar"),
                                      tags$head(tags$style("#binLogRegWeightVar {white-space: nowrap;}"))
                               )
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "binLogRegSplitArePVs"),
                             tags$head(tags$style("#binLogRegSplitArePVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "binLogRegBckgCatArePVs"),
                             tags$head(tags$style("#binLogRegBckgCatArePVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "binLogRegBckgCntArePVs"),
                             tags$head(tags$style("#binLogRegBckgCntArePVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "binLogRegIndepPVsAreBckg"),
                             tags$head(tags$style("#binLogRegIndepPVsAreBckg {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "binLogRegDepBinArePVs"),
                             tags$head(tags$style("#binLogRegDepBinArePVs {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "binLogRegDepNotBin"),
                             tags$head(tags$style("#binLogRegDepNotBin {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "binLogRegWgtsNotWgts"),
                             tags$head(tags$style("#binLogRegWgtsNotWgts {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "binLogRegWarnMoreVars")
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style = "margin-top: 40px",
                                 uiOutput(outputId = "binLogRegWgtNorm")
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style = "margin-top: 25px",
                                 uiOutput(outputId = "binLogRegStandardize")
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style = "margin-top: 25px; margin-bottom: 77px",
                                 uiOutput(outputId = "binLogRegShortcut")
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(
                               style = "display:-webkit-flex; display:-ms-flexbox; display:flex; margin-bottom: 25px",
                               div(style="display:inline-block",
                                   shinySaveButton(id = "binLogRegChooseOutFile", label = "Define the output file name", title = "Define file name", icon = icon("file-export"), filetype = list(xlsx = "xlsx"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")),
                               div(style="display:inline-block; margin-left: 15px; margin-top: 8px;",
                                   uiOutput(outputId = "binLogRegOpenOutput"))
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block", htmlOutput(outputId = "binLogRegSyntaxHead")),
                             div(style="display:inline-block", shinySaveButton(id = "saveBinLogRegSyntax", label = "Save syntax", "Save syntax as...", filetype = list(R = "r"), icon = icon("download"), style = "color: #ffffff; background-color: #000000; border-radius: 5px; font-size: 80%; margin-bottom: 1px; padding: 1px; width: 85px; margin-bottom: 0px; margin-left: 25px")),
                             div(style="display:inline-block", uiOutput(outputId = "copyBinLogRegSyntax")),
                             verbatimTextOutput(outputId = "binLogRegSyntax"),
                             tags$head(tags$style(HTML("#binLogRegSyntax {background-color: white; white-space: pre-wrap;}")))
                      )
                    ),
                    conditionalPanel(condition = "output.binLogRegSyntax",
                                     div(style = "margin-top: 30px",
                                         textOutput(outputId = "binLogRegExecBtnHead")
                                     ),
                                     div(style = "margin-bottom: 30px",
                                         uiOutput(outputId = "execBinLogReg")
                                     )
                    ),
                    conditionalPanel(condition = "binLogRegSyntax",
                                     verbatimTextOutput(outputId = "consoleBinLogReg"),
                                     tags$head(tags$style("#consoleBinLogReg {color:red; background-color: white; overflow-y:scroll; max-height: 500px; margin-bottom: 30px}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleBinLogReg"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     )
                    )
            )
          ),
          tags$footer(HTML("R Analyzer for Large-Scale Assessments by INERI.org - &#127279; 2021"), align = "center", style = "
position: fixed;
left: 0px;
bottom: 0px;
width: 100%;
height: 22px;
color: white;
font-size: 14px;
padding-top: 1px;
padding-left: 260px; /*Add left padding equal to the width of the sidebar (260px) to make the footer text centered to the dashboard body*/
background-color: #000000;
z-index: 1000;")
        )
      )
    )
  )
)
