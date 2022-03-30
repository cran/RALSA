#$ Load required packages silently
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
  inlineCSS(load.app.CSS.screen),
  div(
    id = "loading-content", class = "center",
    h1(HTML("<br/><br/><br/><br/><br/><blink>Loading...</blink><br/><br/><br/><br/>")),
    img(src = 'RALSA_Logo.png', style = "width: 452px; height: 170px;"),
    h1(HTML("<br/><br/><br/>Brought to you by the<br/><br/><strong>International Educational Research and Evaluation Institute</strong><br/><br/>"), tags$a(href="http://www.ralsa.ineri.org/", "(http://www.ineri.org/)", target = "_blank"))
  ),
  hidden(
    div(
      id = "app-content",
      dashboardPage(
        title = "RALSA",
        dashboardHeader(
          title =
            tags$a(img(src = "RALSA_Logo.png", height = "60px"), href = "http://ralsa.ineri.org/", target = "_blank"), titleWidth = 260,
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
background-color: #000000;")
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
                                     menuItem(text = "Help", icon = icon("question-circle"), tabName = "ralsaHelp")
                         ),
                         sidebarMenu(id = "exit",
                                     menuItem(text = "Exit", icon = icon("running"), tabName = "exitUI")
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
            tabItem(tabName = "ralsaHelp", class = "active",
                    h1(textOutput(outputId = "helpHeading")),
                    htmlOutput(outputId = "helpOnRALSAWebsite"),
                    htmlOutput(outputId = "helpRALSAWebsiteLinks")
            ),
            tabItem(tabName = "exitUI", class = "active",
                    h1(textOutput(outputId = "exitHeading")),
                    extendShinyjs(text = jscode.close.RALSA.GUI, functions = c("closeWindow")),
                    actionButton(inputId = "closeGUI", label = "Exit", icon = icon("running"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
            ),
            tabItem(tabName = "convertData", class = "active",
                    h1(textOutput(outputId = "h1ConvertData")),
                    htmlOutput(outputId = "convertIntro"),
                    fluidRow(
                      column(width = 2, shinyDirButton(id = "convertChooseSrcDir", label = "Choose source folder", title = "Navigate and select a folder", icon = icon("folder-open"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                      ),
                      column(width = 9, offset = 1,
                             conditionalPanel(condition = "input.convertChooseSrcDir",
                                              verbatimTextOutput(outputId = "convertSrcPathDisplay"),
                                              tags$head(tags$style("#convertSrcPathDisplay {background-color: white;}"))
                             )
                      )
                    ),
                    fluidRow(column(width = 12,
                                    htmlOutput(outputId = "convertIEAStudyName"),
                                    htmlOutput(outputId = "convertIEAStudyCycle"),
                                    br()
                    )),
                    htmlOutput(outputId = "convertAvailableIEACntsText"),
                    conditionalPanel(condition = "output.convertIEAStudyName && output.convertIEAStudyCycle",
                                     fluidRow(
                                       br(),
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
                                       ),
                                     )),
                    fluidRow(column(width = 12,
                                    htmlOutput(outputId = "convertPISA2015PlusStudyName"),
                                    htmlOutput(outputId = "convertPISA2015PlusStudyCycle"),
                                    br()
                    )),
                    htmlOutput(outputId = "convertAvailablePISA2015PlusFilesText"),
                    fluidRow(
                      column(width = 2),
                      column(width = 8,
                             DTOutput(outputId = "convertPISA2015PlusFiles"),
                             tags$head(tags$style("#convertPISA2015PlusFiles {white-space: nowrap;}"))
                      ),
                      column(width = 2)
                    ),
                    fluidRow(column(width = 12,
                                    htmlOutput(outputId = "convertPISApre2015StudyName"),
                                    htmlOutput(outputId = "convertPISApre2015StudyCycle"),
                                    br()
                    )),
                    htmlOutput(outputId = "convertAvailablePISApre2015FilesText"),
                    fluidRow(
                      column(width = 2),
                      column(width = 8,
                             DTOutput(outputId = "convertPISApre2015Files"),
                             tags$head(tags$style("#convertPISApre2015Files {white-space: nowrap;}"))
                      ),
                      column(width = 2)
                    ),
                    fluidRow(column(width = 12,
                                    htmlOutput(outputId = "convertPISADev2019PlusStudyName"),
                                    htmlOutput(outputId = "convertPISADev2019PlusStudyCycle"),
                                    br()
                    )),
                    htmlOutput(outputId = "convertAvailablePISADev2019PlusFilesText"),
                    fluidRow(
                      column(width = 2),
                      column(width = 8,
                             DTOutput(outputId = "convertPISADev2019PlusFiles"),
                             tags$head(tags$style("#convertPISADev2019PlusFiles {white-space: nowrap;}"))
                      ),
                      column(width = 2)
                    ),
                    fluidRow(
                      br(), br(),
                      column(width = 12,
                             conditionalPanel(condition = "output.convertSelectionIEA || output.convertPISA2015PlusFiles || output.convertPISApre2015Files || output.convertPISADev2019PlusFiles",
                                              fluidRow(
                                                column(width = 4,
                                                       checkboxInput(inputId = "convertMissToNA", label = "Convert user-defined missings to NA", value = FALSE)
                                                ),
                                                br(), br(),
                                                column(width = 2,
                                                       shinyDirButton(id = "convertChooseOutDir", label = "Choose destination folder", title = "Navigate and select a folder", icon = icon("folder-open"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                                                ),
                                                column(width = 9, offset = 1,
                                                       verbatimTextOutput(outputId = "convertOutPathDisplay"),
                                                       tags$head(tags$style("#convertOutPathDisplay {background-color: white;}"))
                                                )
                                              ),
                                              br(), br(),
                                              conditionalPanel(condition = "intput.convertChooseOutDir",
                                                               textOutput(outputId = "convertSyntaxHead"),
                                                               verbatimTextOutput(outputId = "convertSyntax"),
                                                               tags$head(tags$style(HTML("#convertSyntax {background-color: white; white-space: pre-wrap;}")))
                                              ),
                                              br(), br(),
                                              conditionalPanel(condition = "output.convertSyntax",
                                                               textOutput(outputId = "convertExecBtnHead"),
                                                               uiOutput(outputId = "execConvertData")
                                              ),
                                              br(),
                                              conditionalPanel(condition = "output.execConvertData != 0",
                                                               verbatimTextOutput(outputId = "consoleConvertData"),
                                                               tags$head(tags$style("#consoleConvertData {color:red; background-color: white; overflow-y:scroll; max-height: 500px;}")),
                                                               tags$script(
                                                                 '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleConvertData"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                                               ),
                                                               br()
                                              ),
                                              br(), br()
                             )
                      )),
            ),
            tabItem(tabName = "mergeData", class = "active",
                    h1(textOutput(outputId = "h1MergeData")),
                    htmlOutput(outputId = "mergeIntro"),
                    fluidRow(
                      column(width = 2, shinyDirButton(id = "mergeChooseSrcDir", label = "Choose source folder", title = "Navigate and select a folder", icon = icon("folder-open"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                      ),
                      column(width = 9, offset = 1,
                             conditionalPanel(condition = "input.mergeChooseSrcDir",
                                              verbatimTextOutput(outputId = "mergeSrcPathDisplay"),
                                              tags$head(tags$style("#mergeSrcPathDisplay {background-color: white;}"))
                             )
                      ),
                    ),
                    fluidRow(column(width = 12,
                                    htmlOutput(outputId = "mergeIEAStudyName"),
                                    htmlOutput(outputId = "mergeIEAStudyCycle"),
                                    br()
                    )),
                    htmlOutput(outputId = "mergeAvailableIEACntsText"),
                    conditionalPanel(condition = "output.mergeIEAStudyName && output.mergeIEAStudyCycle",
                                     fluidRow(
                                       br(),
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
                                       br(), br(),
                                       column(width = 12, htmlOutput(outputId = "mergeAvailRespText"))
                                     ),
                                     fluidRow(
                                       column(width = 4,
                                              br(),
                                              uiOutput(outputId = "mergeAvailRespCheckboxes")
                                       ),
                                       column(width = 8,
                                              br(), br(),
                                              uiOutput(outputId = "warnNoSuchCombination"),
                                              tags$head(tags$style("#warnNoSuchCombination {color: red; font-weight: bold;}"))
                                       )
                                     ),
                                     fluidRow(
                                       br(),
                                       column(width = 12, htmlOutput(outputId = "mergeAvailVarsText")),
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
                                     br(), br(), br(),
                                     shinySaveButton(id = "mergeChooseOutFile", label = "Define merged file name", title = "Define file name", icon = icon("file-import"), filetype = list(RData = "RData"), style = "color: #ffffff; background-color: #000000; border-radius: 10px"),
                                     br(), br(),
                                     htmlOutput(outputId = "mergeSyntaxHead"),
                                     verbatimTextOutput(outputId = "mergeSyntax"),
                                     tags$head(tags$style(HTML("#mergeSyntax {background-color: white; white-space: pre-wrap;}"))),
                                     br()
                    ),
                    br(), br(),
                    conditionalPanel(condition = "output.mergeSyntax",
                                     textOutput(outputId = "mergeExecBtnHead"),
                                     uiOutput(outputId = "execMergeData")
                    ), br(), br(),
                    br(),
                    conditionalPanel(condition = "output.execMergeData != 0",
                                     verbatimTextOutput(outputId = "consoleMergeData"),
                                     tags$head(tags$style("#consoleMergeData {color:red; background-color: white; overflow-y:scroll; max-height: 500px;}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleMergeData"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     ),
                                     br()
                    ),
                    br(), br()
            ),
            tabItem(tabName = "varProperties", class = "active",
                    h1(textOutput(outputId = "h1VarProperties")),
                    htmlOutput(outputId = "varPropsIntro"),
                    fluidRow(
                      column(width = 2, shinyFilesButton(id = "varPropsChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                      ),
                      column(width = 9, offset = 1,
                             conditionalPanel(condition = "input.varPropsChooseSrcFile",
                                              verbatimTextOutput(outputId = "varPropsSrcPathDisplay"),
                                              tags$head(tags$style("#varPropsSrcPathDisplay {background-color: white;}"))
                             )
                      )
                    ),
                    fluidRow(
                      br(), br(),
                      column(width = 2,
                             htmlOutput(outputId = "varPropsStudyName"),
                             htmlOutput(outputId = "varPropsStudyCycle"),
                      ),
                      column(width = 10,
                             htmlOutput(outputId = "varPropsRespHead"),
                             htmlOutput(outputId = "varPropsRespAvailable"),
                             br(), br()
                      )
                    ),
                    htmlOutput(outputId = "varPropsExplText"),
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
                      ),
                      br(), br()
                    ),
                    conditionalPanel(condition = "output.varPropsVarsSelection",
                                     br(), br(), br(),
                                     checkboxInput(inputId = "varPropsSaveOutput", label = "Save the variable dictionaries in a file", value = FALSE, width = "400px"),
                                     checkboxInput(inputId = "varPropsOpenOutput", label = "Open the variable dictionaries file when finished", value = FALSE),
                                     conditionalPanel(condition = "output.varPropsSaveOutput",
                                                      fluidRow(
                                                        column(width = 2,
                                                               shinySaveButton(id = "varPropsChooseOutFile", label = "Define output file name", title = "Define file name", icon = icon("file-export"), filetype = list(txt = "txt"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                                                        ),
                                                        column(width = 9, offset = 1,
                                                               verbatimTextOutput(outputId = "varPropsOutPathDisplay"),
                                                               tags$head(tags$style("#varPropsOutPathDisplay {background-color: white;}"))
                                                        )
                                                      ),
                                                      br(), br()
                                     ),
                                     htmlOutput(outputId = "varPropsSyntaxHead"),
                                     verbatimTextOutput(outputId = "varPropsSyntax"),
                                     tags$head(tags$style(HTML("#varPropsSyntax {background-color: white; white-space: pre-wrap;}"))),
                                     br(),
                                     conditionalPanel(condition = "output.varPropsSyntax",
                                                      textOutput(outputId = "varPropsExecBtnHead"),
                                                      uiOutput(outputId = "execVarProps")
                                     ), br(), br(),
                                     conditionalPanel(condition = "varPropsSyntax",
                                                      verbatimTextOutput(outputId = "consoleVarProps"),
                                                      tags$head(tags$style("#consoleVarProps {color:red; background-color: white; overflow-y:scroll; max-height: 500px;}")),
                                                      tags$script(
                                                        '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleVarProps"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                                      ),
                                                      br()
                                     )
                    ),
            ),
            tabItem(tabName = "dataDiag", class = "active",
                    h1(textOutput(outputId = "h1DataDiag")),
                    htmlOutput(outputId = "dataDiagIntro"),
                    fluidRow(
                      column(width = 2, shinyFilesButton(id = "dataDiagChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                      ),
                      column(width = 9, offset = 1,
                             conditionalPanel(condition = "input.dataDiagChooseSrcFile",
                                              verbatimTextOutput(outputId = "dataDiagSrcPathDisplay"),
                                              tags$head(tags$style("#dataDiagSrcPathDisplay {background-color: white;}"))
                             )
                      )
                    ),
                    fluidRow(
                      br(), br(),
                      column(width = 2,
                             htmlOutput(outputId = "dataDiagStudyName"),
                             htmlOutput(outputId = "dataDiagStudyCycle"),
                      ),
                      column(width = 10,
                             htmlOutput(outputId = "dataDiagRespHead"),
                             htmlOutput(outputId = "dataDiagRespAvailable"),
                             br(), br()
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "dataDiagNoWeights"),
                             tags$head(tags$style("#dataDiagNoWeights {color: red; font-weight: bold;}"))
                      )
                    ),
                    htmlOutput(outputId = "dataDiagVariablesExplText"),
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
                                      tags$head(tags$style("#dataDiagWeightVar {white-space: nowrap;}")),
                                      br(), br()
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
                             uiOutput(outputId = "dataDiagContFreq"),
                             br(), br(),
                             div(style="display:inline-block", shinySaveButton(id = "dataDiagChooseOutFile", label = "Define the output file name", title = "Define file name", icon = icon("file-export"), filetype = list(xlsx = "xlsx"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")),
                             div(style="display:inline-block", uiOutput(outputId = "dataDiagOpenOutput"))
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "dataDiagSyntaxHead"),
                             verbatimTextOutput(outputId = "dataDiagSyntax"),
                             tags$head(tags$style(HTML("#dataDiagSyntax {background-color: white; white-space: pre-wrap;}")))
                      ),
                      br(), br(), br(), br(), br(), br()
                    ),
                    conditionalPanel(condition = "output.dataDiagSyntax",
                                     textOutput(outputId = "dataDiagExecBtnHead"),
                                     uiOutput(outputId = "execDataDiag"),
                                     br(), br(), br()
                    ),
                    conditionalPanel(condition = "dataDiagSyntax",
                                     verbatimTextOutput(outputId = "consoleDataDiag"),
                                     tags$head(tags$style("#consoleDataDiag {color:red; background-color: white; overflow-y:scroll; max-height: 500px;}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleDataDiag"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     ),
                                     br()
                    )
            ),
            tabItem(tabName = "recodeVars", class = "active",
                    h1(textOutput(outputId = "h1RecodeVars")),
                    htmlOutput(outputId = "recodeIntro"),
                    fluidRow(
                      column(width = 2, shinyFilesButton(id = "recodeChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                      ),
                      column(width = 9, offset = 1,
                             conditionalPanel(condition = "input.recodeChooseSrcFile",
                                              verbatimTextOutput(outputId = "recodeSrcPathDisplay"),
                                              tags$head(tags$style("#recodeSrcPathDisplay {background-color: white;}"))
                             )
                      )
                    ),
                    fluidRow(
                      br(), br(),
                      column(width = 2,
                             htmlOutput(outputId = "recodeStudyName"),
                             htmlOutput(outputId = "recodeStudyCycle"),
                      ),
                      column(width = 10,
                             htmlOutput(outputId = "recodeRespHead"),
                             htmlOutput(outputId = "recodeRespAvailable"),
                             br(), br()
                      )
                    ),
                    htmlOutput(outputId = "recodeVariablesExplText"),
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
                      ),
                      br(), br()
                    ),
                    fluidRow(
                      br(), br(),
                      column(width = 12,
                             htmlOutput(outputId = "recodeSchemeExpl"),
                             htmlOutput(outputId = "recodeSchemeWarn"),
                             tags$head(tags$style("#recodeSchemeWarn {color: red; font-weight: bold;}"))
                      )
                    ),
                    fluidRow(
                      br(), br(),
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
                             tags$head(tags$style("#recodeSchemeCHAR {white-space: nowrap;}")),
                             br()
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             htmlOutput(outputId = "recodeMissings"),
                             textAreaInput(inputId = "recodeNewMissings", label = "Enter the new user-defined missing values", width = "550px", height = "175px", placeholder = 'Enter the new missing values divided by commas. For example:\n\nOmitted or invalid, Logically not applicable, Not reached\n\nThe values must be available in the newly defined levels. If no user-defined missing values are specified, the existing ones (if any) will appear as valid values in the recoded variable(s).')
                      ),
                      column(width = 6,
                             htmlOutput(outputId = "recodeMissingsWarn"),
                             tags$head(tags$style("#recodeMissingsWarn {color: red; font-weight: bold;}"))
                      ),
                      br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             checkboxInput(inputId = "recodeInNewVars", label = "Recode into new variables", value = TRUE),
                             htmlOutput(outputId = "recodeOverwriteWarn"),
                             tags$head(tags$style("#recodeOverwriteWarn {font-weight: bold;}")),
                             DTOutput(outputId = "recodeNewVarNames", width = "500px")
                      ),
                      br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             br(), br(),
                             htmlOutput(outputId = "recodeVarLabExpl"),
                             DTOutput(outputId = "recodeNewVarLabels")
                      ),
                      br(), br(), br()
                    ),
                    fluidRow(
                      br(), br(),
                      column(width = 6,
                             shinySaveButton(id = "recodeChooseOutFile", label = "Define recoded output file name", title = "Define file name", icon = icon("file-export"), filetype = list(RData = "RData"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "recodeSyntaxHead"),
                             verbatimTextOutput(outputId = "recodeSyntax"),
                             tags$head(tags$style(HTML("#recodeSyntax {background-color: white; white-space: pre-wrap;}")))
                      ),
                      br(), br(), br(), br(), br(), br()
                    ),
                    conditionalPanel(condition = "output.recodeSyntax",
                                     textOutput(outputId = "recodeExecBtnHead"),
                                     uiOutput(outputId = "execRecode"),
                                     br(), br(), br()
                    ),
                    conditionalPanel(condition = "recodeSyntax",
                                     verbatimTextOutput(outputId = "consoleRecode"),
                                     tags$head(tags$style("#consoleRecode {color:red; background-color: white; overflow-y:scroll; max-height: 500px;}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleRecode"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     ),
                                     br()
                    )
            ),
            tabItem(tabName = "selectPISACountries", class = "active",
                    h1(textOutput(outputId = "h1selectPISACountries")),
                    htmlOutput(outputId = "selectPISACountriesIntro"),
                    fluidRow(
                      column(width = 2, shinyFilesButton(id = "selectPISACountriesChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                      ),
                      column(width = 9, offset = 1,
                             conditionalPanel(condition = "input.selectPISACountriesChooseSrcFile",
                                              verbatimTextOutput(outputId = "selectPISACountriesSrcPathDisplay"),
                                              tags$head(tags$style("#selectPISACountriesSrcPathDisplay {background-color: white;}"))
                             )
                      )
                    ),
                    fluidRow(
                      br(), br(),
                      column(width = 2,
                             htmlOutput(outputId = "selectPISACountriesStudyName"),
                             htmlOutput(outputId = "selectPISACountriesStudyCycle"),
                      ),
                      column(width = 10,
                             htmlOutput(outputId = "selectPISACountriesRespHead"),
                             htmlOutput(outputId = "selectPISACountriesRespAvailable"),
                             br(), br()
                      )
                    ),
                    htmlOutput(outputId = "selectPISACountriesExplText"),
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
                                      tags$head(tags$style("#selectPISASelectedCountries {white-space: nowrap;}")),
                                      br()
                               )
                             ),
                      ),
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block", shinySaveButton(id = "selectPISACountriesChooseOutFile", label = "Define the output file name", title = "Define file name", icon = icon("file-export"), filetype = list(RData = "RData"), style = "color: #ffffff; background-color: #000000; border-radius: 10px"))
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "selectPISACountriesSyntaxHead"),
                             verbatimTextOutput(outputId = "selectPISACountriesSyntax"),
                             tags$head(tags$style(HTML("#selectPISACountriesSyntax {background-color: white; white-space: pre-wrap;}")))
                      ),
                      br(), br(), br(), br(), br(), br()
                    ),
                    conditionalPanel(condition = "output.selectPISACountriesSyntax",
                                     textOutput(outputId = "selectPISACountriesExecBtnHead"),
                                     uiOutput(outputId = "execSelectPISACountries"),
                                     br(), br(), br()
                    ),
                    conditionalPanel(condition = "selectPISACountriesSyntax",
                                     verbatimTextOutput(outputId = "consoleSelectPISACountries"),
                                     tags$head(tags$style("#consoleSelectPISACountries {color:red; background-color: white; overflow-y:scroll; max-height: 500px;}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleSelectPISACountries"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     ),
                                     br()
                    )
            ),
            tabItem(tabName = "pctsMeans", class = "active",
                    h1(textOutput(outputId = "h1PctsMeans")),
                    htmlOutput(outputId = "pctsMeansIntro"),
                    fluidRow(
                      column(width = 2, shinyFilesButton(id = "pctsMeansChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                      ),
                      column(width = 9, offset = 1,
                             conditionalPanel(condition = "input.pctsMeansChooseSrcFile",
                                              verbatimTextOutput(outputId = "pctsMeansSrcPathDisplay"),
                                              tags$head(tags$style("#pctsMeansSrcPathDisplay {background-color: white;}"))
                             )
                      )
                    ),
                    fluidRow(
                      br(), br(),
                      column(width = 2,
                             htmlOutput(outputId = "pctsMeansStudyName"),
                             htmlOutput(outputId = "pctsMeansStudyCycle"),
                      ),
                      column(width = 10,
                             htmlOutput(outputId = "pctsMeansRespHead"),
                             htmlOutput(outputId = "pctsMeansRespAvailable"),
                             br(), br()
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "pctsMeansNoWeights"),
                             tags$head(tags$style("#pctsMeansNoWeights {color: red; font-weight: bold;}"))
                      )
                    ),
                    htmlOutput(outputId = "pctsMeansVariablesExplText"),
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
                                      tags$head(tags$style("#pctsMeansWeightVar {white-space: nowrap;}")),
                                      br(), br()
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
                             htmlOutput(outputId = "pctsMeansWarnMoreVars"),
                             br()
                      )
                    ),
                    fluidRow(
                      column(width = 3,
                             uiOutput(outputId = "centralTendencyType")
                      ),
                      column(width = 9,
                             htmlOutput(outputId = "centralTendencyTypeExpl")
                      ),
                      br(), br(), br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 6,
                             uiOutput(outputId = "pctsMeansShortcut")
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block", shinySaveButton(id = "pctsMeansChooseOutFile", label = "Define the output file name", title = "Define file name", icon = icon("file-export"), filetype = list(xlsx = "xlsx"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")),
                             div(style="display:inline-block", uiOutput(outputId = "pctsMeansOpenOutput"))
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "pctsMeansSyntaxHead"),
                             verbatimTextOutput(outputId = "pctsMeansSyntax"),
                             tags$head(tags$style(HTML("#pctsMeansSyntax {background-color: white; white-space: pre-wrap;}")))
                      ),
                      br(), br(), br(), br(), br(), br()
                    ),
                    conditionalPanel(condition = "output.pctsMeansSyntax",
                                     textOutput(outputId = "pctsMeansExecBtnHead"),
                                     uiOutput(outputId = "execPctsMeans"),
                                     br(), br(), br()
                    ),
                    conditionalPanel(condition = "pctsMeansSyntax",
                                     verbatimTextOutput(outputId = "consolePctsMeans"),
                                     tags$head(tags$style("#consolePctsMeans {color:red; background-color: white; overflow-y:scroll; max-height: 500px;}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consolePctsMeans"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     ),
                                     br()
                    )
            ),
            tabItem(tabName = "prctls", class = "active",
                    h1(textOutput(outputId = "h1Prctls")),
                    htmlOutput(outputId = "prctlsIntro"),
                    fluidRow(
                      column(width = 2, shinyFilesButton(id = "prctlsChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                      ),
                      column(width = 9, offset = 1,
                             conditionalPanel(condition = "input.prctlsChooseSrcFile",
                                              verbatimTextOutput(outputId = "prctlsSrcPathDisplay"),
                                              tags$head(tags$style("#prctlsSrcPathDisplay {background-color: white;}"))
                             )
                      )
                    ),
                    fluidRow(
                      br(), br(),
                      column(width = 2,
                             htmlOutput(outputId = "prctlsStudyName"),
                             htmlOutput(outputId = "prctlsStudyCycle"),
                      ),
                      column(width = 10,
                             htmlOutput(outputId = "prctlsRespHead"),
                             htmlOutput(outputId = "prctlsRespAvailable"),
                             br(), br()
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "prctlsNoWeights"),
                             tags$head(tags$style("#prctlsNoWeights {color: red; font-weight: bold;}"))
                      )
                    ),
                    htmlOutput(outputId = "prctlsVariablesExplText"),
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
                                      tags$head(tags$style("#prctlsWeightVar {white-space: nowrap;}")),
                                      br(), br()
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
                             htmlOutput(outputId = "prctlsWarnMoreVars"),
                             br()
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "prctlsValuesExpl")
                      ),
                      br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 3,
                             uiOutput(outputId = "prctlsValues")
                      ),
                      column(width = 1,
                             uiOutput(outputId = "prctlsValuesReset"),
                             tags$style(type='text/css', "#prctlsValuesReset {margin-top: 13px;}")
                      ),
                      column(width = 4,
                             htmlOutput(outputId = "prctlsNotNum"),
                             tags$head(tags$style("#prctlsNotNum {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "prctlsNotInRange"),
                             tags$head(tags$style("#prctlsNotInRange {color: red; font-weight: bold;}"))
                      ),
                      br(), br(), br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 6,
                             uiOutput(outputId = "prctlsShortcut")
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block", shinySaveButton(id = "prctlsChooseOutFile", label = "Define the output file name", title = "Define file name", icon = icon("file-export"), filetype = list(xlsx = "xlsx"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")),
                             div(style="display:inline-block", uiOutput(outputId = "prctlsOpenOutput"))
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "prctlsSyntaxHead"),
                             verbatimTextOutput(outputId = "prctlsSyntax"),
                             tags$head(tags$style(HTML("#prctlsSyntax {background-color: white; white-space: pre-wrap;}")))
                      ),
                      br(), br(), br(), br(), br(), br()
                    ),
                    conditionalPanel(condition = "output.prctlsSyntax",
                                     textOutput(outputId = "prctlsExecBtnHead"),
                                     uiOutput(outputId = "execPrctls"),
                                     br(), br(), br()
                    ),
                    conditionalPanel(condition = "prctlsSyntax",
                                     verbatimTextOutput(outputId = "consolePrctls"),
                                     tags$head(tags$style("#consolePrctls {color:red; background-color: white; overflow-y:scroll; max-height: 500px;}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consolePrctls"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     ),
                                     br()
                    )
            ),
            tabItem(tabName = "bnchMarks", class = "active",
                    h1(textOutput(outputId = "h1Bench")),
                    htmlOutput(outputId = "benchIntro"),
                    fluidRow(
                      column(width = 2, shinyFilesButton(id = "benchChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                      ),
                      column(width = 9, offset = 1,
                             conditionalPanel(condition = "input.benchChooseSrcFile",
                                              verbatimTextOutput(outputId = "benchSrcPathDisplay"),
                                              tags$head(tags$style("#benchSrcPathDisplay {background-color: white;}"))
                             )
                      )
                    ),
                    fluidRow(
                      br(), br(),
                      column(width = 2,
                             htmlOutput(outputId = "benchStudyName"),
                             htmlOutput(outputId = "benchStudyCycle"),
                      ),
                      column(width = 10,
                             htmlOutput(outputId = "benchRespHead"),
                             htmlOutput(outputId = "benchRespAvailable"),
                             br(), br()
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
                      column(width = 3,
                             uiOutput(outputId = "benchType")
                      ),
                      column(width = 9,
                             htmlOutput(outputId = "benchTypeExpl")
                      ),
                      br(), br(), br(), br(), br(), br()
                    ),
                    htmlOutput(outputId = "benchVariablesExplText"),
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
                               column(width = 2,
                                      br(), br(),  br(), br(),
                                      uiOutput(outputId = "benchArrowSelWeightVarsRight"),
                                      uiOutput(outputId = "benchArrowSelWeightVarsLeft"),
                               ),
                               column(width = 10,
                                      DTOutput(outputId = "benchWeightVar"),
                                      tags$head(tags$style("#benchWeightVar {white-space: nowrap;}")),
                                      br(), br()
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
                             htmlOutput(outputId = "benchWarnMoreVars"),
                             br()
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "benchValuesExpl")
                      ),
                      br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 3,
                             uiOutput(outputId = "benchValues")
                      ),
                      column(width = 1,
                             uiOutput(outputId = "benchValuesReset"),
                             tags$style(type='text/css', "#benchValuesReset {margin-top: 13px;}")
                      ),
                      column(width = 4,
                             htmlOutput(outputId = "benchNotNum"),
                             tags$head(tags$style("#benchNotNum {color: red; font-weight: bold;}")),
                             htmlOutput(outputId = "benchNotInRange"),
                             tags$head(tags$style("#benchNotInRange {color: red; font-weight: bold;}"))
                      ),
                      br(), br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 2,
                             uiOutput(outputId = "benchComputeWithin")
                      ),
                      column(width = 10,
                             htmlOutput(outputId = "benchComputeWithinExpl")
                      ),
                      br(), br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 6,
                             uiOutput(outputId = "benchShortcut")
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block", shinySaveButton(id = "benchChooseOutFile", label = "Define the output file name", title = "Define file name", icon = icon("file-export"), filetype = list(xlsx = "xlsx"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")),
                             div(style="display:inline-block", uiOutput(outputId = "benchOpenOutput"))
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "benchSyntaxHead"),
                             verbatimTextOutput(outputId = "benchSyntax"),
                             tags$head(tags$style(HTML("#benchSyntax {background-color: white; white-space: pre-wrap;}")))
                      ),
                      br(), br(), br(), br(), br(), br()
                    ),
                    conditionalPanel(condition = "output.benchSyntax",
                                     textOutput(outputId = "benchExecBtnHead"),
                                     uiOutput(outputId = "execBench"),
                                     br(), br(), br()
                    ),
                    conditionalPanel(condition = "benchSyntax",
                                     verbatimTextOutput(outputId = "consoleBench"),
                                     tags$head(tags$style("#consoleBench {color:red; background-color: white; overflow-y:scroll; max-height: 500px;}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleBench"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     ),
                                     br()
                    )
            ),
            tabItem(tabName = "crossTabs", class = "active",
                    h1(textOutput(outputId = "h1CrossTabs")),
                    htmlOutput(outputId = "crossTabsIntro"),
                    fluidRow(
                      column(width = 2, shinyFilesButton(id = "crossTabsChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                      ),
                      column(width = 9, offset = 1,
                             conditionalPanel(condition = "input.crossTabsChooseSrcFile",
                                              verbatimTextOutput(outputId = "crossTabsSrcPathDisplay"),
                                              tags$head(tags$style("#crossTabsSrcPathDisplay {background-color: white;}"))
                             )
                      )
                    ),
                    fluidRow(
                      br(), br(),
                      column(width = 2,
                             htmlOutput(outputId = "crossTabsStudyName"),
                             htmlOutput(outputId = "crossTabsStudyCycle"),
                      ),
                      column(width = 10,
                             htmlOutput(outputId = "crossTabsRespHead"),
                             htmlOutput(outputId = "crossTabsRespAvailable"),
                             br(), br()
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
                                      tags$head(tags$style("#crossTabsWeightVar {white-space: nowrap;}")),
                                      br(), br()
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
                               tags$head(tags$style("#crossTabsWgtsNotWgts {color: red; font-weight: bold;}")),
                               br()
                        )
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             uiOutput(outputId = "crossTabsExpCnts"),
                             uiOutput(outputId = "crossTabsRowPcts"),
                             uiOutput(outputId = "crossTabsColPcts"),
                             uiOutput(outputId = "crossTabsTotPcts")
                      ),
                      br(), br()
                    ),
                    fluidRow(
                      br(), br(),
                      column(width = 6,
                             uiOutput(outputId = "crossTabsShortcut")
                      ),
                      br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block", shinySaveButton(id = "crossTabsChooseOutFile", label = "Define the output file name", title = "Define file name", icon = icon("file-export"), filetype = list(xlsx = "xlsx"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")),
                             div(style="display:inline-block", uiOutput(outputId = "crossTabsOpenOutput"))
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "crossTabsSyntaxHead"),
                             verbatimTextOutput(outputId = "crossTabsSyntax"),
                             tags$head(tags$style(HTML("#crossTabsSyntax {background-color: white; white-space: pre-wrap;}")))
                      ),
                      br(), br(), br(), br(), br(), br()
                    ),
                    conditionalPanel(condition = "output.crossTabsSyntax",
                                     textOutput(outputId = "crossTabsExecBtnHead"),
                                     uiOutput(outputId = "execCrossTabs"),
                                     br(), br(), br()
                    ),
                    conditionalPanel(condition = "crossTabsSyntax",
                                     verbatimTextOutput(outputId = "consoleCrossTabs"),
                                     tags$head(tags$style("#consoleCrossTabs {color:red; background-color: white; overflow-y:scroll; max-height: 500px;}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleCrossTabs"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     ),
                                     br()
                    )
            ),
            tabItem(tabName = "corr", class = "active",
                    h1(textOutput(outputId = "h1Corr")),
                    htmlOutput(outputId = "corrIntro"),
                    fluidRow(
                      column(width = 2, shinyFilesButton(id = "corrChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                      ),
                      column(width = 9, offset = 1,
                             conditionalPanel(condition = "input.corrChooseSrcFile",
                                              verbatimTextOutput(outputId = "corrSrcPathDisplay"),
                                              tags$head(tags$style("#corrSrcPathDisplay {background-color: white;}"))
                             )
                      )
                    ),
                    fluidRow(
                      br(), br(),
                      column(width = 2,
                             htmlOutput(outputId = "corrStudyName"),
                             htmlOutput(outputId = "corrStudyCycle"),
                      ),
                      column(width = 10,
                             htmlOutput(outputId = "corrRespHead"),
                             htmlOutput(outputId = "corrRespAvailable"),
                             br(), br()
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "corrNoWeights"),
                             tags$head(tags$style("#corrNoWeights {color: red; font-weight: bold;}"))
                      )
                    ),
                    fluidRow(
                      column(width = 3,
                             uiOutput(outputId = "corrType")
                      ),
                      column(width = 9,
                             htmlOutput(outputId = "corrTypeExpl")
                      ),
                      br(), br(), br(), br(), br(), br()
                    ),
                    htmlOutput(outputId = "corrVariablesExplText"),
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
                                      tags$head(tags$style("#corrWeightVar {white-space: nowrap;}")),
                                      br(), br()
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
                             htmlOutput(outputId = "corrWarnMoreVars"),
                             br()
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             uiOutput(outputId = "corrShortcut")
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block", shinySaveButton(id = "corrChooseOutFile", label = "Define the output file name", title = "Define file name", icon = icon("file-export"), filetype = list(xlsx = "xlsx"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")),
                             div(style="display:inline-block", uiOutput(outputId = "corrOpenOutput"))
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "corrSyntaxHead"),
                             verbatimTextOutput(outputId = "corrSyntax"),
                             tags$head(tags$style(HTML("#corrSyntax {background-color: white; white-space: pre-wrap;}")))
                      ),
                      br(), br(), br(), br(), br(), br()
                    ),
                    conditionalPanel(condition = "output.corrSyntax",
                                     textOutput(outputId = "corrExecBtnHead"),
                                     uiOutput(outputId = "execCorr"),
                                     br(), br(), br()
                    ),
                    conditionalPanel(condition = "corrSyntax",
                                     verbatimTextOutput(outputId = "consoleCorr"),
                                     tags$head(tags$style("#consoleCorr {color:red; background-color: white; overflow-y:scroll; max-height: 500px;}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleCorr"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     ),
                                     br()
                    )
            ),
            tabItem(tabName = "linReg", class = "active",
                    h1(textOutput(outputId = "h1LinReg")),
                    htmlOutput(outputId = "linRegIntro"),
                    fluidRow(
                      column(width = 2, shinyFilesButton(id = "linRegChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                      ),
                      column(width = 9, offset = 1,
                             conditionalPanel(condition = "input.linRegChooseSrcFile",
                                              verbatimTextOutput(outputId = "linRegSrcPathDisplay"),
                                              tags$head(tags$style("#linRegSrcPathDisplay {background-color: white;}"))
                             )
                      )
                    ),
                    fluidRow(
                      br(), br(),
                      column(width = 2,
                             htmlOutput(outputId = "linRegStudyName"),
                             htmlOutput(outputId = "linRegStudyCycle"),
                      ),
                      column(width = 10,
                             htmlOutput(outputId = "linRegRespHead"),
                             htmlOutput(outputId = "linRegRespAvailable"),
                             br(), br()
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "linRegNoWeights"),
                             tags$head(tags$style("#linRegNoWeights {color: red; font-weight: bold;}"))
                      )
                    ),
                    htmlOutput(outputId = "linRegVariablesExplText"),
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
                             ),
                             br(), br(), br(), br()
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
                             htmlOutput(outputId = "linRegWarnMoreVars"),
                             br()
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             uiOutput(outputId = "linRegStandardize")
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 6,
                             uiOutput(outputId = "linRegShortcut")
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block", shinySaveButton(id = "linRegChooseOutFile", label = "Define the output file name", title = "Define file name", icon = icon("file-export"), filetype = list(xlsx = "xlsx"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")),
                             div(style="display:inline-block", uiOutput(outputId = "linRegOpenOutput"))
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "linRegSyntaxHead"),
                             verbatimTextOutput(outputId = "linRegSyntax"),
                             tags$head(tags$style(HTML("#linRegSyntax {background-color: white; white-space: pre-wrap;}")))
                      ),
                      br(), br(), br(), br(), br(), br()
                    ),
                    conditionalPanel(condition = "output.linRegSyntax",
                                     textOutput(outputId = "linRegExecBtnHead"),
                                     uiOutput(outputId = "execLinReg"),
                                     br(), br(), br()
                    ),
                    conditionalPanel(condition = "linRegSyntax",
                                     verbatimTextOutput(outputId = "consoleLinReg"),
                                     tags$head(tags$style("#consoleLinReg {color:red; background-color: white; overflow-y:scroll; max-height: 500px;}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleLinReg"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     ),
                                     br()
                    )
            ),
            tabItem(tabName = "binLogReg", class = "active",
                    h1(textOutput(outputId = "h1binLogReg")),
                    htmlOutput(outputId = "binLogRegIntro"),
                    fluidRow(
                      column(width = 2, shinyFilesButton(id = "binLogRegChooseSrcFile", label = "Choose data file", title = "Navigate and select a file", multiple = FALSE, icon = icon("file-import"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
                      ),
                      column(width = 9, offset = 1,
                             conditionalPanel(condition = "input.binLogRegChooseSrcFile",
                                              verbatimTextOutput(outputId = "binLogRegSrcPathDisplay"),
                                              tags$head(tags$style("#binLogRegSrcPathDisplay {background-color: white;}"))
                             )
                      )
                    ),
                    fluidRow(
                      br(), br(),
                      column(width = 2,
                             htmlOutput(outputId = "binLogRegStudyName"),
                             htmlOutput(outputId = "binLogRegStudyCycle"),
                      ),
                      column(width = 10,
                             htmlOutput(outputId = "binLogRegRespHead"),
                             htmlOutput(outputId = "binLogRegRespAvailable"),
                             br(), br()
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "binLogRegNoWeights"),
                             tags$head(tags$style("#binLogRegNoWeights {color: red; font-weight: bold;}"))
                      )
                    ),
                    htmlOutput(outputId = "binLogRegVariablesExplText"),
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
                             ),
                             br(), br()
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
                             htmlOutput(outputId = "binLogRegWarnMoreVars"),
                             br()
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             uiOutput(outputId = "binLogRegWgtNorm")
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 6,
                             uiOutput(outputId = "binLogRegStandardize")
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 6,
                             uiOutput(outputId = "binLogRegShortcut")
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             div(style="display:inline-block", shinySaveButton(id = "binLogRegChooseOutFile", label = "Define the output file name", title = "Define file name", icon = icon("file-export"), filetype = list(xlsx = "xlsx"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")),
                             div(style="display:inline-block", uiOutput(outputId = "binLogRegOpenOutput"))
                      ),
                      br(), br(), br(), br()
                    ),
                    fluidRow(
                      column(width = 12,
                             htmlOutput(outputId = "binLogRegSyntaxHead"),
                             verbatimTextOutput(outputId = "binLogRegSyntax"),
                             tags$head(tags$style(HTML("#binLogRegSyntax {background-color: white; white-space: pre-wrap;}")))
                      ),
                      br(), br(), br(), br(), br(), br()
                    ),
                    conditionalPanel(condition = "output.binLogRegSyntax",
                                     textOutput(outputId = "binLogRegExecBtnHead"),
                                     uiOutput(outputId = "execBinLogReg"),
                                     br(), br(), br()
                    ),
                    conditionalPanel(condition = "binLogRegSyntax",
                                     verbatimTextOutput(outputId = "consoleBinLogReg"),
                                     tags$head(tags$style("#consoleBinLogReg {color:red; background-color: white; overflow-y:scroll; max-height: 500px;}")),
                                     tags$script(
                                       '
Shiny.addCustomMessageHandler("scrollCallback",
function(color) {
var objDiv = document.getElementById("consoleBinLogReg"); /* Here we point to "console" output, adapt in other cases */
objDiv.scrollTop = objDiv.scrollHeight;
}
);'
                                     ),
                                     br()
                    )
            )
          ),
          tags$footer(HTML("R Analyzer for Large-Scale Assessments by INERI.org - &copy;(left) 2021"), align = "center", style = "
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
