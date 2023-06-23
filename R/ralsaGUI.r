#' @title Start RALSA's Graphical User Interface (GUI)
#'
#' @description Starts the GUI. The GUI contains elements for each parameter of each function (data preparation or analysis). The GUI starts as a background job without blocking the console which can be used as usual.
#' @export

ralsaGUI <- function() {
  path.to.start.GUI <- system.file("shiny", "GUI", "startGUI.r", package = "RALSA")
  message(paste0("\nRALSA is running at ", Sys.info()[names(Sys.info()) == "nodename"], " for ", Sys.info()[names(Sys.info()) == "effective_user"], "!"))

  eval(parse(text = paste0('invisible(rstudioapi::jobRunScript(path = "', path.to.start.GUI,'", importEnv = TRUE, exportEnv = "R_GlobalEnv", name = "RALSA User Interface"))')))
  Sys.sleep(1)
  browseURL(url = "http://127.0.0.1:3838")
  rstudioapi::executeCommand(commandId = "activateConsole")
}
