#' @title Start RALSA's Graphical User Interface (GUI)
#'
#' @description Starts the GUI. The GUI contains elements for each parameter of each function (data preparation or analysis).
#' @export

ralsaGUI <- function() {
  appDir <- system.file("shiny", "GUI", package = "RALSA")
  if (appDir == "") {
    stop("Could not find the app directory. Try re-installing `RALSA`.", call. = FALSE)
  }
  message(paste0("\nRALSA is running at ", Sys.info()[names(Sys.info()) == "nodename"], " for ", Sys.info()[names(Sys.info()) == "effective_user"], "!"))
  shiny::runApp(appDir, display.mode = "normal", quiet = TRUE, launch.browser = TRUE)
}
