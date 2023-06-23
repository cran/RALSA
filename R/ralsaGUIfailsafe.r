#' @title Start RALSA's Graphical User Interface (GUI) in a failsafe mode
#'
#' @description If, for any reason, the GUI cannot be started using the regular \code{ralsaGUI()} command, use this instead. The downside of using this function is that the R console will be blocked.
#' @export

ralsaGUIfailsafe <- function() {
  appDir <- system.file("shiny", "GUI", package = "RALSA")
  if (appDir == "") {
    stop("Could not find the app directory. Try re-installing `RALSA`.", call. = FALSE)
  }
  message(paste0("\nRALSA is running at ", Sys.info()[names(Sys.info()) == "nodename"], " for ", Sys.info()[names(Sys.info()) == "effective_user"], "!"))
  shiny::runApp(appDir, display.mode = "normal", quiet = TRUE, launch.browser = TRUE)
}
