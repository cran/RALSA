appDir <- system.file("shiny", "GUI", package = "RALSA")
if (appDir == "") {
  stop("Could not find the app directory. Try re-installing `RALSA`.", call. = FALSE)
}
message(paste0("\nRALSA is running at ", Sys.info()[names(Sys.info()) == "nodename"], " for ", Sys.info()[names(Sys.info()) == "effective_user"], "!"))
shiny::runApp(appDir = appDir, display.mode = "normal", quiet = TRUE, host = "127.0.0.1", port = 3838)
