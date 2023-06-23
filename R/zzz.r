# Display a menu for the user after the package is attached.
.onAttach <- function(libname, pkgname) {

  if(interactive() == TRUE) {

    path.to.start.GUI <- system.file("shiny", "GUI", "startGUI.r", package = "RALSA")

    packageStartupMessage("\nWelcome to RALSA!\n\nFor news, help, and requests, please visit http://ralsa.ineri.org/\n")

    switch(
      menu(choices = c("Yes, start now", "No, not at this time"),
           title = "Do you want to start RALSA graphical user interface now?") + 1,
      packageStartupMessage("\nYou can start it later by executing the following:\n\n   ralsaGUI()"),
      eval(parse(text = paste0('invisible(rstudioapi::jobRunScript(path = "', path.to.start.GUI,'", importEnv = TRUE, exportEnv = "R_GlobalEnv", name = "RALSA User Interface"));Sys.sleep(1);browseURL(url = "http://127.0.0.1:3838");rstudioapi::executeCommand(commandId = "activateConsole");packageStartupMessage(paste0("\nRALSA is running at ", Sys.info()[names(Sys.info()) == "nodename"], " for ", Sys.info()[names(Sys.info()) == "effective_user"], "!"))'))),
      packageStartupMessage("You can start it later by executing the following command:\n\n   ralsaGUI()\n"),
    )
  }
}
