# Display a menu for the user after the package is attached.
.onAttach <- function(libname, pkgname){

  if(interactive() == TRUE) {
    packageStartupMessage("\nWelcome to RALSA!\n\nFor news, help, and requests, please visit http://ralsa.ineri.org/\n")

    switch(menu(choices = c("Yes, start now", "No, not at this time"), title = "Do you want to start RALSA graphical user interface now?") + 1, packageStartupMessage("\nYou can start it later by executing the following:\n\n   ralsaGUI()"), ralsaGUI(), packageStartupMessage("You can start it later by executing the following command:\n\n   ralsaGUI()"))
  }
}
