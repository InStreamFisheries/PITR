.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to PITR. PITR is continuously being developed and your input can help.")
  packageStartupMessage("Check for updates regularly and contact maintainer for support.")
}

.onLoad <- function(libname, pkgname) {
  options(scipen = 999)
}

.onUnload <- function(libname, pkgname) {
  options(scipen = 0)
}
