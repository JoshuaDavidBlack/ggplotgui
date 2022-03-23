# zzz.R
.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "assets",
    directoryPath = system.file(
      "assets",
      package = "ggplotgui"
    )
  )
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("assets")
}
