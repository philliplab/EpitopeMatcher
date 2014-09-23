#' Runs the shiny app for this package
#' 
#' @export

run_EpitopeMatcher_app <- function(){
  packageDir <- find.package('EpitopeMatcher')
  shinyAppDir <- file.path(packageDir, "www") 
  runApp(shinyAppDir)
}
