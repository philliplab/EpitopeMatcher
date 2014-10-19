#' Runs the shiny app for this package
#' 
#' @export

run_EpitopeMatcher_app <- function(port = 5436){
  packageDir <- find.package('EpitopeMatcher')
  shinyAppDir <- file.path(packageDir, "www") 
  if (is.null(port)){
    runApp(shinyAppDir, host = "0.0.0.0")
  } else {
    runApp(shinyAppDir, port = port, host = "0.0.0.0")
  }
}
