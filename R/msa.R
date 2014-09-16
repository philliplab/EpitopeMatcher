#' Reads in the query alignment
#' @param file_name Name of the file
#' @export

read_query_alignment <- function(file_name = NULL){
  if (is.null(file_name)){
    warning("No file name supplied to read_patient_hla - using test data")
    file_name <- file.path(find.package('EpitopeMatcher', .libPaths()), 
                           'test_data/query_alignment.FASTA')
  }
  x <- readAAMultipleAlignment(file_name)
  return(x)
}
