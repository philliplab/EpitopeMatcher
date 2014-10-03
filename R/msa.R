#' @include patient_HLA.R
NULL

#' Reads in the query alignment
#' @param file_name Name of the file
#' @export

read_query_alignment <- function(file_name){
#  if (is.null(file_name)){
#    warning("No file name supplied to read_patient_hla - using test data")
#    file_name <- file.path(find.package('EpitopeMatcher', .libPaths()), 
#                           'test_data/query_alignment.FASTA')
#  }
  x <- readAAStringSet(file_name)
  return(x)
}

#' @rdname get_patient_ids-methods
#' @aliases get_patient_ids
setMethod("get_patient_ids", "AAStringSet",
          function(x, sep = '|', id_position = 1){
            split_names <- strsplit(names(x), '\\|')[[1]]
            return(split_names[id_position])
          })
