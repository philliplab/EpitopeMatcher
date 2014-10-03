#' @include patient_HLA.R
NULL

#' Reads in the query alignment
#' @param file_name Name of the file
#' @export

read_query_alignment <- function(file_name){
  x <- readAAStringSet(file_name)
  return(x)
}

#' @rdname get_patient_ids-methods
#' @aliases get_patient_ids
setMethod("get_patient_ids", "AAStringSet",
          function(x, sep = '\\|', id_position = 1){
            split_names <- strsplit(names(x), sep)
            names_vector <- unlist(lapply(split_names, `[[`, id_position))
            names_vector <- gsub("^ +", "", names_vector)
            names_vector <- gsub(" +$", "", names_vector)
            return(names_vector)
          })
