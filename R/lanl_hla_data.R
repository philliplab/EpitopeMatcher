#' The class for the data from LANL that describe the HLA genotype's epitopes
#' 
#' @rdname LANL_HLA_data
#' @aliases LANL_HLA_data-class
#' @exportClass LANL_HLA_data
#' @export .LANL_HLA_data

.LANL_HLA_data <- setClass(
  Class = 'LANL_HLA_data',
  representation = representation(),
  contains = 'data.frame',

  validity = function(object){
    col_names <- c("epitope", "gene_name", "start_pos", "end_pos", 
                   "sub_type", "organism", "hla_genotype")
    for (i in 1:length(col_names)){
      if (names(object)[i] != col_names[i]){
        stop(paste0("incorrent column names. ", i, "th column must be ", col_names[i]))
      }
    }
    if (nrow(object) == 0){
      stop("Must describe at least HLA genotype")
    }
  }
)

#' A function that reads a patient HLA genotype specification file
#' @param file_name Name of the file
#' @export

read_lanl_hla <- function(file_name){
#  if (is.null(file_name)){
#    warning("No file name supplied to read_patient_hla - using test data")
#    file_name <- file.path(find.package('EpitopeMatcher', .libPaths()), 
#                           'test_data/lanl_hla_file.csv')
#  }
  x <- read.csv(file_name,
                stringsAsFactors = FALSE)
  return(.LANL_HLA_data(x))
}


