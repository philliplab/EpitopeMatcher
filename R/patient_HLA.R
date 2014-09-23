#' The class for the data that describes which patients have which HLAs
#' 
#' @rdname Patient_HLA
#' @aliases Patient_HLA-class
#' @exportClass Patient_HLA
#' @export .Patient_HLA

.Patient_HLA <- setClass(
  Class = 'Patient_HLA',
  representation = representation(),
  contains = 'data.frame',

  validity = function(object){
    if (names(object)[1] != 'patient_id'){
      stop("incorrent column names. First Column must be patient_id")
    }
    if (names(object)[2] != 'hla_genotype'){
      stop("incorrent column names. Second Column must be hla_genotype")
    }
    if (nrow(object) == 0){
      stop("Must specify at least one patient HLA genotype")
    }
  }
)

#' A function that reads a patient HLA genotype specification file
#' @param file_name Name of the file
#' @export

read_patient_hla <- function(file_name){
#  if (is.null(file_name)){
#    warning("No file name supplied to read_patient_hla - using test data")
#    file_name <- file.path(find.package('EpitopeMatcher', .libPaths()), 
#                           'test_data/patient_hla_file.csv')
#  }
  x <- read.csv(file_name,
                stringsAsFactors = FALSE)
  return(.Patient_HLA(x))
}

#' Returns the ids of the patients in the data structure
#' @param data The data structure to interrogate
#' @rdname get_patient_ids-methods
#' @export get_patient_ids
setGeneric("get_patient_ids",
           function(x){standardGeneric("get_patient_ids")}
)

#' @rdname get_patient_ids-methods
#' @aliases get_patient_ids
setMethod("get_patient_ids", "Patient_HLA",
          function(x){
            return(x$patient_id)
          })
