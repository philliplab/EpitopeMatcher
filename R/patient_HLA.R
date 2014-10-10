#' @import Biostrings digest methods shiny testthat
#' @importFrom shiny runApp
NULL

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
  x <- read.csv(file_name,
                stringsAsFactors = FALSE)
  return(.Patient_HLA(x))
}

#' A function that returns a test patient hla dataset
#' @param dataset_name The name of the test dataset to return
#' @export

get_test_patient_hla_data <- function(dataset_name = 'base'){
  data_sets <- list(
    'base' = new("Patient_HLA", 
                 .Data = list(c("pat01", "pat01", "pat02", "pat", "pat0[13]", "pat01"), 
                              c("A*3002", "A*3003", "B40", "B63", "B27", "X67")), 
            names = c("patient_id", "hla_genotype"), 
            row.names = 1:6, 
            .S3Class = "data.frame")
    )
  return(data_sets[[dataset_name]])
}

#' Returns the ids of the patients in the data structure
#' @param x The data structure to interrogate
#' @param sep The symbol used to separate elements in the sequence names
#' @param id_position After the sequence name has been split on the 'sep'
#' character, which element of the resulting vector contains the patient id?
#' @rdname get_patient_ids-methods
#' @export get_patient_ids
setGeneric("get_patient_ids",
           function(x, sep = '\\|', id_position = 1){standardGeneric("get_patient_ids")}
)

#' @rdname get_patient_ids-methods
#' @aliases get_patient_ids
setMethod("get_patient_ids", "Patient_HLA",
          function(x){
            return(x$patient_id)
          })
