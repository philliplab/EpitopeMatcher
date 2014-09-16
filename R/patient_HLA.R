#' The class for the data that describes which patients have which HLAs
#' @rdname Patient_HLA
#' @aliases Patient_HLA-class
#' @exportClass Patient_HLA
#' @export .Patient_HLA

.Patient_HLA <- setClass(
  Class = 'Patient_HLA',
  representation = representation(
    data = 'data.frame'
  ),
  validity = function(object){
    if (tolower(names(object@data)[1]) != 'patient_id'){
      stop("incorrent column names. First Column must be patient_id - Capitalization does not 
           matter")
    }
    if (tolower(names(object@data)[2]) != 'hla_genotype'){
      stop("incorrent column names. Second Column must be hla_genotype - Capitalization does not 
           matter")
    }
    if (nrow(object@data) == 0){
      stop("Must specify at least one patient HLA genotype")
    }
  }
)
