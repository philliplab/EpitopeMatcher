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

#' A function that reads a patient HLA genotype specification file
#' @param file_name Name of the file
#' @export

read_patient_hla <- function(file_name){
  warning("read_patient_hla is a stub")
  x <- data.frame(
    patient_id = c('P00885', 'P00885', 'P00885', 'P00885', 'P00885', 'P01288',
                   'P01288', 'P01288', 'P01288', 'P01288', 'P01288', 'P02869',
                   'P02869', 'P02869', 'P02869', 'P02869', 'P02869', 'P03257',
                   'P03257', 'P03257', 'P03257', 'P03257', 'P03257', 'V05130',
                   'V05130', 'V05130', 'V05130', 'V05130', 'V05649', 'V05649',
                   'V05649', 'V05649', 'V05649', 'V05649', 'V11204', 'V11204',
                   'V11204', 'V11204', 'V11474', 'V11474', 'V11474', 'V11474',
                   'V11474', 'V11474'),
    HLA_genotype = c('A*4301', 'A*6801',
                   'B*1503', 'B*5702', 'Cw*1801', 'A*3301', 'A*6601', 'B*1801',
                   'B*4201', 'Cw*0704', 'Cw*1701', 'A*0123', 'A*2401',
                   'B*4403', 'B*5101', 'Cw*0401', 'Cw*1601', 'A*3001',
                   'A*6601', 'B*3910', 'B*4201', 'Cw*1203', 'Cw*1701',
                   'A*2901', 'A*3402', 'B*4403', 'Cw*0704', 'Cw*0701',
                   'A*0201', 'A*0301', 'B*1510', 'B*4501', 'Cw*0304',
                   'Cw*1601', 'B*1503', 'B*4403', 'Cw*0210', 'Cw*0401',
                   'A*0205', 'A*6601', 'B*1516', 'B*5802', 'Cw*0602',
                   'Cw*1601'),
    stringsAsFactors = FALSE)
  return(.Patient_HLA(x))
}

