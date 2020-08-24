#' Get full set of test data
#'
#' Running this function will produce 3 files in the specified folder.
#' \enumerate{
#'   \item test_patient_hla_genotypes.csv as produced by get_test_patient_hla_data()
#'   \item test_lanl_hla_data.csv as produced by get_test_lanl_hla_data()
#'   \item test_query_alignment.fasta as produced by get_test_query_alignment()
#' }
#'
#' @param output_folder Folder in which the files are to be produced. Defaults to the current folder.
#' @export

get_set_of_test_data <- function(output_folder = '.'){
  write.csv(get_test_patient_hla_data(), 
            paste0(gsub('(/$)|(\\$)', '', output_folder), 
                   .Platform$file.sep,
                   'test_patient_hla_genotypes.csv'),
            row.names = F)
  write.csv(get_test_lanl_hla_data(), 
            paste0(gsub('(/$)|(\\$)', '', output_folder), 
                   .Platform$file.sep,
                   'test_lanl_hla_genotypes.csv'),
            row.names = F)
  writeXStringSet(get_test_query_alignment(), 
                  paste0(gsub('(/$)|(\\$)', '', output_folder), 
                         .Platform$file.sep,
                         'test_query_alignment.fasta'),
                  width = 1e4)
  invisible(1)
}
