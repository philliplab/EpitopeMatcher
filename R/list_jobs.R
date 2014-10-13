# This file contains all the code that matches the input files to each other
# and generate a list of the scoring jobs that must be executed

#' A class that contains all the required information to run an epitope scoring
#' job.
#'
#' Three pieces of information is required for a scoring job to be valid:
#' \itemize{
#'   \item{hla_genotype - The name of the hla_genotype to investigate}
#'   \item{query_sequence_names - A character vector of the names of the query
#'   sequences in which this hla_genotype must be looked for}
#'   \item{hla_details - A list of further details about this hla_genotype}
#' }
#'
#' The hla_details is forced to have the following values: "end_pos", 
#' "epitope", "gene_name", "hla_genotype", "hxb2_dna_position", "organism", 
#' "start_pos", "subprotein", and "sub_type". This is reasonable since the HLA
#' data will always come from the LANL file and these details must be added to
#' the results and error logs.
#' 
#' @rdname Scoring_Job
#' @aliases Scoring_Job-class
#' @exportClass Scoring_Job
#' @export .Scoring_Job

.Scoring_Job <- setClass(
  Class = 'Scoring_Job',
  representation = representation(
    hla_genotype = 'character',
    query_sequence_names = 'character',
    hla_details = 'list'),

  validity = function(object){
    if (length(object@hla_details) == 0){
      stop('At least some details must be supplied about the hla')
    }
    required_details <- c("end_pos", "epitope", "gene_name", "hla_genotype", "hxb2_dna_position", 
                          "organism", "start_pos", "subprotein", "sub_type")
    details_supplied <- sort(names(object@hla_details))
    if (!all(details_supplied == required_details)){
      stop("Incorrect hla details supplied")
    }
  }
)

#' Matches the patient hla data to the query sequence to check which hla's must
#' be checked for in which sequences.
#' 
#' It treats the patient_id column in the patient_hla data as a regular
#' expression and performs the lookup to the query sequence names
#' 
#' A central issue is how to communicate the query sequence names / positions
#' between the different functions. The preferred approach is to use the entire
#' FASTA header for the sequence in question.
#' 
#' @return A list of vectors of query_sequence_names. The list is indexed by
#' hla_genotype and the elements is a character vector listing all the query
#' sequences that must be checked against the associated hla_genotype
#'
#' @param query_alignment An AAStringSet that contains the multiple sequence
#' alignment of the patient's viral sequences
#' @param patient_hla The data.frame that specifies which query sequence to
#' check against which hla genotypes
#' @export

match_patient_hla_to_query_alignment <- function(query_alignment, patient_hla){
  q_pids <- get_patient_ids(query_alignment)
  q_pids_long <- get_patient_ids(query_alignment, NULL, NULL)

  matched_patients <- list()
  for (i in 1:nrow(patient_hla)){
    p_pid <- patient_hla[i,'patient_id']
    hla <- patient_hla[i,'hla_genotype']
    query_sequence_ids <- grep(p_pid, q_pids)
    query_sequence_names <- q_pids_long[query_sequence_ids]
    matched_patients[[hla]] <- query_sequence_names
  }
  return(matched_patients)
}

#' Flattens the LANL HLA file
#'
#' Sometimes the same hla has a number of different names. Hence the
#' hla_genotype column in the lanl file must be processed before the matches
#' can be made.
#' 
#' This function takes the LANL HLA file and transforms it so that each row
#' correspond to one and only one hla. This means that row in which the
#' 'hla_genotype' column is unpopulated gets discarded and that rows in which
#' the 'hla_genotype' column contains the names of more than one hla_genotype
#' (assumed to be seperated by commas) will be duplicated and each duplicate
#' will be assigned to one hla_genotype.
#' @param lanl_hla The data.frame (of class LANL_HLA_data) that contains
#' the descriptions of the different HLA genotypes
#' @export

flatten_lanl_hla <- function(lanl_hla){
  lanl_hla_genotypes <- strsplit(lanl_hla[,'hla_genotype'], ',')
  flat_lanl_hla <- lanl_hla[0,]
  for (i in seq_along(lanl_hla_genotypes)){
    hla_genotypes <- strsplit(lanl_hla[i, 'hla_genotype'], ',')[[1]]
    for (j in seq_along(hla_genotypes)){
      hla_genotype <- gsub("^ +", "", hla_genotypes[j])
      hla_genotype <- gsub(" +$", "", hla_genotype)

      flat_lanl_hla <- rbind(flat_lanl_hla, lanl_hla[i,])
      flat_lanl_hla$hla_genotype[nrow(flat_lanl_hla)] <- hla_genotype
    }
  }
  return(flat_lanl_hla)
}

#' Extracts an hla's details from the LANL file
#'
#' @return A list of data.frames of hla details. The list is indexed by
#' hla_genotype and the data.frame contains a single row with the relevant
#' details of the hla_genotype.
#'
#' @param patient_hla The data.frame that specifies which query sequence to
#' check against which hla genotypes
#' @param lanl_hla The data.frame (of class LANL_HLA_data) that contains
#' the descriptions of the different HLA genotypes
#' @export

match_patient_hla_to_lanl_hla <- function(patient_hla, lanl_hla){

}
