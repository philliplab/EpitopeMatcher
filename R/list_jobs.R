# This file contains all the code that matches the input files to each other
# and generate a list of the scoring jobs that must be executed

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
#' @return A list of lists. The inner lists contains the elements
#' 'hla_genotype' and 'query_sequence_names' 
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
    matched_patients[[as.character(i)]] <- list(hla_genotype = hla,
                                                query_sequence_names = query_sequence_names)
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

#' Builds scoring jobs from pre-processed inputs
#' @param matched_patients As produced by
#' \code{\link{match_patient_hla_to_query_alignment}}.
#' @param flat_lanl_hla As produced by \code{\link{flatten_lanl_hla}}
#' @export

build_scoring_jobs <- function(matched_patients, flat_lanl_hla){
  the_scoring_jobs <- list()
  no_hla_details <- data.frame(hla_genotype = character(0),
                               stringsAsFactors = FALSE)
  k <- 0
  for (i in seq_along(matched_patients)){
    hla_genotype <- matched_patients[[i]]$hla_genotype
    query_sequence_names = matched_patients[[i]]$query_sequence_names 
    hla_details <- flat_lanl_hla[flat_lanl_hla$hla_genotype == hla_genotype,
                                 names(flat_lanl_hla) != "hla_genotype"]
    if (nrow(hla_details) == 0){
      no_hla_details <- rbind(no_hla_details, 
                              data.frame(hla_genotype = hla_genotype,
                                         stringsAsFactors = FALSE))
    } else {
      for (j in 1:nrow(hla_details)){
        k <- k + 1
        hla_detail_row <- hla_details[j,]
        the_scoring_jobs[[k]] <- .Scoring_Job(hla_genotype = hla_genotype,
          query_sequence_names = query_sequence_names,
          hla_details = as.list(hla_detail_row))
      } 
    }
  }
  if (length(the_scoring_jobs) > 0){
    msg <- "Success"
  } else {
    msg <- "Failure"
  }
  return(list(msg = msg,
              result = the_scoring_jobs,
              error_logs = list(no_hla_details = no_hla_details)))
}

#' Processes the three input files (query_alignment, patient_hla and lanl_hla)
#' in to a list of scoring jobs.
#' 
#' First the patient_hla data is matched to the query_alignment
#' \code{\link{match_patient_hla_to_query_alignment}}, then the lanl_hla file
#' is flattened \code{\link{flatten_lanl_hla}}, and finally, the jobs are built
#' \code{\link{build_scoring_jobs}}.
#'
#' This list of jobs can then be used to perform the comparisons.
#'
#' @return A list of Scoring_Jobs
#'
#' @param query_alignment An AAStringSet that contains the multiple sequence
#' alignment of the patient's viral sequences
#' @param patient_hla The data.frame that specifies which query sequence to
#' check against which hla genotypes
#' @param lanl_hla The data.frame (of class LANL_HLA_data) that contains
#' the descriptions of the different HLA genotypes
#' @export

list_scores_to_compute <- function(query_alignment, patient_hla, lanl_hla){
  matched_patients <- match_patient_hla_to_query_alignment(query_alignment, patient_hla)
  flat_lanl_hla <- flatten_lanl_hla(lanl_hla)
  the_scoring_jobs <- build_scoring_jobs(matched_patients, flat_lanl_hla)
  return(the_scoring_jobs)
}
