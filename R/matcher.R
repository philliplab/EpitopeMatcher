#' Gets all the patient ids that are in both the MSA and the patient_hla file
#' @param query_alignment The query alignment
#' @param patient_hla The data.frame (of class Patient_HLA) that contain lists
#' all the HLA genotypes each patient has.
#' @export

get_matchable_patient_ids <- function(query_alignment, patient_hla){

  # These checks can be done better using OO features?
  stopifnot(class(query_alignment) == 'AAMultipleAlignment')
  stopifnot(class(patient_hla) == 'Patient_HLA')

  qa_ids <- get_patient_ids(query_alignment)
  ph_ids <- get_patient_ids(patient_hla)
  m_ids <- qa_ids[qa_ids %in% ph_ids]
  if (length(unique(qa_ids)) != length(unique(m_ids))){
    warning("Not all patients in query_alignment have hla genotypes specified.
            They will not be analyzed") }
  return(m_ids)
}

#' Lists all the hla genotypes that must be investigated
#' @param query_alignment The query alignment
#' @param patient_hla The data.frame (of class Patient_HLA) that contain lists
#' all the HLA genotypes each patient has.
#' @export

list_hlas <- function(query_alignment, patient_hla){
  
  # These checks can be done better using OO features?
  stopifnot(class(query_alignment) == 'AAMultipleAlignment')
  stopifnot(class(patient_hla) == 'Patient_HLA')

  m_ids <- get_matchable_patient_ids(query_alignment, patient_hla)
  return(patient_hla[patient_hla$patient_id %in% m_ids, 'hla_genotype'])
}

#' Lists all the hla genotypes that must be investigated
#' @param query_alignment The query alignment
#' @param patient_hla The data.frame (of class Patient_HLA) that contain lists
#' all the HLA genotypes each patient has.
#' @export

list_epitopes <- function(query_alignment, patient_hla, lanl_hla_data){
  
  # These checks can be done better using OO features?
  stopifnot(class(query_alignment) == 'AAMultipleAlignment')
  stopifnot(class(patient_hla) == 'Patient_HLA')
  stopifnot(class(lanl_hla_data) == 'LANL_HLA_data')

  hlas <- list_hlas(query_alignment, patient_hla)
    col_names <- c("epitope", "gene_name", "start_pos", "end_pos", 
                   "sub_type", "organism", "hla_genotype")

  return(lanl_hla_data[lanl_hla_data$hla_genotype %in% hlas, 
                       c('epitope', 'start_pos', 'end_pos')])
}

#' Finds the position of the epitope in the reference sequence
#'
#' It uses pairwiseAlignment with the default settings. See Biostrings manual
#'
#' Lots of things to test and investigate that can improve this function.
#' @param epitope The epitope to find in the sequence. Either a character
#' string or an AAString
#' @param query_alignment The query alignment
#' @export

epitope_pos_in_ref <- function(epitope, query_alignment){
  if (class(epitope) == 'character'){
    epitope <- AAString(epitope)
  }

  # There must be a better way to do this - couldn't find it in Biostrings ref
  # man
  ref_seq <- as.character(query_alignment)[1]
  ref_seq <- AAString(ref_seq)

  alignment <- pairwiseAlignment(pattern = epitope, subject = ref_seq, type = 'overlap')
  start_pos <- start(Views(alignment))
  end_pos <- end(Views(alignment))
  aln_score <- score(alignment)
  return(list(start_pos = start_pos,
              end_pos = end_pos, 
              alignment = alignment))
}

#' Internal function used to compute the comparison between the epitope
#' and the patients sequences
#' Under development - The design for this portion of the code is not yet
#' finalized
#' @export

.sequence_comparison_stats <- function(sequence_substr, pair_alignments, start_pos, end_pos){
  results <- NULL
  for (i in 1:length(pair_alignments)){
    x <- data.frame(name = names(sequence_substr)[i],
                    score = score(pair_alignments[i]),
                    score_type = type(pair_alignments[i]),
                    epitope = as.character(subject(pair_alignments[i])),
                    sub_sequence = as.character(pattern(pair_alignments[i])),
                    comparison = compareStrings(pair_alignments[i]),
                    pid = pid(pair_alignments[i]),
                    nmatch = nmatch(pair_alignments[i]),
                    nmismatch = nmismatch(pair_alignments[i]),
                    leven.dist = nedit(pair_alignments[i]),
                    start_pos = start_pos,
                    end_pos = end_pos,
                    stringsAsFactors = FALSE)
    results <- rbind(results, x)
  }
  return(results)
}

#' A function that indicates if an alignment was successful
#' @param alignment The alignment to check - usually the output from
#' pairwiseAlignment
#' @export

alignment_successful <- function(alignment){
  if (as.character(pattern(alignment)) == "" | as.character(subject(alignment)) == ""){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Computes the similarities between the epitope and the sequences in the
#' alignment
#' @param epitope The epitope to find in the sequence. Either a character
#' string or an AAString
#' @param query_alignment The query alignment
#' @export

compute_epitope_scores <- function(epitope, query_alignment){
  if (class(epitope) == 'character'){
    epitope <- AAString(epitope)
  }
  ref_pos <- epitope_pos_in_ref(epitope, query_alignment)
  if (alignment_successful(ref_pos$alignment)){
    sequence_substr <- substr(query_alignment, ref_pos$start_pos, ref_pos$end_pos)
    sequence_substr <- lapply(sequence_substr, AAString)
    pair_alignments <- pairwiseAlignment(pattern = sequence_substr, subject = epitope)
    results <- .sequence_comparison_stats(sequence_substr, pair_alignments, ref_pos$start_pos, ref_pos$end_pos)
  } else {
    warning('Alignment Unsuccessful - skipping it')
    results <- NULL
  }
  return(results)
}

#' Scores how well the epitopes in a patient's virus' sequences will be recognized by 
#' the patient's HLA genotype.
#'
#' Call for testing:
#' score_sequence_epitopes(read_query_alignment(), read_patient_hla(),
#' read_lanl_hla())
#' @param query_alignment The query alignment
#' @param patient_hla The data.frame (of class Patient_HLA) that contain lists
#' all the HLA genotypes each patient has.
#' @param lanl_hla_data The data.frame (of class LANL_HLA_data) that contains
#' the descriptions of the different HLA genotypes
#' @export

score_sequence_epitopes <- function(query_alignment, patient_hla, lanl_hla_data, ncpu = 6){
  epitopes <- list_epitopes(query_alignment, patient_hla, lanl_hla_data)
  results <- NULL
  i <- 0
  for (epitope in epitopes$epitope){
    i <- i+1
    print(paste0(i, ' of ', length(epitopes$epitope)))
    alignment_score <- compute_epitope_scores(epitope, query_alignment)
    results <- rbind(results, alignment_score)
  }

  return(results)
}
