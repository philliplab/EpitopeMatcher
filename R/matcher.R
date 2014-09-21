#' Gets all the patient ids that are in both the MSA and the patient_hla file
#' @param query_alignment The query alignment
#' @param patient_hla The data.frame (of class Patient_HLA) that contain lists
#' all the HLA genotypes each patient has.
#' @export

get_matchable_patient_ids <- function(query_alignment, patient_hla){

  # These checks can be done better using OO features?
  stopifnot(class(query_alignment) == 'AAStringSet')
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
  stopifnot(class(query_alignment) == 'AAStringSet')
  stopifnot(class(patient_hla) == 'Patient_HLA')

  m_ids <- get_matchable_patient_ids(query_alignment, patient_hla)
  hla_genotypes <- patient_hla[patient_hla$patient_id %in% m_ids, 'hla_genotype']
  return(hla_genotypes)
}

#' Lists all the hla genotypes that must be investigated
#' @param query_alignment The query alignment
#' @param patient_hla The data.frame (of class Patient_HLA) that contain lists
#' all the HLA genotypes each patient has.
#' @export

list_epitopes <- function(query_alignment, patient_hla, lanl_hla_data){
  
  # These checks can be done better using OO features?
  stopifnot(class(query_alignment) == 'AAStringSet')
  stopifnot(class(patient_hla) == 'Patient_HLA')
  stopifnot(class(lanl_hla_data) == 'LANL_HLA_data')

  hlas <- list_hlas(query_alignment, patient_hla)

  return(lanl_hla_data[lanl_hla_data$hla_genotype %in% hlas, 
                       c('epitope', 'start_pos', 'end_pos', 'hla_genotype')])
}

#' Finds the position of the epitope in the reference sequence
#'
#' It uses pairwiseAlignment with the default settings. See Biostrings manual
#'
#' Lots of things to test and investigate that can improve this function.
#' @param epitope The epitope to find in the sequence. Either a character
#' string or an AAString
#' @param query_alignment The query alignment
#' @param alignment_type The type of alignment to try. Defaults to 'overlap'
#' use 'global' if 'overlap' alignment cannot be found
#' @export

epitope_pos_in_ref <- function(epitope, query_alignment, alignment_type = 'overlap'){
  if (class(epitope) == 'character'){
    epitope <- AAString(epitope)
  }

  ref_seq <- query_alignment[[1]]

  alignment <- pairwiseAlignment(pattern = epitope, subject = ref_seq, type = alignment_type)
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

.sequence_comparison_stats <- function(sequence_substr_name, pair_alignment, start_pos, 
                                       end_pos, range_expansion, candidate_substr){
  x <- data.frame(name = sequence_substr_name,
                  score = score(pair_alignment),
                  score_type = type(pair_alignment),
                  epitope = as.character(pattern(pair_alignment)),
                  candidate_substr = candidate_substr,
                  matched_substr = as.character(subject(pair_alignment)),
                  comparison = compareStrings(pair_alignment),
                  pid = pid(pair_alignment),
                  nmatch = nmatch(pair_alignment),
                  nmismatch = nmismatch(pair_alignment),
                  leven.dist = nedit(pair_alignment),
                  epitope_search_start_pos = start_pos,
                  epitope_search_end_pos = end_pos,
                  epitope_match_start_pos = start(Views(pair_alignment)),
                  epitope_match_end_pos = end(Views(pair_alignment)),
                  range_expansion = range_expansion,
                  stringsAsFactors = FALSE)
  return(x)
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
#' @param range_expansion After the epitope is found in the reference
#' seqeuence, search in each of the query sequences for the same epitope, but
#' expand the range with this number of amino acids
#' @export

compute_epitope_scores <- function(epitope, query_alignment, range_expansion = 0){
  if (class(epitope) == 'character'){
    epitope <- AAString(epitope)
  }
  ref_pos <- epitope_pos_in_ref(epitope, query_alignment)
  if (alignment_successful(ref_pos$alignment)){
#    sequence_substr <- substr(query_alignment, ref_pos$start_pos, ref_pos$end_pos)
    sequence_substr <- substr(query_alignment, ref_pos$start_pos - range_expansion, 
                              ref_pos$end_pos + range_expansion)
    sequence_substr <- lapply(sequence_substr, AAString)
    results <- NULL
    for (i in 1:length(sequence_substr)){
      pair_alignment <- pairwiseAlignment(subject = sequence_substr[[i]], pattern = epitope, 
                                           type = 'overlap')
      results <- rbind(results,
                       .sequence_comparison_stats(names(sequence_substr)[i], pair_alignment, 
                                                  ref_pos$start_pos, ref_pos$end_pos,
                                                  range_expansion, 
                                                  as.character(sequence_substr[[i]])))
      error_log <- NULL
    }
  } else {
    global_alignment <- epitope_pos_in_ref(epitope, query_alignment, alignment_type = 'global')
    alignment <- global_alignment$alignment
    results <- NULL
    error_log <- data.frame(epitope = as.character(epitope),
                            pattern = as.character(pattern(alignment)),
                            subject = as.character(subject(alignment)),
                            global_alignment_start = global_alignment$start_pos,
                            global_alignment_end = global_alignment$end_pos)
  }
  return(list(results = results,
              error_log = error_log))
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

score_sequence_epitopes <- function(query_alignment, patient_hla, lanl_hla_data,
                                    range_expansion = 5){
  epitopes <- list_epitopes(query_alignment, patient_hla, lanl_hla_data)
  results <- NULL
  error_log <- NULL
  for (i in 1:nrow(epitopes)){
    epitope <- epitopes$epitope[i]
    print(paste0(i, ' of ', length(epitopes$epitope), epitope))
    epitope_info <- data.frame(hla_genotype = epitopes$hla_genotype[i],
                               lanl_start_pos = epitopes$start_pos[i],
                               lanl_end_pos = epitopes$end_pos[i],
                               stringsAsFactors = FALSE)
    
    epitope_match <- compute_epitope_scores(epitope, query_alignment, 
                                            range_expansion)
    if (!is.null(epitope_match$error_log)){
      error_log <- rbind(error_log, cbind(epitope_match$error_log, epitope_info))
    }
    alignment_score <- epitope_match$results
    if (!is.null(alignment_score)){
      alignment_score <- cbind(alignment_score, epitope_info)
      results <- rbind(results, alignment_score)
    }
  }

  return(list(results = results,
              error_log = error_log))
}
