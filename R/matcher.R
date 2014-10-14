#' @include patient_HLA.R
#' @include msa.R
#' @include lanl_hla_data.R
NULL

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
  x <- data.frame(sequence_id = sequence_substr_name,
                  score = score(pair_alignment),
                  score_type = type(pair_alignment),
                  eregion_in_refseq = as.character(pattern(pair_alignment)),
                  candidate_substr = candidate_substr,
                  matched_substr = as.character(subject(pair_alignment)),
                  comparison = compareStrings(pair_alignment),
                  pid = pid(pair_alignment),
                  simple_distance = 100 - pid(pair_alignment),
                  nmatch = nmatch(pair_alignment),
                  nmismatch = nmismatch(pair_alignment),
                  leven.dist = nedit(pair_alignment),
                  start_pos_in_ref = start_pos,
                  end_pos_in_ref = end_pos,
                  start_pos_in_candidate = start(Views(pair_alignment)),
                  end_pos_in_candidate = end(Views(pair_alignment)),
                  range_expansion = range_expansion,
                  stringsAsFactors = FALSE)
  return(x)
}

#' A function that indicates if an alignment was successful
#' @param epitope The epitope that was searched for in the reference
#' @param alignment The alignment to check - usually the output from
#' pairwiseAlignment
#' @export

alignment_successful <- function(epitope, alignment){
  pat <- as.character(pattern(alignment))
  subj <- as.character(subject(alignment))
  epitope <- as.character(epitope)
  print(paste(pat, subj, epitope, sep = ' - '))
  if (pat == "" | subj == "" | nchar(pat) != nchar(epitope)){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Computes the similarities between the epitope and the sequences in the
#' alignment
#'
#' @param epitope The epitope to find in the sequence. Either a character
#' string or an AAString
#' @param query_alignment The query alignment
#' @param range_expansion After the epitope is found in the reference
#' seqeuence, search in each of the query sequences for the same epitope, but
#' expand the range with this number of amino acids
#' @details The output from this function is a list with two data.frames. The first is
#' the results data.frame that contains these columns:
#' \itemize{
#'  \item{sequence_id - The sequence description from the FASTA file}
#'  \item{score - The similarity score produced by the alignment}
#'  \item{score_type - The type of similarity score as returned by pairwiseAlignment}
#'  \item{eregion_in_refseq - The region of the reference sequence that was attempted to be aligned to the query sequence as returned by pairwiseAlignment}
#'  \item{candidate_substr - The candidate substring that was obtained by expanding the coordinates found in the reference sequence by 5 AAs on each side (unless at the end or beginning of the sequence)}
#'  \item{matched_substr - The part of the candidate substring that was matched to the epitope as returned by pairwiseAlignment}
#'  \item{comparison - A comparison between the epitope and the query sequence indicating where there were mismatches}
#'  \item{pid - The percentage of amino acids that were identical (Percentage IDentity) between the epitope and query sequences}
#'  \item{simple_distance - 100 - PID}
#'  \item{nmatch - The number of matches in the alignment}
#'  \item{nmismatch - The number of mismatches in the alignment}
#'  \item{leven.dist - The Levenshtein distance (or edit distance) between the two sequences}
#'  \item{start_pos_in_ref - The starting position in the reference sequence of the matching substring that was found for the epitope}
#'  \item{end_pos_in_ref - The end position in the reference sequence of the matching substring that was found for the epitope}
#'  \item{start_pos_in_candidate - The starting position in the candidate subsequence of the query sequence that was obtained by expanding the range of the reference that matches the epitope by starting a number of amino acids earlier in the query sequence. The number of amino acids is controlled by the range_extention parameter.}
#'  \item{end_pos_in_candidate - The end position in the candidate subsequence of the query sequence that was obtained by expanding the range of the reference that matches the epitope by stopping a number of amino acids later in the query sequence. The number of amino acids is controlled by the range_extention parameter.}
#'  \item{range_expansion - The number of amino acids by which the range of the query sequence that is compared to the epitope is larger than then match found for the epitope in the reference sequence.}
#'  \item{These three column are usually added to the table by the score_sequence_epitopes function:
#'  \itemize{
#'  \item{epitope - The epitope from the lanl file that was searched for in the reference sequence}
#'   \item{hla_genotype - The name of the hla genotype the epitope is associated with}
#'   \item{lanl_start_pos - The start position of the epitope according to the lanl file}
#'   \item{lanl_end_pos - The end position of the epitope according to the lanl file}
#'   }
#'  }
#' }
#' The second element of the list is the error log data.frame that contains
#' these columns:
#' 
#' \itemize{
#'  \item{pattern - The epitope as aligned to the reference sequence when a less restrictive alignment algorithm is used than the one that failed when aligning to the reference sequence the first time}
#'  \item{subject - The portion of the reference sequence to which the epitope was aligned to when a less restrictive alignment algorithm is used than the one that failed when aligning to the reference sequence the first time}
#'  \item{global_alignment_start - The starting position in the reference sequence of the subsequence of the reference sequence that the epitope was aligned to when a less restrictive alignment algorithm is used than the one that failed when aligning to the reference sequence the first time}
#'  \item{global_alignment_end - The end position in the reference sequence of the subsequence of the reference sequence that the epitope was aligned to when a less restrictive alignment algorithm is used than the one that failed when aligning to the reference sequence the first time}
#'  \item{These three column are usually added to the table by the score_sequence_epitopes function:
#'  \itemize{
#'  \item{epitope - The epitope from the lanl file that was searched for in the reference sequence}
#'   \item{hla_genotype - The name of the hla genotype the epitope is associated with}
#'   \item{lanl_start_pos - The start position of the epitope according to the lanl file}
#'   \item{lanl_end_pos - The end position of the epitope according to the lanl file}
#'   }
#'  }
#' }
#' 
#' @export

compute_epitope_scores <- function(epitope, query_alignment, range_expansion = 0){
  if (class(epitope) == 'character'){
    epitope <- AAString(epitope)
  }
  ref_pos <- epitope_pos_in_ref(epitope, query_alignment)
  if (alignment_successful(epitope, ref_pos$alignment)){
    eregion_in_refseq <- subject(ref_pos$alignment)
    sequence_substr <- substr(query_alignment, ref_pos$start_pos - range_expansion, 
                              ref_pos$end_pos + range_expansion)
    sequence_substr <- lapply(sequence_substr, AAString)
    results <- NULL
    for (i in 1:length(sequence_substr)){
      pair_alignment <- pairwiseAlignment(subject = sequence_substr[[i]], 
                                          pattern = eregion_in_refseq, 
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
    error_log <- data.frame(pattern = as.character(pattern(alignment)),
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
#' @param query_alignment The query alignment
#' @param patient_hla The data.frame (of class Patient_HLA) that contain lists
#' all the HLA genotypes each patient has.
#' @param lanl_hla_data The data.frame (of class LANL_HLA_data) that contains
#' the descriptions of the different HLA genotypes
#' @param range_expansion After the epitope is found in the reference
#' seqeuence, search in each of the query sequences for the same epitope, but
#' expand the range with this number of amino acids
#' @export

score_sequence_epitopes <- function(query_alignment, patient_hla, lanl_hla_data,
                                    range_expansion = 5){
  if ((class(query_alignment) != 'AAStringSet') |
    (class(patient_hla) != 'Patient_HLA') |
    (class(lanl_hla_data) != 'LANL_HLA_data')){
    return(list(msg = 'Input Files Invalid',
                results = data.frame(note = 'Input Files Invalid'),
                error_log = data.frame(note = 'Input Files Invalid')))
  }
  x <- list_scores_to_compute(query_alignment, patient_hla, lanl_hla_data)
  the_scoring_jobs <- x$result
  error_log <- x$error_log
  epitopes <- unlist(lapply(the_scoring_jobs, get_epitope))
  results <- NULL
  epitopes_not_in_seq <- NULL
  for (i in seq_along(the_scoring_jobs)){
    epitope <- get_epitope(the_scoring_jobs[[i]])
    print(paste0(i, ' of ', length(epitopes), ': ', epitope))
    hla_details <- as.data.frame(get_hla_details(the_scoring_jobs[[i]]),
                                 stringsAsFactors = FALSE)
    
    epitope_match <- compute_epitope_scores(epitope, query_alignment, 
                                            range_expansion)
    if (!is.null(epitope_match$error_log)){
      epitopes_not_in_seq <- rbind(epitopes_not_in_seq, 
                                   cbind(epitope_match$error_log, hla_details))
    }
    alignment_score <- epitope_match$results
    if (!is.null(alignment_score)){
      alignment_score <- cbind(alignment_score, hla_details)
      results <- rbind(results, alignment_score)
    }
  }
  error_log$epitopes_not_in_seq <- epitopes_not_in_seq

  return(list(results = results,
              error_log = error_log,
              msg = 'Scores computed successfully'))
}
