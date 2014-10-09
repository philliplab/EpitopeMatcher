# This file contains all the code that matches the input files to each other
# and generate a list of the scoring jobs that must be executed

#' A class that contains all the required information to run an epitope scoring
#' job.
#'
#' Three pieces of information is required for a scoring job to be valid:
#' \begin{itemize}
#'   \item{hla_genotype - The name of the hla_genotype to investigate}
#'   \item{query_sequence_names - A character vector of the names of the query
#'   sequences in which this hla_genotype must be looked for}
#'   \item{hla_details - A list of further details about this hla_genotype}
#' \end{itemize}
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

