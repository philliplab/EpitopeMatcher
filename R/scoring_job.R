# The Scoring_Job class and the methods associated with it.

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
    required_details <- c("end_pos", "epitope", "gene_name", "hxb2_dna_position", 
                          "organism", "start_pos", "subprotein", "sub_type")
    details_supplied <- sort(names(object@hla_details))
    if (!all(details_supplied %in% required_details)){
      stop(paste("Incorrect hla details supplied", 
                 paste(details_supplied, sep = ', ', collapse = ', ')))
    }
  }
)

#' Returns the epitope associated with a scoring_job
#'
#' @param the_scoring_job The scoring job which epitope must be extracted
#' @rdname get_epitope-methods
#' @export get_epitope

setGeneric("get_epitope",
           function(the_scoring_job){
             standardGeneric("get_epitope")
           }
)

#' @rdname get_epitope-methods
#' @aliases get_epitope
setMethod("get_epitope", 
          c('Scoring_Job'),

function(the_scoring_job){
  return(the_scoring_job@hla_details$epitope)
}
  
)

#' Returns the hla details associated with a scoring_job
#'
#' @param the_scoring_job The scoring job whose details must be extracted
#' @rdname get_hla_details-methods
#' @export get_hla_details

setGeneric("get_hla_details",
           function(the_scoring_job){
             standardGeneric("get_hla_details")
           }
)

#' @rdname get_hla_details-methods
#' @aliases get_hla_details
setMethod("get_hla_details", 
          c('Scoring_Job'),

function(the_scoring_job){
  return(the_scoring_job@hla_details)
}
  
)
