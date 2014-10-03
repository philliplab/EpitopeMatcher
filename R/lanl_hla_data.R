#' The class for the data from LANL that describe the HLA genotype's epitopes
#' 
#' @rdname LANL_HLA_data
#' @aliases LANL_HLA_data-class
#' @exportClass LANL_HLA_data
#' @export .LANL_HLA_data

.LANL_HLA_data <- setClass(
  Class = 'LANL_HLA_data',
  representation = representation(),
  contains = 'data.frame',

  validity = function(object){
    col_names <- c("epitope", "gene_name", "start_pos", "end_pos", 
                   "subprotein", "hxb2_dna_position", "sub_type", "organism", "hla_genotype")
    for (i in 1:length(col_names)){
      if (names(object)[i] != col_names[i]){
        stop(paste0("incorrent column names. ", i, "th column must be ", col_names[i]))
      }
    }
    if (nrow(object) == 0){
      stop("Must describe at least HLA genotype")
    }
  }
)

#' A function that reads a patient HLA genotype specification file
#' @param file_name Name of the file
#' @export

read_lanl_hla <- function(file_name){
  x <- read.csv(file_name,
                stringsAsFactors = FALSE)
  column_renames <- list('1' = list(source = 'Epitope',
                                    target = 'epitope',
                                    col_num = 1),
                         '2' = list(source = 'Protein',
                                    target = 'gene_name',
                                    col_num = 2),
                         '3' = list(source = 'HXB2.start',
                                    target = 'start_pos',
                                    col_num = 3),
                         '4' = list(source = 'HXB2.end',
                                    target = 'end_pos',
                                    col_num = 4),
                         '5' = list(source = 'Subprotein',
                                    target = 'subprotein',
                                    col_num = 5),
                         '6' = list(source = 'HXB2.DNA.Contig',
                                    target = "hxb2_dna_position",
                                    col_num = 6),
                         '7' = list(source = 'Subtype',
                                    target = 'sub_type',
                                    col_num = 7),
                         '8' = list(source = 'Species',
                                    target = 'organism',
                                    col_num = 8),
                         '9' = list(source = 'HLA',
                                    target = 'hla_genotype',
                                    col_num = 9)
                                    )
  for (i in seq_along(column_renames)){
    if (names(x)[column_renames[[i]]$col_num] == column_renames[[i]]$source){
      names(x)[column_renames[[i]]$col_num] <- column_renames[[i]]$target
    }
  }
  return(.LANL_HLA_data(x))
}


