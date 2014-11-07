#' The class for the data from LANL that describe the HLA genotype's epitopes
#'
#' This class is an extension of the data.frame class placing some extra
#' restrictions on the data format. This data is typically obtained from
#' http://www.hiv.lanl.gov/content/immunology/tables/tables.html.
#'
#' The data.frame has the following columns:
#' \itemize{
#'  \item{Epitope}
#'  \item{Protein}
#'  \item{HXB2.start}
#'  \item{HXB2.end}
#'  \item{Subprotein}
#'  \item{HXB2.DNA.Contig}
#'  \item{Subtype}
#'  \item{Species}
#'  \item{HLA}
#' }
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

#' A function that reads a HLA genotype specification file
#'
#' This function converts the file into an object of class LANL_HLA. See the
#' documentation of that function for more details:
#' \code{\link{.LANL_HLA_data}}
#'
#' @param file_name Name of the file
#' @export

read_lanl_hla <- function(file_name){
  lanl_hla <- read.csv(file_name,
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
    file_col_name <- tolower(names(lanl_hla)[column_renames[[i]]$col_num])
    rename_source <- tolower(column_renames[[i]]$source)
    if (file_col_name == rename_source){
      names(lanl_hla)[column_renames[[i]]$col_num] <- column_renames[[i]]$target
    }
  }

  lanl_hla$epitope <- gsub(" *", "", lanl_hla$epitope)
  lanl_hla$epitope <- gsub("\\?", "", lanl_hla$epitope)

  return(.LANL_HLA_data(lanl_hla))
}

#' A function that returns a test lanl hla genotype dataset
#' @param dataset_name The name of the test dataset to return
#' @export

get_test_lanl_hla_data <- function(dataset_name = 'base'){
  data_sets <- list(
'base' = new("LANL_HLA_data"
    , .Data = list(c("MGARASVLSGGELD", "GELDRWEKI", "ELDRWEKIRL", "EKIRLRPGGKKYKL", 
"FAVNPGLLETSEGC", "RLSYNTVATLY"), c("Gag", "Gag", "Gag", "Gag", 
"Gag", "Gag"), c(1L, 11L, 12L, 17L, 44L, 76L), c(14L, 19L, 21L, 
31L, 57L, 86L), c("p17(5-18)", "p17(11-19)", "p17(12-21)", "p17(17-31)", 
"p17(44-57)", "p17(76-86)"), c("802..843", "820..846", "823..852", 
"838..882", "900..939", "1015..1047"), c("B", "B", "B, C", "", 
"B", "C"), c("human", "human", "human", "human", "human", "human"
), c("", "B*4002, B40", "B63", "B27, B7", "X67", "A*3002"))
    , names = c("epitope", "gene_name", "start_pos", "end_pos", "subprotein", 
"hxb2_dna_position", "sub_type", "organism", "hla_genotype")
    , row.names = 1:6
    , .S3Class = "data.frame"
)
    )
  return(data_sets[[dataset_name]])
}
