#' @include patient_HLA.R
NULL

#' Reads in the query alignment
#' @param file_name Name of the file
#' @export

read_query_alignment <- function(file_name){
  x <- readAAStringSet(file_name)
  return(x)
}

#' A function that returns a test query alignment
#' @param dataset_name The name of the test dataset to return
#' @export

get_test_query_alignment <- function(dataset_name = 'base'){
  data_sets <- list(
    'base' =  AAStringSet(structure(c(
        "MGARASVLSGGELDRWEKIRLRPGGKK-YKLKHIVWASRELERFAVNPPPPGLLETSEGCRQILGQLQPSLQTGSEELRSLYNTVAT", 
        "MGTRASVLSGIEADRWEKIRLRPGGKKKYKLKHIVWASRELERFAVNP---GLLETSEGCRQILGQLQPSLQTGSEELRSLYNTVAT", 
        "MGARASVLSGGEADRREKIRLRPGGKKKYKLKHIVWASRELERFAVNPPPPGLLETSEGCRQILGQLQPSLQTGSEELRSLYNTVAT", 
        "MGTRASVLSQGEADRREKQRLRPGGKKKYKLKHIVWASRELERFAVNPPPPGLLETSEGCRQILGQLQPSLQTGSEELRSLYNTVAT"), 
      .Names = c("hxb2 ", "pat01|scribbles", 
                 "pat02|human|protein piece|>@booo \"\" -/.,!@#@#%^&*()", "pat03")))
  )
  return(data_sets[[dataset_name]])
}

#' @rdname get_patient_ids-methods
#' @aliases get_patient_ids
setMethod("get_patient_ids", "AAStringSet",
          function(x, sep = '\\|', id_position = 1){
            split_names <- strsplit(names(x), sep)
            names_vector <- unlist(lapply(split_names, `[[`, id_position))
            names_vector <- gsub("^ +", "", names_vector)
            names_vector <- gsub(" +$", "", names_vector)
            return(names_vector)
          })
