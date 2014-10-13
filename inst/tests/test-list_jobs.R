context('List jobs')

test_that('Scoring_Job constructor works',{
          # A simple test to check that if given the proper arguments 
          # for constructor, then the object is correctly construced. This is
          # worth testing because there are funny issues with the if in the
          # validity function and the cases when it is null.

  job_details <- list(hla_genotype = 'B67',
    query_sequence_names = 'CAP_0376',
    hla_details = list(epitope = "MGARASVLSGGELD", gene_name = "Gag", 
                       start_pos = 1L, end_pos = 14L, subprotein = "p17(5-18)",
                       hxb2_dna_position = "802..843", sub_type = "B", 
                       organism = "human"))
  y <- do.call(.Scoring_Job, job_details)

  expect_that(y, is_a('Scoring_Job'))
})

test_that('match_patient_hla_to_query_alignment works', {
  patient_hla <- get_test_patient_hla_data()
  query_alignment <- get_test_query_alignment()
  x <- match_patient_hla_to_query_alignment(query_alignment, patient_hla)
  expect_that(x[['A*3002']], equals("pat01|scribbles"))
  expect_that(x[['B63']], equals(c("pat01|scribbles", 
                                   "pat02|human|protein piece|>@booo \"\" -/.,!@#@#%^&*()",
                                   "pat03")))
})

test_that('flatten_lanl_hla works', {
  patient_hla <- get_test_patient_hla_data()
  lanl_hla <- get_test_lanl_hla_data()

  flat_lanl_hla <- flatten_lanl_hla(lanl_hla)
  expect_that("MGARASVLSGGELD" %in% flat_lanl_hla[,1], is_false())
  expect_that(sum("GELDRWEKI" == flat_lanl_hla[,1]), equals(2))
})

test_that('list_scoring_jobs works', {
          # I am not so sure about these tests. They require too much knowledge
          # of the Scoring_Job class?
  query_alignment <- get_test_query_alignment()
  patient_hla <- get_test_patient_hla_data()
  lanl_hla <- get_test_lanl_hla_data()
  the_scoring_jobs <- list_scores_to_compute(query_alignment, patient_hla, lanl_hla)
  expect_that(the_scoring_jobs[[1]], is_a('Scoring_Job'))
  expect_that(the_scoring_jobs[['B27']]@hla_details$epitope, equals('EKIRLRPGGKKYKL'))
  expect_that(the_scoring_jobs[['B27']]@query_sequence_names, 
              equals(c("pat01|scribbles", "pat03")))
})
