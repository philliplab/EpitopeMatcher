context('List jobs')

test_that('match_patient_hla_to_query_alignment works', {
  patient_hla <- get_test_patient_hla_data()
  query_alignment <- get_test_query_alignment()
  x <- match_patient_hla_to_query_alignment(query_alignment, patient_hla)
  expect_that(x[['1']], is_a('list'))
  expect_that(names(x[['2']]), equals(c("hla_genotype",
                                        "query_sequence_names")))
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
  expect_that(the_scoring_jobs[['result']][[1]], is_a('Scoring_Job'))
  expect_that(the_scoring_jobs[['result']][[1]]@hla_details$epitope, equals('RLSYNTVATLY'))
})
