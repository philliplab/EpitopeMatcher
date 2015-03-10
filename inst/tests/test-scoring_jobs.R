context('Scoring Job class and methods')

test_that('Scoring_Job constructor and accessors work',{
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
  expect_that(get_epitope(y), equals("MGARASVLSGGELD"))
  expect_that(get_hla_genotype(y), equals("B67"))
})

