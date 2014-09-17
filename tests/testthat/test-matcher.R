context('Matcher')

test_that('get_matchable_patient_ids works', {
  qa <- read_query_alignment(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/query_alignment.FASTA'))
  ph <- read_patient_hla(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/patient_hla_file.csv'))
  m_ids <- get_matchable_patient_ids(qa, ph)
  expect_that(sort(m_ids), equals(c("P00885", "P01288", "P02869", "P03257", "V05130", "V05649", 
    "V11204", "V11474")))
})
