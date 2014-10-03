context('Data Files')

test_that('the correct test data files are supplied', {
  x <- read.csv(file.path(find.package('EpitopeMatcher', .libPaths()), 
                          'test_data/patient_hla_test_file.csv'))
  expect_that(digest(x), equals("4059bf3fee7ff3c9a982c59f620b70f0"))

  x <- read.csv(file.path(find.package('EpitopeMatcher', .libPaths()), 
                          'test_data/lanl_hla_test_file.csv'))
  expect_that(digest(x), equals("58fea75008a554f4f95fb16309b79fb9"))

  x <- readAAMultipleAlignment(file.path(find.package('EpitopeMatcher', .libPaths()), 
                          'test_data/query_alignment_test_file.fasta'))
  expect_that(digest(x), equals("9c968dbb9a188795395b9cd66e87e27f"))

})
