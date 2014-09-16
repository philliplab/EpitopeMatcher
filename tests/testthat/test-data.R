context('Data Files')

test_that('the correct test data files are supplied', {
  x <- read.csv(file.path(find.package('EpitopeMatcher', .libPaths()), 
                          'test_data/patient_hla_file.csv'))
  expect_that(digest(x), equals("549bc9e44c1bb5412f493f57832a7ee5"))

  x <- read.csv(file.path(find.package('EpitopeMatcher', .libPaths()), 
                          'test_data/lanl_hla_file.csv'))
  expect_that(digest(x), equals("467f8f2140afdd3cbc8043b28d6e2245"))

  x <- readAAMultipleAlignment(file.path(find.package('EpitopeMatcher', .libPaths()), 
                          'test_data/query_alignment.FASTA'))
  expect_that(digest(x), equals("9012d510bcbdf2ec78b2ca4e2f154a73"))

})
