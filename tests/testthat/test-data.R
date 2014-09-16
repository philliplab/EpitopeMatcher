context('Data Files')

test_that('the correct test data files are supplied', {
  x <- read.csv(file.path(find.package('EpitopeMatcher', .libPaths()), 
                          'test_data/patient_hla_file.csv'))
  expect_that(digest(x), equals("549bc9e44c1bb5412f493f57832a7ee5"))
})
