context('Data Files')

test_that('the correct test data files are supplied', {
  x <- read.csv(file.path(find.package('EpitopeMatcher', .libPaths()), 
                          'test_data/patient_hla_file.csv'))
  expect_that(digest(x), equals("eedd72a0f9fdcda10b7c2a1f4b558b7b"))
})
