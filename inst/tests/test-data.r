context('Data Files')

test_that('the correct test data files are supplied', {
  x <- get_test_patient_hla_data()
  expect_that(digest(x), equals("58237040b39e2baa5dab6f5c87d1a6d8"))

  x <- get_test_lanl_hla_data()
  expect_that(digest(x), equals("c8ffd415e9e5d74f9e068a8907590d74"))

  x <- get_test_query_alignment()
  expect_that(digest(x), equals("9beaf63ab683c0353b9dea853b13ef9b"))
})
