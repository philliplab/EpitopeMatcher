context("MSA")

test_that("read_query_alignment works", {
  x <- read_query_alignment(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/query_alignment.FASTA'))
  expect_that(x, is_a('AAMultipleAlignment'))
  expect_that(digest(x), equals("b6a4178dc682837e8cbd656a4395a8b2"))
})

test_that("get_patient_ids work", {
  x <- read_query_alignment(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/query_alignment.FASTA'))
  ids <- get_patient_ids(x)
  expect_that("P00885" %in% ids, is_true())
  expect_that("V05130" %in% ids, is_true())
  expect_that("A*4301" %in% ids, is_false())
})
