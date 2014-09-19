context("MSA")

test_that("read_query_alignment works", {
  x <- read_query_alignment(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/query_alignment.FASTA'))
  expect_that(x, is_a('AAStringSet'))
  expect_that(digest(x), equals("b2a9316b11da1c07d56e5846e88ce49c"))
})

test_that("get_patient_ids work", {
  x <- read_query_alignment(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/query_alignment.FASTA'))
  ids <- get_patient_ids(x)
  expect_that("P00885" %in% ids, is_true())
  expect_that("V05130" %in% ids, is_true())
  expect_that("A*4301" %in% ids, is_false())
})
