context("MSA")

test_that("read_query_alignment works", {
  x <- read_query_alignment(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/query_alignment.FASTA'))
  expect_that(x, is_a('AAMultipleAlignment'))
  expect_that(digest(x), equals("9012d510bcbdf2ec78b2ca4e2f154a73"))
})
