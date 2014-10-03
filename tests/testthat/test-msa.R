context("MSA")

test_that("read_query_alignment works", {
  x <- read_query_alignment(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/query_alignment_test_file.fasta'))
  expect_that(x, is_a('AAStringSet'))
  expect_that(digest(x), equals("9beaf63ab683c0353b9dea853b13ef9b"))
})

test_that("get_patient_ids work", {
  x <- read_query_alignment(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/query_alignment_test_file.fasta'))
  ids <- get_patient_ids(x)
  expect_that("hxb2" %in% ids, is_true())
  expect_that("hxb2 " %in% ids, is_false())
  expect_that("pat02" %in% ids, is_true())
})
