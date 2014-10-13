context("MSA")

test_that("read_query_alignment works", {
  x <- get_test_query_alignment()
  expect_that(x, is_a('AAStringSet'))
  expect_that(digest(x), equals("9beaf63ab683c0353b9dea853b13ef9b"))
})

test_that("get_patient_ids work", {
  x <- get_test_query_alignment()
  ids <- get_patient_ids(x)
  expect_that("hxb2" %in% ids, is_true())
  expect_that("hxb2 " %in% ids, is_false())
  expect_that("pat02" %in% ids, is_true())

  x <- AAStringSet(structure(c( "MG", "MG", "MG", "MG"), 
      .Names = c("17h00,hxb2 ,sdklfj", "45h34,pat01,scribbles", 
                 ",pat02", "hello, pat03")))
  ids <- get_patient_ids(x, ',', 2)
  expect_that("hxb2" %in% ids, is_true())
  expect_that("hxb2 " %in% ids, is_false())
  expect_that("pat02" %in% ids, is_true())

  ids <- get_patient_ids(x, sep = NULL)
  expect_that("17h00,hxb2 ,sdklfj" %in% ids, is_true())
})
