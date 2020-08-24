context("MSA")

test_that("read_query_alignment works", {
  x <- get_test_query_alignment()
  expect_that(x, is_a('AAStringSet'))
  expect_that(digest(x), equals("49892fc52f9dc19bbbaf41227450ff20"))
})

test_that("get_patient_ids work", {
  x <- get_test_query_alignment()
  ids <- get_patient_ids(x)
  expect_true("hxb2" %in% ids)
  expect_false("hxb2 " %in% ids)
  expect_true("pat02" %in% ids)

  x <- AAStringSet(structure(c( "MG", "MG", "MG", "MG"), 
      .Names = c("17h00,hxb2 ,sdklfj", 
                 "45h34,pat01,scribbles", 
                 ",pat02", 
                 "hello, pat03")))
  ids <- get_patient_ids(x, ',', 2)
  expect_true("hxb2" %in% ids)
  expect_false("hxb2 " %in% ids)
  expect_true("pat02" %in% ids)
  expect_false("hello" %in% ids)

  ids <- get_patient_ids(x, sep = NULL)
  expect_true("17h00,hxb2 ,sdklfj" %in% ids)
})
