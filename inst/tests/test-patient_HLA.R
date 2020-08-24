context("Patient_HLA")

test_that("Patient_HLA Class Constructor works", {
  x <- data.frame(dude = 'CAP117', mutation = 'B*3501')
  expect_that(.Patient_HLA(x), throws_error("incorrent column names"))
  x <- data.frame(patient_id = 'CAP117', hla_genotype = 'B*3501')
  expect_that(.Patient_HLA(x[0,]), throws_error("Must specify at least one patient HLA genotype"))
  expect_that(.Patient_HLA(x), is_a('Patient_HLA'))
})

test_that("read_patient_hla works", {
  x <- get_test_patient_hla_data()
  expect_that(x, is_a('Patient_HLA'))
  expect_that(digest(x), equals("58237040b39e2baa5dab6f5c87d1a6d8"))
})

test_that("get_patient_ids works", {
  x <- get_test_patient_hla_data()
  ids <- get_patient_ids(x)
  expect_true("pat01" %in% ids)
  expect_true("pat0[13]" %in% ids)
  expect_false("B27" %in% ids)
})
