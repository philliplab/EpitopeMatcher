context("Patient_HLA")

test_that("Patient_HLA Class Constructor works", {
  x <- data.frame(dude = 'CAP117', mutation = 'B*3501')
  expect_that(.Patient_HLA(x), throws_error("incorrent column names"))
  x <- data.frame(patient_id = 'CAP117', hla_genotype = 'B*3501')
  expect_that(.Patient_HLA(x[0,]), throws_error("Must specify at least one patient HLA genotype"))
  expect_that(.Patient_HLA(x), is_a('Patient_HLA'))
})

test_that("read_patient_hla works", {
  x <- read_patient_hla(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/patient_hla_file.csv'))
  expect_that(x, is_a('Patient_HLA'))
  expect_that(digest(x), equals("5b69bd79d227dc0c8268dd84c1f9463b"))
})

test_that("get_patient_id works", {
  x <- read_patient_hla(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/patient_hla_file.csv'))
  ids <- get_patient_ids(x)
  expect_that("P00885" %in% ids, is_true())
  expect_that("V05130" %in% ids, is_true())
  expect_that("A*4301" %in% ids, is_false())
})
