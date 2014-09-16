context("Patient_HLA")

test_that("Patient_HLA Class Constructor works", {
  x <- data.frame(dude = 'CAP117', mutation = 'B*3501')
  expect_that(.Patient_HLA(data = x), throws_error("incorrent column names"))
  x <- data.frame(patient_id = 'CAP117', HLA_Genotype = 'B*3501')
  expect_that(.Patient_HLA(data = x[0,]), throws_error("Must specify at least one patient HLA genotype"))
  expect_that(.Patient_HLA(data = x), is_a('Patient_HLA'))
})

test_that("read_patient_hla works", {
  x <- read_patient_hla(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/patient_hla_file.csv'))
  expect_that(x, is_a('Patient_HLA'))
  expect_that(digest(x), equals("b713f0e9f4e5b4f47b07dc324d7f7129"))
})
