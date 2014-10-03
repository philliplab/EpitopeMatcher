context("LANL_HLA")

test_that("LANL_HLA_data Class Constructor works", {
  x <- data.frame(dude = 'CAP117', mutation = 'B*3501')
  expect_that(.LANL_HLA_data(x), throws_error("incorrent column names"))

  x <- structure(list(epitope = c("MGARASVLSG", "ASVLSGGEL", "ASILRGGKLDK"), 
                      gene_name = c("Gag_p17", "Gag_p17", "Gag_p17"), 
                      start_pos = c(1L, 5L, 5L), 
                      end_pos = c(10L, 13L, 15L), 
                      subprotein = c("","",""),
                      hxb2_dna_position = c("", "", ""),
                      sub_type = c("CRF01_AE", "B", "C"), 
                      organism = c("human", "human", "human"), 
                      hla_genotype = c("", "", "")), 
                 .Names = c("epitope", "gene_name", "start_pos", "end_pos", "subprotein", 
                            "hxb2_dna_position", "sub_type", "organism", "hla_genotype"), 
                 row.names = c(NA, 3L), 
                 class = "data.frame")

  expect_that(.LANL_HLA_data(x[0,]), throws_error("Must describe at least HLA genotype"))
  expect_that(.LANL_HLA_data(x), is_a('LANL_HLA_data'))
})

test_that("read_patient_hla works", {
#  x <- read_lanl_hla(file.path(find.package('EpitopeMatcher', .libPaths()), 
#                                  'test_data/lanl_hla_test_file.csv'))
  x <- get_test_lanl_hla_data()
  expect_that(x, is_a('LANL_HLA_data'))
  expect_that(digest(x), equals("c8ffd415e9e5d74f9e068a8907590d74"))
})
