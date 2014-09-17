context('Matcher')

test_that('get_matchable_patient_ids works', {
  qa <- read_query_alignment(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/query_alignment.FASTA'))
  ph <- read_patient_hla(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/patient_hla_file.csv'))
  m_ids <- get_matchable_patient_ids(qa, ph)
  expect_that(sort(m_ids), equals(c("P00885", "P01288", "P02869", "P03257", "V05130", "V05649", 
    "V11204", "V11474")))
})

test_that('list_hlas works', {
  input_hlas <- c("A*0123", "A*0201", "A*0205", "A*0301", "A*2401", "A*2901", 
                  "A*3001", "A*3301", "A*3402", "A*4301", "A*6601", "A*6601", "A*6601", 
                  "A*6801", "B*1503", "B*1503", "B*1510", "B*1516", "B*1801", "B*3910", 
                  "B*4201", "B*4201", "B*4403", "B*4403", "B*4403", "B*4501", "B*5101", 
                  "B*5702", "B*5802", "Cw*0210", "Cw*0304", "Cw*0401", "Cw*0401", 
                  "Cw*0602", "Cw*0701", "Cw*0704", "Cw*0704", "Cw*1203", "Cw*1601", 
                  "Cw*1601", "Cw*1601", "Cw*1701", "Cw*1701", "Cw*1801")
  qa <- read_query_alignment(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/query_alignment.FASTA'))
  ph <- read_patient_hla(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/patient_hla_file.csv'))
  hlas_to_target <- list_hlas(qa, ph)
  expect_that(sort(hlas_to_target), equals(input_hlas))
})

test_that('list_epitopes works', {
  input_epitopes <- c("AEQATQDVKNW", "AETFYVDGA", "AETGQETAYY", "ALVEICTEL", "EEGVGFPVRPQ", 
                      "EEHEKYHSNW", "EELREHLLKW", "EILKEPVGHV", "EIVASCDKCQL", "FYKTLRAEQ", 
                      "GAFDLSFFL", "IEEKAFSPEVI", "ILKLAGRWPVK", "IQQEFGIPYNPQ", "IYQYMDDLYV", 
                      "KIIKDYGKQM", "KLGKAGYVV", "KQEFGIPY", "LPQGWKGSPAI", "MTSNPPIPV", 
                      "NANPDCKTILRAL", "NNETPGIRY", "NNETPGVRY", "NPDCKTILRAL", "PLTFGWCYKLV", 
                      "QEEHEKYHSNW", "QGWKGSPAI", "QVRDQAEHL", "RKAKIIKDY", "RVYLSWVPAHK", 
                      "SEVNIVTDSQY", "SLYNTVAAL", "TELQAIQLAL", "THLEGKVIL", "TKIQNFRVYY", 
                      "TLNAWVKLV", "TLNAWVKW", "TLRAEQATQD", "VGFPVRPQV", "VHQAISPRTL", 
                      "VKVVEEKAF", "VLMWQFDSRL", "WKFDSRLAF", "YPGIKVKQL", "YVDRFFKTL")
  qa <- read_query_alignment(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/query_alignment.FASTA'))
  ph <- read_patient_hla(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/patient_hla_file.csv'))
  ln <- read_lanl_hla(file.path(find.package('EpitopeMatcher', .libPaths()), 
                                  'test_data/lanl_hla_file.csv'))
  epitopes_to_target <- list_epitopes(qa, ph, ln)
  expect_that(sort(epitopes_to_target$epitope), equals(input_epitopes))
})
