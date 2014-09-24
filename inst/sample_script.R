library(EpitopeMatcher)
file_name <- file.path(find.package('EpitopeMatcher', .libPaths()), 
                       'test_data/patient_hla_file.csv')
ph <- read_patient_hla(file_name )
file_name <- file.path(find.package('EpitopeMatcher', .libPaths()), 
                       'test_data/lanl_hla_file.csv')
ln <- read_lanl_hla(file_name )
file_name <- file.path(find.package('EpitopeMatcher', .libPaths()), 
                       'test_data/query_alignment.FASTA')
qa <- read_query_alignment(file_name )

query_alignment <- qa
patient_hla <- ph
lanl_hla_data <- ln
x <- score_sequence_epitopes(qa, ph, ln)

write.csv(x$results, '/tmp/results.csv', row.names = FALSE)
write.csv(x$error_log, '/tmp/error_log.csv', row.names = FALSE)
