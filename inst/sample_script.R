library(EpitopeMatcher)
ph <- read_patient_hla()
ln <- read_lanl_hla()
qa <- read_query_alignment()

query_alignment <- qa
patient_hla <- ph
lanl_hla_data <- ln
x <- score_sequence_epitopes(qa, ph, ln)

write.csv(x$results, '/tmp/results.csv', row.names = FALSE)
write.csv(x$error_log, '/tmp/error_log.csv', row.names = FALSE)
