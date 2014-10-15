library(EpitopeMatcher)
library(devtools)
load_all('..')
file_name <- file.path('~/projects/EpitopeMatcher/patient_hla_file.csv')
ph <- read_patient_hla(file_name )
file_name <- file.path('~/projects/EpitopeMatcher/ctl_summary.csv')
ln <- read_lanl_hla(file_name )
file_name <- file.path('~/projects/EpitopeMatcher/query_alignment.FASTA')
qa <- read_query_alignment(file_name )

query_alignment <- qa
patient_hla <- ph
lanl_hla_data <- ln

y <- list_scores_to_compute(qa, ph, ln)

x <- match_epitopes(qa, ph, ln)

write.csv(x$results, '/tmp/results.csv', row.names = FALSE)
write.csv(x$error_log, '/tmp/error_log.csv', row.names = FALSE)
