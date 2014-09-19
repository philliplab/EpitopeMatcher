library(EpitopeMatcher)
ph <- read_patient_hla()
ln <- read_lanl_hla()
qa <- read_query_alignment()

query_alignment <- qa
patient_hla <- ph
lanl_hla_data <- ln

x <- score_sequence_epitopes(qa, ph, ln)
#epitope <- AAString(ln[1,1])
#
#pairwiseAlignment(pattern = epitope, subject = qa, type = 'overlap')
#
#pairwiseAlignment(pattern = epitope, subject = x[[1]], type = 'overlap')

