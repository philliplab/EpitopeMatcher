EpitopeMatcher
==============

A package that can be used to find out how well the epitopes in a patient's virus' will be
recognized by the HLA's present in the patient.

It can be install directly from github using devtools:
```r
install.packages('devtools')
library(devtools)
install_github('EpitopeMatcher', 'philliplab')
library(EpitopeMatcher)
```

To run the web UI:
```r
library(EpitopeMatcher)
run_EpitopeMatcher_app()
```

## Design Notes

### Outline showing execution order

```r
match_epitopes()
	list_scores_to_compute()
	score_all_epitopes()
	output_results()

list_scores_to_compute()
	matched_patients = match_patient_hla_to_query_alignment()
	build_scoring_jobs(matched_patients, lanl_hla_data)

build_scoring_jobs(matched_patients, lanl_hla_data)
  jobs = NULL
  for (mp in matched_patients)
    hla_details = get_hla_details(mp$..., lanl_hla_data)
    jobs = c(jobs,
             .Scoring_Job(hla_genotype,
                          patients,
                          hla_details))

score_all_epitopes()
	for (job in …)
		score_epitope()

score_epitope()
	find_epitope_in_ref()
	if not found()
		log_epitope_not_found()
	if found()
		get_query_sequences()
		align_ref_epitope_to_query_seqs()
		log_epitope_found()
```

### List of Classes

#### Scoring_Job
* attributes
  - hla_genotype : character
  - vector of query_sequence_names : character
  - hla_details : list
* methods  
  - get_query_sequence_names() : vector of character
  - get_epitope() : AAString
  - get_hla_details() : data.frame

#### Epitope_Position

### Design Choices

1. The input data is named and used in this order:
  - query_alignment
  - patient_hla
  - lanl_hla
2. The way to refer to a query sequence is by it's full FASTA header. Not the
   patient_id extracted from it nor it's position (index) in the alignment.
