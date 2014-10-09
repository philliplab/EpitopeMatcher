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
	match_patient_hla_to_query_alignment()
	add_hla_information()

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

### List of Data Structures

#### ScoringJob
* attributes
  - hla_genotype
  - vector of query_sequence_names
* methods  
  - get_query_sequence_names
  - get_epitope
  - get_hla_details

#### EpitopePosition
