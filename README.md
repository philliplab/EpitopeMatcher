EpitopeMatcher
==============

A package that can be used to find out how well the epitopes in a patient's virus' will be
recognized by the HLA's present in the patient.

## Installation Instructions for Ubuntu

Make sure you have a recent version of R. Follow
the instructions in the following link to set up the correct repositiory for apt:
http://stackoverflow.com/questions/10476713/how-to-upgrade-r-in-ubuntu. 

Make sure that both r-base and r-base-dev is installed
```{sh}
sudo apt-get install r-base r-base-dev
```

Next, install devtools' depedancies with apt-get:
```{sh}
sudo apt-get install libssl-dev libxml2-dev libcurl4-gnutls-dev
```

Then, from within R, install devtools and the BioConductor dependencies:
```{r}
install.packages('devtools', repo = 'http://cran.rstudio.com/')
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("Biostrings")
```

Finally install the latest version of shiny and then EpitopeMatcher:
```r
library(devtools)
install_github('rstudio/shiny')
install_github('philliplab/EpitopeMatcher')
```

## Using EpitopeMatcher

To run the web UI:
```r
library(EpitopeMatcher)
run_EpitopeMatcher_app()
```

To get some test data:
```r
library(EpitopeMatcher)
get_set_of_test_data()
```
or download it from [Test Data](https://github.com/philliplab/EpitopeMatcher/raw/master/inst/test_data/EpitopeMatcher_test_data.zip)

The test data consists of 3 sample files:

- **test_patient_hla_genotypes.csv** as produced by get_test_patient_hla_data() which contains the details of the patient's HLA genotype.
- **test_lanl_hla_data.csv** as produced by get_test_lanl_hla_data() which contains the details of the hla genotypes (location in the genome, epitope etc).
- **test_query_alignment.fasta** as produced by get_test_query_alignment() which contains an alignment of sequences of the patient's quasispecies to HXB2.

To use EpitopeMatcher in an R session, see the help file of these functions:
* read_lanl_hla
* read_patient_hla
* read_query_alignment
* match_epitopes 

Docker not available right now:
Alternatively it can also be obtained using docker:
* https://registry.hub.docker.com/u/philliplab/epitopematcher/

## Design Notes

### Outline showing execution order

```r
match_epitopes()
	list_scores_to_compute()
	score_all_epitopes()
	output_results()

list_scores_to_compute()
	matched_patients = match_patient_hla_to_query_alignment()
  flat_lanl_hla = flatten_lanl_hla()
	build_scoring_jobs(matched_patients, matched_hlas)

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

### Design Choices

1. The input data is named and used in this order:
  - query_alignment
  - patient_hla
  - lanl_hla
2. The way to refer to a query sequence is by it's full FASTA header. Not the
   patient_id extracted from it nor it's position (index) in the alignment.
3. Error Logging. Probably not the best design, but it should be good enough.
   Let each function that should log errors return as output a list with
   elements: 'msg', 'result', and 'error_logs' where 'error_logs' is again a list
   each of whom's elements is a data.frame that logs a specific type of error.
   This design should allow the users to inspect the error logs in EXCEL quite
   comfortably. A better design might be to produce traditional logs using a
   standard logging library and then to process those logs at a later stage in
   easy to analyze formats, but in the short term this is more work.

